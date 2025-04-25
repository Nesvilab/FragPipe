/*
 * This file is part of FragPipe.
 *
 * FragPipe is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * FragPipe is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with FragPipe. If not, see <https://www.gnu.org/licenses/>.
 */

package org.nesvilab.fragpipe.tabs;

import static org.nesvilab.fragpipe.FragpipeRun.createVersionsString;
import static org.nesvilab.fragpipe.FragpipeRun.printProcessDescription;
import static org.nesvilab.fragpipe.messages.MessagePrintToConsole.toConsole;
import static org.nesvilab.fragpipe.tabs.TabMsfragger.setJTableColSize;
import static org.nesvilab.fragpipe.tabs.TabRun.saveLogToFile;
import static org.nesvilab.utils.PropertiesUtils.saveConvert;

import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.nesvilab.fragpipe.Fragpipe;
import org.nesvilab.fragpipe.FragpipeLocations;
import org.nesvilab.fragpipe.FragpipeRun;
import org.nesvilab.fragpipe.api.*;
import org.nesvilab.fragpipe.cmd.*;
import org.nesvilab.fragpipe.messages.*;
import org.nesvilab.fragpipe.process.ProcessDescription;
import org.nesvilab.fragpipe.process.ProcessDescription.Builder;
import org.nesvilab.fragpipe.process.RunnableDescription;
import org.nesvilab.fragpipe.util.BatchRun;
import org.nesvilab.utils.OsUtils;
import org.nesvilab.utils.PathUtils;
import org.nesvilab.utils.SwingUtils;
import org.nesvilab.utils.swing.*;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.swing.*;
import javax.swing.border.TitledBorder;
import javax.swing.filechooser.FileFilter;
import javax.swing.filechooser.FileNameExtensionFilter;

import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.ThreadMode;
import org.nesvilab.utils.swing.renderers.TableCellDoubleRenderer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class TabBatch extends JPanelWithEnablement {

    private static final Logger log = LoggerFactory.getLogger(TabBatch.class);

    public static final MigUtils mu = MigUtils.get();
    public static final String TAB_PREFIX = "tab-batch.";
    final TextConsole console;
    Color defTextColor;
    private UiCheck uiCheckDryRun;
    public JButton btnRun;
    public JButton btnStop;
    private JProgressBar batchProgressBar;
    private List<BatchRun> batchRuns;

    private JPanel pBottom;
    private JPanel pConsole;
    private UiCheck uiCheckWordWrap;

    private BatchTable batchTable;
    private static final String[] TABLE_BATCH_COL_NAMES = {"Workflow File Path", "Manifest File Path", "Output Directory",
             "Tools Folder Path (optional)", "Fasta Path (optional)", "RAM (optional)", "Threads (optional)"};
    public static final String PROP_FILECHOOSER_LAST_PATH = "batch.filechooser.path";


    public TabBatch() {
        this.console = createConsole();
        init();
        initMore();
    }

    private void initMore() {
        Bus.registerQuietly(this);
        Bus.postSticky(this);
    }

    private void clearConsole() {
        console.setText("");
    }

    private int runBatch(MessageRunBatch m) {
        clearConsole();
        boolean runConfigurationDone = false;
        List<BatchRun> runs = batchTable.model.getRuns();
        batchRuns = runs;
        batchProgressBar.setMaximum(runs.size());
        batchProgressBar.setValue(0);
        batchProgressBar.setString(String.format("Completed %d of %d batch runs", batchProgressBar.getValue(), batchProgressBar.getMaximum()));
        batchProgressBar.setStringPainted(true);

        try {
            Bus.post(new MessageRunButtonEnabled(false));

            // prepare the processes
            List<ProcessBuildersDescriptor> pbDescsBuilderDescs = new ArrayList<>(1);

            for (BatchRun run : runs) {
                CmdBatch cmdBatch = new CmdBatch(true, run.outputPath);
                if (cmdBatch.configure(this, run)) {
                    ProcessBuildersDescriptor processBuildersDescriptor = cmdBatch.getBuilderDescriptor();
                    pbDescsBuilderDescs.add(processBuildersDescriptor);
                }
            }

            toConsole(OsUtils.OsInfo() + "\n" + OsUtils.JavaInfo() + "\n" + OsUtils.NetCoreInfo() + "\n", console);
            toConsole("", console);
            toConsole("Version info:\n" + createVersionsString(), console);
            toConsole("", console);

            final List<ProcessBuilderInfo> pbis = pbDescsBuilderDescs.stream().flatMap(pbd -> pbd.pbis.stream().map(pbi -> {
                PbiBuilder b = new PbiBuilder();
                b.setPb(pbi.pb);
                b.setName(pbi.name != null ? pbi.name : pbd.name);
                b.setFnStdOut(pbi.fnStdout != null ? pbi.fnStdout : pbd.fnStdout);
                b.setFnStdErr(pbi.fnStderr != null ? pbi.fnStderr : pbd.fnStderr);
                b.setParallelGroup(pbi.parallelGroup != null ? pbi.parallelGroup : pbd.getParallelGroup());
                return b.create();
            })).collect(Collectors.toList());

            toConsole(String.format(Locale.ROOT, "%d commands to execute:", pbis.size()), console);
            for (final ProcessBuilderInfo pbi : pbis) {
                printProcessDescription(pbi, console);
            }
            toConsole("~~~~~~~~~~~~~~~~~~~~~~", console);
            toConsole("", console);

            if (m.isDryRun) {
                toConsole(Fragpipe.COLOR_RED_DARKEST, "\nIt's a dry-run, not running the commands.\n", true, console);
                return 0;
            }

            // run everything
            long startTime = System.nanoTime();
            final List<RunnableDescription> toRun = new ArrayList<>();
            for (final ProcessBuilderInfo pbi : pbis) {
                // runnable for the batch run
                Runnable runnable = ProcessBuilderInfo.toRunnable(pbi, pbi.pb.directory().toPath(), FragpipeRun::printProcessDescription, console, false);
                ProcessDescription.Builder b = new ProcessDescription.Builder().setName(pbi.name);
                if (pbi.pb.directory() != null) {
                    b.setWorkDir(pbi.pb.directory().toString());
                }
                if (pbi.pb.command() != null && !pbi.pb.command().isEmpty()) {
                    b.setCommand(String.join(" ", pbi.pb.command()));
                }
                toRun.add(new RunnableDescription(b.create(), runnable, pbi.parallelGroup, pbi));

                // add finalizer process for each run to update the batch progress
                final Runnable finalizerRun = () -> {
                    Bus.post(new MessageUpdateBatchProgress());
                };
                toRun.add(new RunnableDescription(new Builder().setName("Finalizer Task").create(), finalizerRun));
            }

            // add finalizer process for the end of all batches
            final Runnable finalizerRun = () -> {
                String totalTime = String.format("%.1f", (System.nanoTime() - startTime) * 1e-9 / 60);
                toConsole(Fragpipe.COLOR_RED_DARKEST, "\n++++++++++++++++++++++++++++++++++++++++++++++++++++++++ALL BATCH JOBS DONE IN " + totalTime + " MINUTES++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++", true, console);
                Bus.post(MessageSaveLog.saveInDir(FragpipeLocations.get().getDirJobs(), console));   // save final log to jobs dir as a backup for the individual run logs
            };
            toRun.add(new RunnableDescription(new Builder().setName("Finalizer Task").create(), finalizerRun));

            Bus.post(new MessageStartProcesses(toRun));

            runConfigurationDone = true;
        } catch (Exception ex) {
            toConsole(Fragpipe.COLOR_RED_DARKEST, ex.getMessage(), true, console);
            return 1;
        } finally {
            if (!runConfigurationDone) {
                Bus.post(new MessageRunButtonEnabled(true));
            }
        }

        return 0;
    }

    private JPanel createPanelBottom(TextConsole console) {
        uiCheckDryRun = UiUtils.createUiCheck("Dry Run", false);
        btnRun = UiUtils.createButton("Run All", e -> Bus.post(new MessageRunBatch(isDryRun(), null)));

        JButton btnClearConsole = UiUtils.createButton("Clear Console", e -> clearConsole());
        uiCheckWordWrap = UiUtils.createUiCheck("Word wrap", true, e -> {
            console.setScrollableTracksViewportWidth(uiCheckWordWrap.isSelected());
            console.setVisible(false);
            console.setVisible(true);
        });

        console.setScrollableTracksViewportWidth(true);
        batchProgressBar = new JProgressBar(0, 100);

        btnStop = UiUtils.createButton("Stop All", e -> {
            Bus.post(new MessageKillAll(MessageKillAll.REASON.USER_ACTION, console));
            Path currentWorkDir = batchRuns.get(batchProgressBar.getValue()).outputPath;
            if (currentWorkDir != null) {
                Bus.post(MessageSaveLog.saveInDir(currentWorkDir, console));
                FragpipeRun.saveRuntimeConfig(currentWorkDir);
            }
            batchProgressBar.setValue(0);
            batchProgressBar.setStringPainted(false);
        });

        JPanel p = mu.newPanel(null, true);
        mu.add(p, btnRun).split(6);
        mu.add(p, btnStop);
        mu.add(p, uiCheckDryRun);
        mu.add(p, btnClearConsole);
        mu.add(p, uiCheckWordWrap).wrap();
        mu.add(p, batchProgressBar).growX().spanX().wrap();

        return p;
    }

    @Subscribe(threadMode = ThreadMode.ASYNC)
    public void on(MessageRunBatch m) {
            int returnCode = runBatch(m);
        if (Fragpipe.headless && returnCode != 0) {
            System.exit(returnCode);
        }
    }

    @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
    public void on(MessageUpdateBatchProgress m) {
        batchProgressBar.setValue(batchProgressBar.getValue() + 1);
        batchProgressBar.setString(String.format("Completed %d of %d batch runs", batchProgressBar.getValue(), batchProgressBar.getMaximum()));
        batchProgressBar.setStringPainted(true);
    }

    @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
    public void on(MessageBatchCrashed m) {
        // make sure this was actually a batch run that crashed, not a regular run
        if (!(m.console == this.console)) {
            return;
        }
        // reset progress bar and print a message to console saying which batch run crashed
        toConsole(String.format("\n++++++++++Non-zero exit code in batch run %d of %d. Stopping batch run.++++++++++", batchProgressBar.getValue() + 1, batchProgressBar.getMaximum()), console);
        batchProgressBar.setValue(0);
        batchProgressBar.setStringPainted(false);
    }

    public boolean isDryRun() {
        return SwingUtils.isEnabledAndChecked(uiCheckDryRun);
    }


    protected void init() {
        defTextColor = UIManager.getColor("TextField.foreground");
        if (defTextColor == null) {
            defTextColor = Color.BLACK;
        }
        batchRuns = new ArrayList<>();

        JPanel pBatch = new JPanel(new MigLayout(new LC()));
        pBatch.setBorder(new TitledBorder("Batch FragPipe Runs"));
        batchTable = createBatchTable();

        JScrollPane tableScrollBatch = new JScrollPane(batchTable, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        tableScrollBatch.setPreferredSize(new Dimension(1200, 200));

        JButton btnLoadJobs = new JButton("Load Job(s)");     // loads jobs from built-in jobs folder
        btnLoadJobs.addActionListener(e -> this.actionBtnLoadBatchTemplate(e, FragpipeLocations.get().getDirJobs().toString()));
        JButton btnLoadBatchTemplate = new JButton("Load Job Manifest");    // loads jobs from user's saved manifest file(s)
        btnLoadBatchTemplate.addActionListener(e -> this.actionBtnLoadBatchTemplate(e, Fragpipe.propsVarGet(PROP_FILECHOOSER_LAST_PATH)));
        JButton btnSaveBatchTemplate = new JButton("Save Job Manifest");
        btnSaveBatchTemplate.addActionListener(this::actionBtnSaveBatchTemplate);
        JButton btnOpenJobsFolder = new JButton("Open Jobs Folder in File Manager");
        btnOpenJobsFolder.addActionListener(e -> openJobsFolder());

        JButton btnRemoveSelected = new JButton("Remove Selected");
        btnRemoveSelected.addActionListener(e -> btnRemoveSelected());
        JButton btnClearTable = new JButton("Clear Table");
        btnClearTable.addActionListener(e -> clearTable());

        mu.add(pBatch, btnLoadJobs).split();
        mu.add(pBatch, btnLoadBatchTemplate).split();
        mu.add(pBatch, btnSaveBatchTemplate).split();
        mu.add(pBatch, btnOpenJobsFolder).split().wrap();
        mu.add(pBatch, btnRemoveSelected).split();
        mu.add(pBatch, btnClearTable).wrap();
        mu.add(pBatch, tableScrollBatch, new CC().minHeight("100px").maxHeight("200px").growX().spanX().wrap());

        pBottom = createPanelBottom(console);
        pBottom.setPreferredSize(new Dimension(400, 50));
        initConsole(console);
        pConsole = createPanelConsole(console);

        mu.layout(this).fillX();
        mu.add(this, pBatch).growX().alignY("top").wrap();
        mu.add(this, pBottom).growX().alignY("top").wrap();
        mu.add(this, pConsole).grow().push().alignY("top").wrap();
    }


    private TextConsole createConsole() {
        TextConsole c = new TextConsole(false);
        final Font currentFont = c.getFont();
        c.setFont(new Font(Font.MONOSPACED, currentFont.getStyle(), currentFont.getSize()));
        c.setContentType("text/plain; charset=UTF-8");
        return c;
    }


    private JPanel createPanelConsole(TextConsole tc) {
        JPanel p = mu.newPanel("Console", mu.lcNoInsetsTopBottom());

        JScrollPane scroll = SwingUtils.wrapInScroll(tc);
        scroll.setMinimumSize(new Dimension(400, 50));
        // the editor does not originally occupy the whole width of the viewport
        // so we mask it off with the same color as the console
        scroll.getViewport().setBackground(tc.getBackground());

        mu.add(p, scroll).grow().push().wrap();
        return p;
    }

    private void initConsole(TextConsole console) {
        final Font currentFont = console.getFont();

        console.setFont(new Font(Font.MONOSPACED, currentFont.getStyle(), currentFont.getSize()));
        console.setContentType("text/plain; charset=UTF-8");
    }

    private BatchTable createBatchTable() {
        BatchTableModel model = new BatchTableModel(TABLE_BATCH_COL_NAMES);
        BatchTable t = new BatchTable(model, TABLE_BATCH_COL_NAMES, TabBatch::convertBatchToTableData);

        Fragpipe.rename(t, "table.batch", TabBatch.TAB_PREFIX);
        t.setToolTipText("<html>Batch runs table. <br/>" +
                "Enter paths to workflow and manifest files and output directories for FragPipe runs.<br/>" +
                "Each row represents a complete FragPipe run.<br/>");
        t.setDefaultRenderer(Double.class, new TableCellDoubleRenderer());
        t.setFillsViewportHeight(true);

        return t;
    }

    public static Object[][] convertBatchToTableData(List<BatchRun> runs) {
        Object[][] data = new Object[runs.size()][TABLE_BATCH_COL_NAMES.length];
        for (int i = 0; i < runs.size(); i++) {
            BatchRun run = runs.get(i);
            data[i][0] = run.workflow;
            data[i][1] = run.manifest;
            data[i][2] = run.outputPath;
            data[i][3] = run.toolsPath;
            data[i][4] = run.fastaPath == null ? "" : run.fastaPath;
            data[i][5] = run.ram;
            data[i][6] = run.threads;
        }
        return data;
    }

    public static List<BatchRun> parseBatchTemplate(Component parent, String templatePath) {
        ArrayList<BatchRun> runs = new ArrayList<>();
        try {
            BufferedReader in = new BufferedReader(new FileReader(templatePath));
            String line;
            int lineIndex = 1;
            while ((line = in.readLine()) != null) {
                if (line.startsWith("#")) {
                    continue; // skip comment lines
                }
                String[] splits = line.split("\t");
                String workflowPath = splits[0].replace("\"", "");
                String manifestPath = splits[1].replace("\"", "");
                String outputDir = splits[2].replace("\"", "");
                String toolsFolderPath = splits.length > 3 ? splits[3].replace("\"", ""): "";
                String fastaPath = splits.length > 4 ? splits[4].replace("\"", ""): "";
                int ram = 0, threads = 0;
                try {
                    ram = splits.length > 5 ? Integer.parseInt(splits[5]) : 0;
                    threads = splits.length > 6 ? Integer.parseInt(splits[6]) : 0;
                } catch (NumberFormatException e) {
                    SwingUtils.showErrorDialog(parent, "Invalid number format for RAM or threads in batch template line " + lineIndex + ": " + e.getMessage() + "\nThe input value will be set to the default (0)", "Number Format Error");
                }
                BatchRun run = new BatchRun(workflowPath, manifestPath, outputDir, toolsFolderPath, fastaPath, ram, threads);
                if (checkPaths(parent, run, false)) {
                    runs.add(run);
                }
                lineIndex++;
            }
        } catch (IOException e) {
            log.error("Error reading batch template file: {}", e.getMessage());
            SwingUtils.showErrorDialogWithStacktrace(e, parent);
        }
        return runs;
    }

    public static boolean checkPaths(Component parent, BatchRun run, boolean makeDirs) {
        if (run.workflowPath == null) {
            SwingUtils.showErrorDialog(parent, String.format("Workflow file path not found: %s\n This batch run will be skipped.", run.workflow), "File Not Found");
            return false;
        }
        if (run.manifestPath == null) {
            SwingUtils.showErrorDialog(parent, String.format("Manifest file path not found: %s\n This batch run will be skipped.", run.manifest), "File Not Found");
            return false;
        }
        if (run.toolsPath == null || !(Files.exists(run.toolsPath) && Files.isDirectory(run.toolsPath))) {
            TabConfig tabConfig = Fragpipe.getStickyStrict(TabConfig.class);
            String defaultToolsStr = tabConfig.uiTextToolsFolder.getNonGhostText();
            Path defaultToolsPath = PathUtils.existing(defaultToolsStr);
            if (defaultToolsPath == null) {
                SwingUtils.showErrorDialog(parent, "Tools folder path not found: " + run.toolsPath +
                        "\n and the default tools folder path was not configured on the Config tab. This job could not be added and will be skipped.", "Tools folder Not Found");
                return false;
            } else {
                run.toolsPath = defaultToolsPath;
            }
        }
        if (!(Files.exists(run.outputPath) && Files.isDirectory(run.outputPath))) {
            if (makeDirs) {
                if (!run.outputPath.toFile().mkdirs()) {
                    SwingUtils.showErrorDialog(parent, "Could not create output directory: " + run.outputPath, "Directory Creation Failed");
                    return false;
                }
            }
        }
        // only check fasta path if provided
        if (run.fastaPath != null) {
            if (!(Files.exists(run.fastaPath))) {
                SwingUtils.showErrorDialog(parent, String.format("Fasta file path not found: %s\n This batch run will be skipped.", run.fastaPath), "File Not Found");
                return false;
            }
            // overwrite fasta path in workflow file if the fasta path was provided
            TabDatabase tabDatabase = Fragpipe.getStickyStrict(TabDatabase.class);
            try {
                tabDatabase.validateFastaForBatch(run.fastaPath.toAbsolutePath().normalize().toString());
            } catch (Exception e) {
                SwingUtils.showErrorDialog(parent, String.format("Fasta file %s could not be validated due to the following error:\n %s\n This batch run will be skipped.", run.fastaPath, e.getMessage()), "Fasta file validation failed");
                return false;
            }
            // if fasta validates, update the workflow file with the new path
            try {
                editWorkflowFasta(run.workflowPath, run.fastaPath);
            } catch (IOException e) {
                SwingUtils.showErrorDialog(parent, String.format("Could not edit workflow file %s with the new fasta path %s.\n This batch run will be skipped.", run.workflowPath, run.fastaPath), "Workflow file editing failed");
                return false;
            }
        }
        return true;
    }

    private static void editWorkflowFasta(Path workflowPath, Path fastaPath) throws IOException {
        BufferedReader in = new BufferedReader(new FileReader(workflowPath.toFile()));
        StringBuilder sb = new StringBuilder();
        String line;
        while ((line = in.readLine()) != null) {
            if (line.startsWith("database.db-path=")) {
                String val = saveConvert(fastaPath.toAbsolutePath().normalize().toString(), false, true); // save absolute path of the fasta file
                line = "database.db-path=" + val;
            }
            sb.append(line).append("\n");
        }
        in.close();
        BufferedWriter out = new BufferedWriter(new FileWriter(workflowPath.toFile()));
        out.write(sb.toString());
        out.flush();
        out.close();
    }

    // Load job(s) or job manifest file(s)
    private void actionBtnLoadBatchTemplate(ActionEvent event, String startPath) {
        List<FileFilter> fileFilters = new ArrayList<>();
        FileFilter filter = new FileNameExtensionFilter("Job Manifest file (.job) or .tsv", "job", "tsv");
        fileFilters.add(filter);

        JFileChooser fc = FileChooserUtils.builder("Select the Job Manifest file to load")
                .approveButton("Select").mode(FileChooserUtils.FcMode.FILES_ONLY)
                .acceptAll(false).multi(true).filters(fileFilters)
                .paths(Stream.of(startPath)).create();


        int userSelection = fc.showOpenDialog(SwingUtils.findParentFrameForDialog(this));
        if (JFileChooser.APPROVE_OPTION == userSelection) {
            ArrayList<File> files = new ArrayList<>(Arrays.asList(fc.getSelectedFiles()));
            List<BatchRun> runs = new ArrayList<>();
            for (File file : files) {
                Fragpipe.propsVarSet(PROP_FILECHOOSER_LAST_PATH, file.getParent());
                runs.addAll(parseBatchTemplate(this, file.toString()));
            }
            addBatchRuns(runs);
        }
    }

    public void addBatchRuns(List<BatchRun> runs) {
        batchTable.addData(runs);
    }

    private void actionBtnSaveBatchTemplate(ActionEvent event) {
        // get file path to save
        FileNameEndingFilter filter = new FileNameEndingFilter("Job Manifest (.job)", "job");
        Path savePath = TabWorkflow.getSaveFilePath(null, PROP_FILECHOOSER_LAST_PATH, filter, ".job", false, this);

        if (savePath == null) {
            // user cancelled action
            return;
        }

        List<BatchRun> runs = batchTable.model.getRuns();
        if (runs.isEmpty()) {
            return;
        }
        // save to file
        try {
            saveJobsToFile(runs, savePath);
        } catch (IOException ex) {
            log.error("Could not save job manifest to file {} due to error: {}", savePath, ex.getMessage());
            SwingUtils.showErrorDialogWithStacktrace(ex, this);
        }
    }

    public static void saveJobsToFile(List<BatchRun> runs, Path savePath) throws IOException{
        PrintWriter out = new PrintWriter(Files.newBufferedWriter(savePath));
        out.print("# " + String.join("\t", TABLE_BATCH_COL_NAMES) + "\n");
        for (BatchRun run : runs) {
            out.print(run.toString());
            out.print("\n");
        }
        out.flush();
        out.close();
    }

    private void btnRemoveSelected() {
        int[] removeRows = batchTable.getSelectedRows();
        HashSet<Integer> rowsToRemove = Arrays.stream(removeRows).boxed().collect(Collectors.toCollection(HashSet::new));
        // remove in reverse order to avoid index shifting errors
        for (int i = batchTable.model.getRowCount(); i >= 0; i--) {
            if (rowsToRemove.contains(i)) {
                batchTable.model.removeRow(i);
            }
        }
    }

    private void clearTable() {
        batchTable.setData(new ArrayList<>());
    }

    private void openJobsFolder() {
        Path jobsDir = FragpipeLocations.get().getDirJobs();
        if (Files.exists(jobsDir) && Files.isDirectory(jobsDir)) {
            try {
                Desktop.getDesktop().open(jobsDir.toFile());
            } catch (IOException e) {
                log.error("Could not open jobs folder: {}", e.getMessage());
                SwingUtils.showErrorDialog(this, "Could not open jobs folder: " + e.getMessage(), "Error");
            }
        } else {
            SwingUtils.showErrorDialog(this, "Jobs folder does not exist or is not a directory.", "Error");
        }
    }
}
