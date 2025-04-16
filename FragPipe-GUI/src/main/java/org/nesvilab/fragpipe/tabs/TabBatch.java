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

import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.nesvilab.fragpipe.Fragpipe;
import org.nesvilab.fragpipe.FragpipeRun;
import org.nesvilab.fragpipe.api.*;
import org.nesvilab.fragpipe.cmd.*;
import org.nesvilab.fragpipe.messages.MessageRunBatch;
import org.nesvilab.fragpipe.messages.MessageRunButtonEnabled;
import org.nesvilab.fragpipe.messages.MessageStartProcesses;
import org.nesvilab.fragpipe.process.ProcessDescription;
import org.nesvilab.fragpipe.process.ProcessDescription.Builder;
import org.nesvilab.fragpipe.process.RunnableDescription;
import org.nesvilab.fragpipe.util.BatchRun;
import org.nesvilab.utils.OsUtils;
import org.nesvilab.utils.SwingUtils;
import org.nesvilab.utils.swing.*;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.nio.file.Files;
import java.nio.file.Path;
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

    private JPanel pBottom;
    private JPanel pConsole;
    private UiCheck uiCheckWordWrap;

    private BatchTable batchTable;
    private static final String[] TABLE_BATCH_COL_NAMES = {"Workflow File Path", "Manifest File Path", "Output Directory",
            "Tools Folder Path", "RAM", "Threads"};
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
        boolean runConfigurationDone = false;
        try {
            Bus.post(new MessageRunButtonEnabled(false));

            // prepare the processes
            List<ProcessBuildersDescriptor> pbDescsBuilderDescs = new ArrayList<>(1);

            for (BatchRun run : batchTable.model.getRuns()) {
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
                printReference();
                return 0;
            }

            // run everything
            long startTime = System.nanoTime();
            final List<RunnableDescription> toRun = new ArrayList<>();
            for (final ProcessBuilderInfo pbi : pbis) {
                Runnable runnable = ProcessBuilderInfo.toRunnable(pbi, m.workdir, FragpipeRun::printProcessDescription, console, false);
                ProcessDescription.Builder b = new ProcessDescription.Builder().setName(pbi.name);
                if (pbi.pb.directory() != null) {
                    b.setWorkDir(pbi.pb.directory().toString());
                }
                if (pbi.pb.command() != null && !pbi.pb.command().isEmpty()) {
                    b.setCommand(String.join(" ", pbi.pb.command()));
                }
                toRun.add(new RunnableDescription(b.create(), runnable, pbi.parallelGroup, pbi));
            }

            // add finalizer process
            final Runnable finalizerRun = () -> {
                String totalTime = String.format("%.1f", (System.nanoTime() - startTime) * 1e-9 / 60);
                toConsole(Fragpipe.COLOR_RED_DARKEST, "\n=============================================================ALL JOBS DONE IN " + totalTime + " MINUTES=============================================================", true, console);

                Bus.post(new MessageRunButtonEnabled(true));
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

    // todo: print all? check from workflows?
    private void printReference() {
        toConsole(Fragpipe.COLOR_RED_DARKEST, "\nPlease cite:", true, console);
        toConsole(Fragpipe.COLOR_BLACK, "Teo, G., et al. SAINTexpress: improvements and additional features in Significance Analysis of INTeractome software. J Proteomics, 100:37 (2014)", true, console);
    }

    private JPanel createPanelBottom(TextConsole console) {
        uiCheckDryRun = UiUtils.createUiCheck("Dry Run", false);
        btnRun = UiUtils.createButton("Run", e -> Bus.post(new MessageRunBatch(isDryRun(), null)));

        JButton btnClearConsole = UiUtils.createButton("Clear Console", e -> clearConsole());
        uiCheckWordWrap = UiUtils.createUiCheck("Word wrap", true, e -> {
            console.setScrollableTracksViewportWidth(uiCheckWordWrap.isSelected());
            console.setVisible(false);
            console.setVisible(true);
        });

        console.setScrollableTracksViewportWidth(true);

        JPanel p = mu.newPanel(null, true);
        mu.add(p, btnRun).split(5);
        mu.add(p, uiCheckDryRun);
        mu.add(p, btnClearConsole);
        mu.add(p, uiCheckWordWrap).wrap();

        return p;
    }

    @Subscribe(threadMode = ThreadMode.ASYNC)
    public void on(MessageRunBatch m) {
        int returnCode = runBatch(m);
        if (Fragpipe.headless && returnCode != 0) {
            System.exit(returnCode);
        }
    }

    public boolean isDryRun() {
        return SwingUtils.isEnabledAndChecked(uiCheckDryRun);
    }


    protected void init() {
        defTextColor = UIManager.getColor("TextField.foreground");
        if (defTextColor == null) {
            defTextColor = Color.BLACK;
        }

        JPanel pBatch = new JPanel(new MigLayout(new LC()));
        pBatch.setBorder(new TitledBorder("Batch FragPipe Runs"));
        batchTable = createBatchTable();

        JScrollPane tableScrollBatch = new JScrollPane(batchTable, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        tableScrollBatch.setPreferredSize(new Dimension(1200, 200));
        SwingUtilities.invokeLater(() -> {
            setJTableColSize(batchTable, 0, 20, 1000, 600);
            setJTableColSize(batchTable, 3, 10, 150, 100);
            setJTableColSize(batchTable, 4, 10, 150, 100);
        });

        JButton btnLoadBatchTemplate = new JButton("Load Batch Template");
        btnLoadBatchTemplate.addActionListener(this::actionBtnLoadBatchTemplate);
        JButton btnSaveBatchTemplate = new JButton("Save Batch Template");
        btnSaveBatchTemplate.addActionListener(this::actionBtnSaveBatchTemplate);

        mu.add(pBatch, btnLoadBatchTemplate).split();
        mu.add(pBatch, btnSaveBatchTemplate).split().wrap();
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
            data[i][4] = run.ram;
            data[i][5] = run.threads;
        }
        return data;
    }

    private List<BatchRun> parseBatchTemplate(String templatePath) {
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
                int ram = 0, threads = 0;
                try {
                    ram = splits.length > 4 ? Integer.parseInt(splits[4]) : 0;
                    threads = splits.length > 5 ? Integer.parseInt(splits[5]) : 0;
                } catch (NumberFormatException e) {
                    log.error("Invalid number format for RAM or threads in batch template line {}: {}", lineIndex, e.getMessage());
                    SwingUtils.showErrorDialog(this, "Invalid number format for RAM or threads in batch template line " + lineIndex + ": " + e.getMessage(), "Number Format Error");
                }
                BatchRun run = new BatchRun(workflowPath, manifestPath, outputDir, toolsFolderPath, ram, threads);
                if (checkPaths(this, run, false)) {
                    runs.add(run);
                }
                lineIndex++;
            }
        } catch (IOException e) {
            log.error("Error reading batch template file: {}", e.getMessage());
            SwingUtils.showErrorDialogWithStacktrace(e, this);
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
            SwingUtils.showErrorDialog(parent, String.format("Tools directory not found: %s\n. This batch run will be skipped.", run.toolsStr), "Directory Not Found");
            return false;
        }
        if (!(Files.exists(run.outputPath) && Files.isDirectory(run.outputPath))) {
            if (makeDirs) {
                if (!run.outputPath.toFile().mkdirs()) {
                    SwingUtils.showErrorDialog(parent, "Could not create output directory: " + run.outputPath, "Directory Creation Failed");
                    return false;
                }
            }
        }
        return true;
    }


    private void actionBtnLoadBatchTemplate(ActionEvent event) {
        List<FileFilter> tsvFilters = new ArrayList<>();
        FileFilter filter = new FileNameExtensionFilter("Batch template file (.tsv or .txt)", "tsv", "txt");
        tsvFilters.add(filter);

        String loc = Fragpipe.propsVarGet(PROP_FILECHOOSER_LAST_PATH);
        JFileChooser fc = FileChooserUtils.builder("Select the Batch template file to load")
                .approveButton("Select").mode(FileChooserUtils.FcMode.FILES_ONLY)
                .acceptAll(false).multi(false).filters(tsvFilters)
                .paths(Stream.of(loc)).create();


        String selectedPath;
        int userSelection = fc.showOpenDialog(SwingUtils.findParentFrameForDialog(this));
        if (JFileChooser.APPROVE_OPTION == userSelection) {
            selectedPath = fc.getSelectedFile().toString();
            Fragpipe.propsVarSet(PROP_FILECHOOSER_LAST_PATH, selectedPath);
            List<BatchRun> runs = parseBatchTemplate(selectedPath);
            batchTable.setData(runs);
        }
    }

    private void actionBtnSaveBatchTemplate(ActionEvent event) {
        // get file path to save
        FileNameEndingFilter filter = new FileNameEndingFilter("Batch Template (.tsv)", "tsv");
        Path savePath = TabWorkflow.getSaveFilePath(null, PROP_FILECHOOSER_LAST_PATH, filter, ".tsv", false, this);

        if (savePath == null) {
            // user cancelled action
            return;
        }

        Vector<Vector> runs = batchTable.model.getDataVector();
        if (runs.isEmpty()) {
            return;
        }
        // save to file
        try {
            PrintWriter out = new PrintWriter(savePath.toFile());
            out.print("# " + String.join("\t", TABLE_BATCH_COL_NAMES) + "\n");
            for (Vector<?> row : runs) {
                out.print(row.stream().map(Object::toString).collect(Collectors.joining("\t")));
                out.print("\n");
            }
            out.flush();
            out.close();
        } catch (IOException ex) {
            log.error("Could not save batch template to file {} due to error: {}", savePath, ex.getMessage());
            SwingUtils.showErrorDialogWithStacktrace(ex, this);
        }
    }
}
