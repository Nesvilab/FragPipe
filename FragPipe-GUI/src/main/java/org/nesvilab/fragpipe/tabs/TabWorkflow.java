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

import static org.nesvilab.fragpipe.Fragpipe.PROP_NOCACHE;
import static org.nesvilab.fragpipe.Fragpipe.getStickyStrict;
import static org.nesvilab.fragpipe.Fragpipe.propsVarGet;
import static org.nesvilab.fragpipe.Fragpipe.propsVarSet;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Optional;
import java.util.Properties;
import java.util.Set;
import java.util.TreeMap;
import java.util.concurrent.ConcurrentLinkedDeque;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.swing.Box;
import javax.swing.ButtonGroup;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.SwingUtilities;
import javax.swing.border.LineBorder;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;
import javax.swing.filechooser.FileNameExtensionFilter;
import javax.swing.table.DefaultTableModel;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.apache.commons.io.FileUtils;
import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.ThreadMode;
import org.jooq.lambda.Seq;
import org.jooq.lambda.Unchecked;
import org.nesvilab.fragpipe.Fragpipe;
import org.nesvilab.fragpipe.FragpipeLocations;
import org.nesvilab.fragpipe.Version;
import org.nesvilab.fragpipe.api.Bus;
import org.nesvilab.fragpipe.api.FragpipeCacheUtils;
import org.nesvilab.fragpipe.api.InputLcmsFile;
import org.nesvilab.fragpipe.api.LcmsFileGroup;
import org.nesvilab.fragpipe.api.LcmsInputFileTable;
import org.nesvilab.fragpipe.api.PropsFile;
import org.nesvilab.fragpipe.api.SimpleETable;
import org.nesvilab.fragpipe.api.TableModelColumn;
import org.nesvilab.fragpipe.api.UniqueLcmsFilesTableModel;
import org.nesvilab.fragpipe.cmd.CmdMsfragger;
import org.nesvilab.fragpipe.dialogs.SetExpDialog;
import org.nesvilab.fragpipe.dialogs.SetRepDialog;
import org.nesvilab.fragpipe.messages.*;
import org.nesvilab.fragpipe.messages.MessageLcmsGroupAction.Type;
import org.nesvilab.fragpipe.params.ThisAppProps;
import org.nesvilab.fragpipe.tools.diatracer.DiaTracerPanel;
import org.nesvilab.fragpipe.tools.tmtintegrator.QuantLabel;
import org.nesvilab.fragpipe.tools.umpire.UmpirePanel;
import org.nesvilab.fragpipe.util.BatchRun;
import org.nesvilab.fragpipe.util.SDRFtable;
import org.nesvilab.utils.FileDrop;
import org.nesvilab.utils.MapUtils;
import org.nesvilab.utils.OsUtils;
import org.nesvilab.utils.PathUtils;
import org.nesvilab.utils.PropertiesUtils;
import org.nesvilab.utils.StringUtils;
import org.nesvilab.utils.SwingUtils;
import org.nesvilab.utils.swing.FileChooserUtils;
import org.nesvilab.utils.swing.FileChooserUtils.FcMode;
import org.nesvilab.utils.swing.FileNameEndingFilter;
import org.nesvilab.utils.swing.FormEntry;
import org.nesvilab.utils.swing.HtmlStyledJEditorPane;
import org.nesvilab.utils.swing.JPanelWithEnablement;
import org.nesvilab.utils.swing.MigUtils;
import org.nesvilab.utils.swing.UiCombo;
import org.nesvilab.utils.swing.UiSpinnerInt;
import org.nesvilab.utils.swing.UiText;
import org.nesvilab.utils.swing.UiUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class TabWorkflow extends JPanelWithEnablement {

  private static final Logger log = LoggerFactory.getLogger(TabWorkflow.class);
  private final MigUtils mu = MigUtils.get();
  private JButton btnFilesRemove;
  private JButton btnFilesClear;
  public static final String TAB_PREFIX = "workflow.";
  public static final String manifestExt = ".fp-manifest";
  public static final String sdrfExt = "sdrf.tsv";
  public static final String workflowExt = ".workflow";
  public static final int maxProcessors = 128;

  private SimpleETable tableRawFiles;
  private UniqueLcmsFilesTableModel tableModelRawFiles;
  private FileDrop tableRawFilesFileDrop;
  private JScrollPane scrollPaneRawFiles;
  private JButton btnGroupsConsecutiveExperiment;
  private JButton btnGroupsConsecutiveBioreplicate;
  private JButton btnGroupsConsecutiveBioreplicateByExperiment;
  private JButton btnGroupsByParentDir;
  private JButton btnGroupsByFilename;
  private JButton btnSetExp;
  private JButton btnSetRep;
  private JButton btnSetDda;
  private JButton btnSetDia;
  private JButton btnSetGpfDia;
  private JButton btnSetDiaQuant;
  private JButton btnSetDiaLib;
  private JButton btnSetDdaPlus;
  private JButton btnGroupsClearExperiment;
  private JButton btnGroupsClearBioreplicate;
  private JButton btnManifestSave;
  private JButton btnManifestLoad;
  private HtmlStyledJEditorPane epWorkflowsInfo;
  private JLabel numSelectedFilesLabel;
  private UiSpinnerInt uiSpinnerRam;
  private UiSpinnerInt uiSpinnerThreads;
  public Map<String, PropsFile> workflows;
  private UiCombo uiComboWorkflows;
  public static final String PROP_WORKFLOW_DESC = "workflow.description";
  public static final String PROP_WORKFLOW_SAVED_WITH_VER = "workflow.saved-with-ver";
  public static final String PROP_WORKFLOW_SAVEDIR = "workflow.last-save-dir";
  private HtmlStyledJEditorPane epWorkflowsDesc;
  private UiText uiTextLastAddedLcmsDir;
  private ButtonGroup btnGroupMsType;
  private JRadioButton btnTypeRegularMs;
  private JRadioButton btnTypeIms;

  private static final Set<String> builtInWorkflows = new HashSet<>(); // this list also include renamed and deleted ones.

  static {
    builtInWorkflows.add("common-mass-offsets");
    builtInWorkflows.add("Common-mass-offsets");
    builtInWorkflows.add("glyco-N-open-Hybrid");
    builtInWorkflows.add("Labile_phospho");
    builtInWorkflows.add("TMT10-MS3");
    builtInWorkflows.add("Default");
    builtInWorkflows.add("glyco-N-TMT");
    builtInWorkflows.add("LFQ");
    builtInWorkflows.add("TMT10-MS3-phospho");
    builtInWorkflows.add("DIA-MSFragger_SpecLib");
    builtInWorkflows.add("glyco-O-HCD");
    builtInWorkflows.add("LFQ-MBR");
    builtInWorkflows.add("TMT10-phospho");
    builtInWorkflows.add("DIA-Umpire_SpecLib");
    builtInWorkflows.add("glyco-O-Hybrid");
    builtInWorkflows.add("Nonspecific-HLA");
    builtInWorkflows.add("TMT10-phospho-bridge");
    builtInWorkflows.add("glyco-N-HCD");
    builtInWorkflows.add("glyco-O-open-HCD");
    builtInWorkflows.add("Nonspecific-peptidome");
    builtInWorkflows.add("TMT16");
    builtInWorkflows.add("glyco-N-Hybrid");
    builtInWorkflows.add("glyco-O-open-Hybrid");
    builtInWorkflows.add("Open");
    builtInWorkflows.add("Diagnostic-ion-mining");
    builtInWorkflows.add("TMT16-MS3");
    builtInWorkflows.add("glyco-N-LFQ");
    builtInWorkflows.add("iTRAQ4");
    builtInWorkflows.add("TMT10");
    builtInWorkflows.add("glyco-N-open-HCD");
    builtInWorkflows.add("Labile_ADP-ribosylation");
    builtInWorkflows.add("TMT10-bridge");
    builtInWorkflows.add("glyco-N-quant-HCD");
    builtInWorkflows.add("DIA-Umpire");
    builtInWorkflows.add("SILAC3");
    builtInWorkflows.add("SILAC3-phospho");
    builtInWorkflows.add("SpecLib");
    builtInWorkflows.add("TMT10-ubiquitin");
    builtInWorkflows.add("Mass-Offset-CommonPTMs");
    builtInWorkflows.add("isoDTB");
    builtInWorkflows.add("isoDTB-ABPP");
    builtInWorkflows.add("MSFragger-DIA-narrow-window-SpecLib");
    builtInWorkflows.add("MSFragger-DIA-wide-window-SpecLib");
    builtInWorkflows.add("DIA_DIA-Umpire_SpecLib_Quant");
    builtInWorkflows.add("DIA_SpecLib_Quant");
    builtInWorkflows.add("isoTOP-ABPP");
    builtInWorkflows.add("SLC-ABPP");
    builtInWorkflows.add("TMT10-acetyl");
    builtInWorkflows.add("TMT16-acetyl");
    builtInWorkflows.add("XRNAX-MassOffset");
    builtInWorkflows.add("LFQ-phospho");
    builtInWorkflows.add("TMT16-phospho");
    builtInWorkflows.add("ipIAA-ABPP");
    builtInWorkflows.add("Custom");
    builtInWorkflows.add("Nonspecific-HLA-C57");
    builtInWorkflows.add("diaTOP_ABPP");
    builtInWorkflows.add("LFQ-ubiquitin");
    builtInWorkflows.add("Nonspecific-HLA-DIA");
    builtInWorkflows.add("TMT10-Open");
    builtInWorkflows.add("Nonspecific-HLA-glyco");
    builtInWorkflows.add("Nonspecific-HLA-phospho");
    builtInWorkflows.add("Nonspecific-HLA-TMT10");
    builtInWorkflows.add("glyco-O-Pair");
    builtInWorkflows.add("FPOP");
    builtInWorkflows.add("iTRAQ4-phospho");
    builtInWorkflows.add("TMT10-acetyl-noloc");
    builtInWorkflows.add("TMT16-acetyl-noloc");
    builtInWorkflows.add("TMT10-ubiquitination-K_tmt_or_ubiq");
    builtInWorkflows.add("TMT10-ubiquitination-K_tmt_plus_ubiq");
    builtInWorkflows.add("Open-quickscan");
    builtInWorkflows.add("WWA");
    builtInWorkflows.add("DIA_SpecLib_Quant_Phospho");
    builtInWorkflows.add("TMT16-ubiquitination-K_tmt_or_ubiq");
    builtInWorkflows.add("TMT16-ubiquitination-K_tmt_plus_ubiq");
    builtInWorkflows.add("glyco-O-DIA-OPair");
    builtInWorkflows.add("glyco-O-DIA-HCD");
    builtInWorkflows.add("DIA_SpecLib_Quant_Phospho_diaPASEF");
    builtInWorkflows.add("DIA_SpecLib_Quant_diaPASEF");
    builtInWorkflows.add("Nonspecific-HLA-diaPASEF");
    builtInWorkflows.add("PAL");
    builtInWorkflows.add("glyco-N-DIA");
    builtInWorkflows.add("Nonspecific-HLA-DIA-Astral");
    builtInWorkflows.add("Stellar-DDA");
    builtInWorkflows.add("Stellar-GPFDIA");
    builtInWorkflows.add("DIA_SpecLib_Quant_Ubiq");
    builtInWorkflows.add("TMT18-Astral");
    builtInWorkflows.add("TMT35");
    builtInWorkflows.add("chemprot-ABPP-diaTOP");
    builtInWorkflows.add("chemprot-ABPP-IADTB-diaPASEF");
    builtInWorkflows.add("chemprot-ABPP-IADTB-TMT16");
    builtInWorkflows.add("chemprot-ABPP-ipIAA");
    builtInWorkflows.add("chemprot-ABPP-isoDTB");
    builtInWorkflows.add("chemprot-ABPP-isoTOP");
    builtInWorkflows.add("chemprot-PAL");
    builtInWorkflows.add("Nonspecific-HLA-customDB-groupFDR");
    builtInWorkflows.add("citrullination");
    builtInWorkflows.add("Basic-Search");
  }

  // Ok, if we could keep some workflows pinned toward the top,  I would say Default, SpecLib, Open, Common-mass-offset, LFQ-MBR,  then the rest
  private static final List<String> builtInWorkflowsPinned = ((Supplier<List<String>>) () -> {
    final List<String> a = new ArrayList<>();
    a.add("Basic-Search");
    a.add("Custom");
    a.add("Open");
    a.add("Open-quickscan");
    a.add("Mass-Offset-CommonPTMs");
    a.add("LFQ-MBR");
    a.add("DIA_SpecLib_Quant");
    return a;
  }).get();

  public TabWorkflow() {
    init();
    initMore();
  }

  public static UniqueLcmsFilesTableModel createTableModelRawFiles() {
    List<TableModelColumn<InputLcmsFile, ?>> cols = new ArrayList<>();

    TableModelColumn<InputLcmsFile, String> colPath = new TableModelColumn<>(
        "Path (can drag & drop from Explorer)",
        String.class, false, data -> data.getPath().toString());
    TableModelColumn<InputLcmsFile, String> colExp = new TableModelColumn<>(
        "Experiment (can be empty, alphanumeric, and _)", String.class, true, InputLcmsFile::getExperiment);
    TableModelColumn<InputLcmsFile, Integer> colRep = new TableModelColumn<>(
        "Bioreplicate (can be empty and integer)", Integer.class, true, InputLcmsFile::getReplicate);
    TableModelColumn<InputLcmsFile, String> colDataType = new TableModelColumn<>(
        "Data type (DDA, DDA+, DIA, DIA-Quant, DIA-Lib, GPF-DIA)", String.class, true, InputLcmsFile::getDataType);

    cols.add(colPath);
    cols.add(colExp);
    cols.add(colRep);
    cols.add(colDataType);

    UniqueLcmsFilesTableModel m = new UniqueLcmsFilesTableModel(cols, 0);

    return m;
  }

  public static void processAddedLcmsPaths(LcmsFileAddition files, Component parent) {
    final HashMap<Path, Set<String>> reasonsDir = new HashMap<>();
    final HashMap<Path, Set<String>> reasonsFn = new HashMap<>();

    for (Path path : files.paths) {
      Set<String> why = InputLcmsFile.validatePath(path.toAbsolutePath().getParent().toString());
      if (!why.isEmpty()) {
        reasonsDir.put(path, why);
      }
    }

    for (Path path : files.paths) {
      Set<String> why = InputLcmsFile.validateFilename(path.getFileName().toString());
      if (!why.isEmpty()) {
        reasonsFn.put(path, why);
      }
    }

    // in case there were suspicious paths
    if (!reasonsDir.isEmpty() || !reasonsFn.isEmpty()) {
      HashMap<Path, String> path2reasons = new HashMap<>();
      for (Entry<Path, Set<String>> kv : reasonsDir.entrySet()) {
        for (String reason : kv.getValue()) {
          path2reasons.compute(kv.getKey(), (path, s) -> s == null ? "Directory " + reason : s.concat(", Directory " + reason));
        }
      }
      for (Entry<Path, Set<String>> kv : reasonsFn.entrySet()) {
        for (String reason : kv.getValue()) {
          path2reasons.compute(kv.getKey(), (path, s) -> s == null ? "File name " + reason : s.concat(", File name " + reason));
        }
      }
      String[] columns = {"Reasons", "Path"};
      String[][] data = new String[path2reasons.size()][2];
      int index = -1;
      for (Entry<Path, String> kv : path2reasons.entrySet()) {
        data[++index][0] = kv.getValue();
        data[index][1] = kv.getKey().toString();
      }

      DefaultTableModel model = new DefaultTableModel(data, columns);
      JTable table = new JTable(model);
      table.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
      JPanel panel = new JPanel(new BorderLayout());
      panel.add(new JLabel("<html>Found problems with some files (" + path2reasons.size() + " of " + files.paths.size() + ").<br/>"
          + "This <b>will likely cause trouble</b> with some of the processing tools.<br/><br/>"
          + "What do you want to do with these files?<br/>"), BorderLayout.NORTH);
      panel.add(Box.createVerticalStrut(100), BorderLayout.CENTER);
      panel.add(new JScrollPane(table), BorderLayout.CENTER);
      SwingUtils.makeDialogResizable(panel);

      String[] options = new String[]{"Cancel", "Only add well-behaved files", "Try to rename files"};
      if (!reasonsDir.isEmpty()) { // Can not rename directory names.
        options = new String[]{"Cancel", "Only add well-behaved files"};
      }

      int confirmation = JOptionPane
          .showOptionDialog(parent, panel, "Add these files?",
              JOptionPane.DEFAULT_OPTION, JOptionPane.WARNING_MESSAGE, null, options, options[0]);

      switch (confirmation) {
        case 0:
          files.toAdd.clear();
          break;
        case 1:
          files.toAdd = files.toAdd.stream().filter(path -> !path2reasons.containsKey(path)).collect(Collectors.toList());
          break;
        case 2: // rename files
          int confirm1 = SwingUtils.showConfirmDialog(parent, new JLabel(
              "<html>Attempt to rename files without moving them.<br/>\n" +
                  "This is a non-reversible operation.<br/><br/>\n" +
                  "We'll show you a preview before proceeding with the renaming.<br/>\n" +
                  "Do you want to continue?"));
          if (JOptionPane.YES_OPTION != confirm1) {
            return;
          }
          final Map<Path, Path> toRename = reasonsFn.keySet().stream()
              .collect(Collectors.toMap(Function.identity(), InputLcmsFile::renameBadFile));
          Set<Path> uniqueRenamed = new HashSet<>(toRename.values());
          if (uniqueRenamed.size() != reasonsFn.size()) {
            SwingUtils.showDialog(parent, new JLabel(
                    "<html>Renaming given files according to our scheme would result<br/>\n" +
                        "in clashing file paths. Renaming cancelled. Consider renaming manually.<br/>\n" +
                        "It is preferable to not have spaces in file names, not have more than one dot<br/>\n" +
                        "and to not use non-latin characters."),
                "Not safe to rename files", JOptionPane.WARNING_MESSAGE);
            return;
          }

          final Map<Path, Path> existingRenames = new HashMap<>();
          for (Entry<Path, Path> kv : toRename.entrySet()) {
            if (Files.exists(kv.getValue())) {
              existingRenames.put(kv.getKey(), kv.getValue());
            }
          }
          if (!existingRenames.isEmpty()) {
            JPanel pane = new JPanel(new BorderLayout());
            pane.add(new JLabel("<html>Renaming given files according to our scheme would result<br/>\n" +
                "in file paths that already exist on your computer.<br/>\n" +
                "Renaming cancelled."), BorderLayout.NORTH);

            pane.add(new JScrollPane(SwingUtils.tableFromTwoSiblingFiles(existingRenames)));
            SwingUtils.showDialog(parent, pane, "Not safe to rename files", JOptionPane.WARNING_MESSAGE);
            return;
          }

        {
          JPanel pane = new JPanel(new BorderLayout());
          pane.add(new JLabel("<html>Proposed renaming scheme, do you want to proceed?<br/>\n"));
          pane.add(new JScrollPane(SwingUtils.tableFromTwoSiblingFiles(toRename)));
          int confirm2 = SwingUtils.showConfirmDialog(parent, pane);
          if (JOptionPane.YES_OPTION != confirm2) {
            return;
          }
        }

        final Map<Path, Path> couldNotRename = new HashMap<>();
        final Map<Path, Path> renamedOk = new HashMap<>();
        Runnable runnable = () -> {
          for (Entry<Path, Path> kv : toRename.entrySet()) {
            try {
              Files.move(kv.getKey(), kv.getValue());
              renamedOk.put(kv.getKey(), kv.getValue());
            } catch (Exception e) {
              log.error(String.format("From '%s' to '%s' at '%s'",
                  kv.getKey().getFileName(), kv.getValue().getFileName(), kv.getKey().toAbsolutePath().getParent()));
              couldNotRename.put(kv.getKey(), kv.getValue());
            }
          }
        };

        SwingUtils.DialogAndThread dat = SwingUtils.runThreadWithProgressBar("Renaming files", parent, runnable);
        dat.thread.start();
        dat.dialog.setVisible(true);
        try {
          dat.thread.join();
        } catch (InterruptedException e) {
          throw new RuntimeException(e);
        }
        if (!couldNotRename.isEmpty()) {
          JPanel pane = new JPanel(new BorderLayout());
          pane.add(new JLabel("<html>Could not rename some of the files:<br/>"), BorderLayout.NORTH);
          pane.add(new JScrollPane(SwingUtils.tableFromTwoSiblingFiles(couldNotRename)), BorderLayout.CENTER);
          SwingUtils.showDialog(parent, pane, "Renaming failed", JOptionPane.WARNING_MESSAGE);
          return;
        }

        // renaming succeeded, change paths to renamed ones
        files.toAdd = files.toAdd.stream().map(path -> renamedOk.getOrDefault(path, path)).collect(Collectors.toList());

        break;
      }
    }
  }

  private void initMore() {
    SwingUtils.renameDeep(this, false, TAB_PREFIX, null);
    tableRawFilesFileDrop = makeFileDrop(); // file drop is registered after all components are created
    Bus.register(this);
    Bus.postSticky(this);
  }

  private void init() {
    this.setLayout(new MigLayout(new LC().fillX()));

    add(createPanelWorkflows(), mu.ccGx().wrap());
    add(createPanelOptions(), mu.ccGx().wrap());
    add(createPanelLcmsFiles(), mu.ccGx().wrap());
  }

  private FormEntry.Builder fe(JComponent comp, String name) {
    return Fragpipe.fe(comp, name, TAB_PREFIX);
  }

  private JPanel createPanelOptions() {
    JPanel p = mu.newPanel("Global settings", true);

    uiSpinnerRam = new UiSpinnerInt(0, 0, 1024, 1, 3);
    FormEntry feRam = fe(uiSpinnerRam, "ram").label("RAM (GB, 0=auto)").tooltip("Leave at zero to automatically use a reasonable amount of memory").create();
    uiSpinnerThreads = new UiSpinnerInt(Math.max(1, Math.min(Runtime.getRuntime().availableProcessors() - 1, maxProcessors)), 1, maxProcessors, 1);
    FormEntry feThreads = fe(uiSpinnerThreads, "threads").label("Parallelism").create();

    mu.add(p, feRam.label()).split();
    mu.add(p, feRam.comp);
    mu.add(p, feThreads.label());
    mu.add(p, feThreads.comp).pushX().wrap();

    return p;
  }

  public int getRamGb() {
    return uiSpinnerRam.getActualValue() > 0 ? uiSpinnerRam.getActualValue() : OsUtils.getDefaultXmx();
  }

  public int getThreads() {
    return uiSpinnerThreads.getActualValue() > 0 ? uiSpinnerThreads.getActualValue() : Math.max(1, Math.min(Runtime.getRuntime().availableProcessors(), maxProcessors));
  }

  public enum InputDataType {RegularMs, ImMsTimsTof}

  public InputDataType getInputDataType() {
    for (int i = 0; i < tableModelRawFiles.getRowCount(); ++i) {
      if (tableModelRawFiles.getValueAt(i, 0).toString().toLowerCase().endsWith(".d")) {
        return InputDataType.ImMsTimsTof;
      }
    }

    for (int i = 0; i < tableModelRawFiles.getRowCount(); ++i) {
      if (tableModelRawFiles.getValueAt(i, 0).toString().toLowerCase().endsWith(".raw")) {
        return InputDataType.RegularMs;
      }
    }

    if (btnTypeIms.isSelected()) {
      return InputDataType.ImMsTimsTof;
    } else {
      return InputDataType.RegularMs;
    }
  }

  private Map<String, PropsFile> findPropsFiles(Path startDir) throws IOException {
    final Map<String, PropsFile> files = new HashMap<>();
    final Predicate<Path> filter = p -> "workflow".equalsIgnoreCase(StringUtils.afterLastDot(p.getFileName().toString()));
    Files.walk(startDir).filter(Files::isRegularFile)
        .filter(filter)
        .forEach(Unchecked.consumer(path -> {
          PropsFile f = new PropsFile(path, "Loaded from: " + path.toString());
          f.load();
          if (f.size() > 0) {
            String s = f.getPath().getFileName().toString();
            String name = StringUtils.upToLastDot(s);
            files.put(StringUtils.isBlank(name) ? s : name, f);
          }
        }, throwable -> {
          log.error("Error while reading alleged workflow file", throwable);
        }));
    return files;
  }

  private Map<String, PropsFile> loadWorkflowFiles() {
    try {
      Map<String, PropsFile> files;
      Path dirWorkflows = FragpipeLocations.get().getDirWorkflows();
      Path dirLongTermStorage = FragpipeLocations.get().getPathLongTermStorage();
      Map<String, PropsFile> filesLocal = findPropsFiles(dirWorkflows);
      Map<String, PropsFile> filesStored = findPropsFiles(dirLongTermStorage);

      for (String builtInWorkflow : builtInWorkflows) {
        filesStored.remove(builtInWorkflow);
      }

      files = filesLocal;
      List<String> diffNames = MapUtils.keysDiffRight(filesLocal, filesStored).collect(Collectors.toList());
      List<PropsFile> diffPropFiles = Seq.seq(filesStored).filter(kv -> diffNames.contains(kv.v1)).map(kv -> kv.v2).toList();

      if (!diffNames.isEmpty()) {
        JLabel message = new JLabel(SwingUtils.makeHtml("Found workflows from previous FragPipe sessions:\n - "+Seq.seq(diffNames).sorted().toString("\n - ")));
        final String[] choices = {"Copy", "Ignore", "Delete"};
        final int choice = Fragpipe.headless ? JOptionPane.CLOSED_OPTION : SwingUtils.showChoiceDialog(this, "Load workflows?", message, choices, 0);
        switch (choice) {
          case 0:
            for (PropsFile propsFile : diffPropFiles) {
              Path p = dirWorkflows.resolve(propsFile.getPath().getFileName());
              propsFile.setPath(p);
              propsFile.save();
            }
            files = findPropsFiles(FragpipeLocations.get().getDirWorkflows());
            break;

          case 1:
          case JOptionPane.CLOSED_OPTION:
            break; // do nothing

          case 2:
            if (SwingUtils.showConfirmDialogShort(this, "Are you sure you want to delete stored workflows?")) {
              for (PropsFile diffPropFile : diffPropFiles) {
                Files.deleteIfExists(diffPropFile.getPath());
              }
            }
            break;
          default:
            throw new IllegalStateException("Unknown option [" + choice + "], probably forgot to add code branch for a newly added option");
        }
      }
      files.put("Custom", null);
      return files;
    } catch (IOException e) {
      SwingUtils.showErrorDialogWithStacktrace(e, this);
      return Collections.emptyMap();
    }
  }

  private JPanel createPanelWorkflows() {
    JPanel p = mu.newPanel("Workflows", true);

    final String link = Fragpipe.propsFix().getProperty("fragpipe.workflow-tutorial.url", "https://fragpipe.nesvilab.org/docs/tutorial_fragpipe.html");
    epWorkflowsInfo = SwingUtils.createClickableHtml(true,
        String.format("FragPipe supports multiple proteomic workflows.\n"
            + "Select and load an option from the dropdown menu below to configure "
            + "all the tools. Workflows can be customized, saved, and shared.\n"
            + "<a href=\"%s\">See the tutorial</a>.", link));

    workflows = loadWorkflowFiles();
    List<String> names = createNamesForWorkflowsCombo(workflows);
    uiComboWorkflows = UiUtils.createUiCombo(names);
    epWorkflowsDesc = new HtmlStyledJEditorPane();
    epWorkflowsDesc.setPreferredSize(new Dimension(400, 50));
    uiComboWorkflows.addItemListener(e -> {
      String name = (String) uiComboWorkflows.getSelectedItem();
      PropsFile propsFile = workflows.get(name);
      if (propsFile != null) {
        epWorkflowsDesc.setText(propsFile.getProperty(PROP_WORKFLOW_DESC, "Description not present"));
      } else {
        epWorkflowsDesc.setText("");
      }
    });
    uiComboWorkflows.addActionListener(this::actionLoadSelectedWorkflow);

    FormEntry feComboWorkflow = Fragpipe.feNoCache(uiComboWorkflows, "workflow-option").label("Select a workflow:").tooltip("Conveniently loads appropriate defaults for various standard workflows\n").create();
    JButton btnOpenInExplorer = SwingUtils.createButtonOpenInFileManager(this, "Open built-in folder", () -> FragpipeLocations.get().getDirWorkflows());
    JButton btnLoadJob = UiUtils.createButton("Load Previous Job", this::actionLoadJob);

    mu.add(p, epWorkflowsInfo).growX().spanX().wrap();
    mu.add(p, feComboWorkflow.label()).split();
    mu.add(p, feComboWorkflow.comp);
    mu.add(p, new JLabel("or save current settings as workflow")).gapLeft("15px");
    mu.add(p, UiUtils.createButton("Save to built-in folder", e -> Bus.post(new MessageSaveAsWorkflow(false))));
    mu.add(p, UiUtils.createButton("Save to custom folder", e -> Bus.post(new MessageSaveAsWorkflow(true))));
    if (false && Version.isDevBuild()) {
      mu.add(p, UiUtils.createButton("Save Dev", e -> Bus.post(new MessageSaveAsWorkflow(false, true))));
    }
    mu.add(p, btnOpenInExplorer);
    mu.add(p, btnLoadJob).wrap();

    mu.add(p, epWorkflowsDesc).growX().spanX().wrap();

    return p;
  }

  private void logObjectType(Object m) {
    log.debug("Got {}", m.getClass().getSimpleName());
  }

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void on(MessageLcmsAddFiles m) {
    logObjectType(m);

    final javax.swing.filechooser.FileFilter ff = CmdMsfragger.getFileChooserFilter();
    Predicate<File> supportedFilePredicate = CmdMsfragger.getSupportedFilePredicate();
    final var fc = new JFileChooser() {
      @Override
      public boolean isTraversable(final File f) {
        return f.isDirectory() && !f.toString().endsWith(".d");
      }
    };
    FileChooserUtils.create(fc, "Choose raw data files", "Select", true, FcMode.ANY, true, ff);
    fc.setFileFilter(ff);
    tableModelRawFiles.dataCopy();
    File cwd = null;
    File[] sf;
    do {
      if (cwd == null)
        FileChooserUtils.setPath(fc, Stream.of(ThisAppProps.load(ThisAppProps.PROP_LCMS_FILES_IN)));
      else
        fc.setCurrentDirectory(cwd);
      int result = fc.showDialog(this, "Select");
      if (JFileChooser.APPROVE_OPTION != result) {
        return;
      }
      sf = fc.getSelectedFiles();
      cwd = sf[0];
    } while (sf.length == 1 && sf[0].isDirectory() && !sf[0].toString().endsWith(".d"));
    final List<Path> paths = Arrays.stream(fc.getSelectedFiles()).filter(supportedFilePredicate).map(File::toPath).collect(Collectors.toList());
    if (paths.isEmpty()) {
      JOptionPane.showMessageDialog(this, "None of selected files/folders are supported.\nIf you are analyzing timsTOF (.d) data, please make sure that you have the latest MSFragger with ext folder exist.", "Warning", JOptionPane.WARNING_MESSAGE);
    } else {
      Bus.post(new MessageLcmsFilesAdded(paths));
    }
  }

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void on(MessageLcmsFilesAdded m) {
    // save locations
    String saveDir = null;
    if (m.recursiveAdditionRoot != null) {
      saveDir = m.recursiveAdditionRoot.toString();
    } else if (!m.paths.isEmpty()) {
      saveDir = m.paths.get(0).toAbsolutePath().getParent().toString();
    }
    if (saveDir != null) {
      propsVarSet(ThisAppProps.PROP_LCMS_FILES_IN, saveDir);
      if (uiTextLastAddedLcmsDir != null) {
        uiTextLastAddedLcmsDir.setText(saveDir);
      }
    }

    LcmsFileAddition lfa = new LcmsFileAddition(m.paths, new ArrayList<>(m.paths));
    processAddedLcmsPaths(lfa, this);

    // add the files
    List<InputLcmsFile> toAdd = lfa.toAdd.stream()
        .map(p -> new InputLcmsFile(p, ThisAppProps.DEFAULT_LCMS_EXP_NAME, null, null))
        .collect(Collectors.toList());
    if (!toAdd.isEmpty()) {
      tableModelRawFiles.dataAddAll(toAdd);
      postFileListUpdate();
      adjustToolsBasedOnDataTypes();
    }
  }

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void on(MessageLcmsAddFolder m) {
    logObjectType(m);
    List<Path> accepted = new ArrayList<>();
    List<Path> inputPaths;

    if (!m.dirs.isEmpty()) {
      inputPaths = m.dirs;
    }
    else {
      JFileChooser fc = FileChooserUtils
          .builder("Select a folder with LC/MS files (searched recursively)")
          .approveButton("Select").mode(FcMode.DIRS_ONLY).multi(true)
          .acceptAll(true)
          .paths(Stream.of(propsVarGet(ThisAppProps.PROP_LCMS_FILES_IN)))
          .create();

      if (JFileChooser.APPROVE_OPTION != fc.showOpenDialog(this)) {
        return;
      }
      File[] selectedFiles = fc.getSelectedFiles();
      inputPaths = Seq.of(selectedFiles).map(File::toPath).toList();
    }

    if (inputPaths.isEmpty()) {
      log.warn("No input paths were given");
      return;
    }
    propsVarSet(ThisAppProps.LAST_RECURSIVE_FOLDER_ADDED, inputPaths.get(0).toString());

    for (Path p : inputPaths) {
      final Predicate<File> pred = CmdMsfragger.getSupportedFilePredicate();
      PathUtils.traverseDirectoriesAcceptingFiles(p.toFile(), pred, accepted, false);
    }

    List<Path> lessGenerated = Seq.seq(accepted)
        .filter(p -> {
          final String fnLo = p.getFileName().toString().toLowerCase();
          if (fnLo.endsWith("_calibrated.mzml") || fnLo.endsWith("_uncalibrated.mzml") || fnLo.endsWith("_calibrated.mgf") || fnLo.endsWith("_uncalibrated.mgf")) {
            return false;
          } else {
            return true;
          }
        }).toList();

    if (!accepted.isEmpty()) {
      Bus.post(new MessageLcmsFilesAdded(lessGenerated, inputPaths.get(0)));
    }
  }

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void on(MessageLcmsRemoveSelected m) {
    logObjectType(m);

    final List<InputLcmsFile> toRemove = new ArrayList<>();
    Arrays.stream(this.tableRawFiles.getSelectedRows())
        .map(tableRawFiles::convertRowIndexToModel)
        .boxed()
        .forEach(i -> toRemove.add(tableModelRawFiles.dataGet(i)));
    tableRawFiles.getSelectionModel().clearSelection();

    if (!toRemove.isEmpty()) {
      tableModelRawFiles.dataRemoveAll(toRemove);
      postFileListUpdate();
      adjustToolsBasedOnDataTypes();
    }
  }

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void on(MessageLcmsClearFiles m) {
    logObjectType(m);
    tableModelRawFiles.dataClear();
    postFileListUpdate();
    adjustToolsBasedOnDataTypes();
  }

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void on(MessageLcmsFilesList m) {
    if (m.type == MessageType.REQUEST) {
      Bus.post(new MessageLcmsFilesList(MessageType.RESPONSE, tableModelRawFiles.dataCopy()));
    }
  }

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void on(MessageSaveAsWorkflow m) throws IOException {
    Fragpipe fp0 = getStickyStrict(Fragpipe.class);
    final javax.swing.JFrame fp = fp0.toJFrame();
    Properties uiProps = FragpipeCacheUtils.tabsSave0(fp0.tabs, m.saveWithFieldTypes);

    FileNameEndingFilter workflowEndingFilter = new FileNameEndingFilter("workflow files", workflowExt);

    Path saveDir;
    Path savePath = null;
    Path fpWorkflowsDir = FragpipeLocations.get().getDirWorkflows();
    if (!m.toCustomDir) {
      saveDir = fpWorkflowsDir;
    } else {
      // save to custom dir. Allow direct selection of file path to save
      final String propWorkflowDir = "workflow.last-save-dir";
      savePath = getSaveFilePath(null, propWorkflowDir, workflowEndingFilter, workflowExt, false, this);
      if (savePath == null) {
        return;
      }
      saveDir = savePath.toAbsolutePath().getParent();
      propsVarSet(PROP_WORKFLOW_SAVEDIR, saveDir.toString());
    }

    // ask about name and description
    String curName = savePath == null? (String)uiComboWorkflows.getSelectedItem() : savePath.getFileName().toString();
    String curDesc = epWorkflowsDesc.getText();

    MigUtils mu = MigUtils.get();
    final JPanel p = mu.newPanel(null, true);
    UiText uiTextName = UiUtils.uiTextBuilder().cols(20).ghost("Name is required").text(curName).create();
    uiTextName.setName("file-name");
    final HtmlStyledJEditorPane ep = new HtmlStyledJEditorPane();
    ep.setBackground(Color.WHITE);
    ep.setBorder(new LineBorder(Color.LIGHT_GRAY, 1));
    ep.setText(curDesc);
    ep.setPreferredSize(new Dimension(320, 240));
    ep.setName("file-desc");
    if (savePath == null) {
      mu.add(p, new JLabel("Name")).split();
      mu.add(p, uiTextName).growX().wrap();
    }
    mu.add(p, new JLabel("Description (optional)")).wrap();
    mu.add(p, ep).spanX().wrap();

    while (true) {
      int answer = SwingUtils.showConfirmDialog(fp, p, "Assign workflow description");
      if (JOptionPane.OK_OPTION != answer) {
        return;
      }
      String text = uiTextName.getNonGhostText().trim();
      if (StringUtils.isBlank(text)) {
        SwingUtils.showErrorDialog(fp, "Workflow name can't be left empty", "Error saving workflow");
        return;
      }
      final String fn = StringUtils.appendOnce(text, workflowExt);
      if (saveDir.equals(fpWorkflowsDir) && builtInWorkflows.stream().anyMatch(path -> path.equalsIgnoreCase(text))) {
        SwingUtils.showInfoDialog(this,
            "Name can't be the same as one of default ones",
            "Please choose another file name");
        continue;
      }
      try {
        savePath = saveDir.resolve(fn).normalize().toAbsolutePath();
      } catch (Exception e) {
        SwingUtils.showErrorDialog(fp, "Not a valid path", "Error saving workflow");
        continue;
      }
      break;
    }

    if (PathUtils.existing(savePath.toString()) != null) {
      int ans = SwingUtils.showConfirmDialog(fp, new JLabel(SwingUtils.makeHtml("Overwrite existing file?\n" + savePath)), "Overwrite?");
      if (JOptionPane.OK_OPTION != ans) {
        log.debug("user chose not to overwrite file");
        return;
      }
    }

    // save workflow
    String desc = SwingUtils.tryExtractHtmlBody(ep.getText());
    saveWorkflow(savePath, desc, uiProps);
    SwingUtils.showInfoDialog(fp, "Saved to: " + savePath, "Workflow saved");

    epWorkflowsDesc.setText(desc);

    if (FragpipeLocations.get().getDirWorkflows().equals(saveDir)) {
      Bus.post(new MessageUpdateWorkflows());
    }
  }

  public static void saveWorkflow(Path savePath, String desc, Properties props) {
    Map<String, String> vetted = Seq.seq(PropertiesUtils.toMap(props)).filter(kv -> filterPropsForWorkflow(kv.v1())).toMap(kv -> kv.v1, kv -> kv.v2);
    if (StringUtils.isNotBlank(desc)) {
      vetted.put(PROP_WORKFLOW_DESC, desc);
    }
    vetted.put(PROP_WORKFLOW_SAVED_WITH_VER, Version.version());
    FragpipeCacheUtils.saveToFileSorted(PropertiesUtils.from(vetted), savePath, "Workflow: " + StringUtils.upToLastDot(savePath.getFileName().toString()));
  }

  public static boolean filterPropsForWorkflow(final String k0) {
    final String k = k0.toLowerCase();
    return filterPropsForUi(k) && !k.contentEquals("workdir") && !k.endsWith(".ram") && !k.endsWith(".threads") && !k.startsWith(TabConfig.TAB_PREFIX);
  }

  public static boolean filterPropsForUi(final String k0) {
    final String k = k0.toLowerCase();
    if (k.contains("workflow-option")) {
      return true;
    }
    return !k.contains(PROP_NOCACHE);
  }

  private List<String> createNamesForWorkflowsCombo(Map<String, PropsFile> fileMap) {
    final Set<String> builtInWorkflowsCurrent = new HashSet<>(fileMap.keySet());
    builtInWorkflowsCurrent.retainAll(builtInWorkflows);
    builtInWorkflowsCurrent.removeAll(builtInWorkflowsPinned);
    final Set<String> userWorkflows = new HashSet<>(fileMap.keySet());
    userWorkflows.removeAll(builtInWorkflows);
    return Seq.seq(userWorkflows).sorted()
            .append(builtInWorkflowsPinned)
            .append(Seq.seq(builtInWorkflowsCurrent).sorted())
            .toList();
  }

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void on(MessageUpdateWorkflows m) {
    String previouslySelected = (String) uiComboWorkflows.getSelectedItem();
    List<String> previousOptions = new ArrayList<>();
    for (int i = 0; i < uiComboWorkflows.getModel().getSize(); i++) {
      previousOptions.add(uiComboWorkflows.getModel().getElementAt(i));
    }
    log.debug("Previously selected: {}, Previous options: {}", previouslySelected, previousOptions);

    workflows = loadWorkflowFiles();
    List<String> names = createNamesForWorkflowsCombo(workflows);
    uiComboWorkflows.setModel(new DefaultComboBoxModel<>(names.toArray(new String[0])));
    Optional<String> newName = names.stream().filter(name -> !previousOptions.contains(name))
        .findFirst();
    if (newName.isPresent()) {
      log.debug("Figured that '{}' workflow was added", newName.get());
      uiComboWorkflows.setSelectedItem(newName.get());
    } else {
      log.debug("Cant figure which workflow name was added. Use the previously selected one.");
      uiComboWorkflows.setSelectedItem(previouslySelected);
    }
  }

  private void postFileListUpdate() {
    ArrayList<InputLcmsFile> data = tableModelRawFiles.dataCopy();
    if (!data.isEmpty()) {
      ThisAppProps.save(ThisAppProps.PROP_LCMS_FILES_IN, data.get(0).getPath().toString());
    }
    Bus.post(new MessageLcmsFilesList(MessageType.UPDATE, data));
  }

  private JPanel createPanelLcmsFiles() {
    JPanel p = mu.newPanel("Input LC-MS Files", true);

    btnGroupMsType = new ButtonGroup();
    btnTypeRegularMs = new JRadioButton("Regular MS");
    btnTypeRegularMs.setName("input.data-type.regular-ms");
    btnTypeRegularMs.setSelected(true);
    btnTypeIms = new JRadioButton("IM-MS (ion mobility, timsTOF only)");
    btnTypeIms.setName("input.data-type.im-ms");
    btnTypeIms.setSelected(false);
    btnGroupMsType.add(btnTypeRegularMs);
    btnGroupMsType.add(btnTypeIms);

    JButton btnFilesAddFiles = button("Add files", MessageLcmsAddFiles::new);
    JButton btnFilesAddFolder = button("Add folder recursively", MessageLcmsAddFolder::new);
    btnFilesRemove = button("Remove selected files", MessageLcmsRemoveSelected::new);
    btnFilesRemove.setEnabled(false);
    btnFilesClear = button("Clear files", MessageLcmsClearFiles::new);
    btnFilesClear.setEnabled(false);
    JLabel labelDataType = new JLabel("MS data type");

    Font font = labelDataType.getFont();
    Font fontBigger = font.deriveFont(font.getSize2D() * 1.2f);
    labelDataType.setFont(fontBigger);
    btnTypeRegularMs.setFont(fontBigger);
    btnTypeIms.setFont(fontBigger);

    mu.add(p, labelDataType).split().spanX();
    mu.add(p, btnTypeRegularMs);
    mu.add(p, btnTypeIms).wrap();

    btnGroupsConsecutiveExperiment = button("Consecutive",
        () -> new MessageLcmsGroupAction(Type.CONSECUTIVE_EXP));
    btnGroupsConsecutiveBioreplicate = button("Consecutive",
        () -> new MessageLcmsGroupAction(Type.CONSECUTIVE_REP));
    btnGroupsConsecutiveBioreplicateByExperiment = button("Consecutive by experiment",
        () -> new MessageLcmsGroupAction(Type.CONSECUTIVE_REP_BY_EXP));
    btnGroupsByParentDir = button("By parent directory",
        () -> new MessageLcmsGroupAction(Type.BY_PARENT_DIR));
    btnGroupsByFilename = button("By file name",
        () -> new MessageLcmsGroupAction(Type.BY_FILE_NAME));
    btnSetExp = button("Custom",
        () -> new MessageLcmsGroupAction(Type.SET_EXP));
    btnSetRep = button("Custom",
        () -> new MessageLcmsGroupAction(Type.SET_REP));
    btnSetDda = button("Set DDA",
        () -> new MessageLcmsGroupAction(Type.SET_DDA));
    btnSetDia = button("Set DIA",
        () -> new MessageLcmsGroupAction(Type.SET_DIA));
    btnSetGpfDia = button("Set GPF-DIA",
        () -> new MessageLcmsGroupAction(Type.SET_GPF_DIA));
    btnSetDiaQuant = button("Set DIA-Quant",
        () -> new MessageLcmsGroupAction(Type.SET_DIA_QUANT));
    btnSetDiaLib = button("Set DIA-Lib",
        () -> new MessageLcmsGroupAction(Type.SET_DIA_LIB));
    btnSetDdaPlus = button("Set DDA+",
        () -> new MessageLcmsGroupAction(Type.SET_DDA_PLUS));
    btnGroupsClearExperiment = button("Clear", () -> new MessageLcmsGroupAction(Type.CLEAR_EXP));
    btnGroupsClearBioreplicate = button("Clear", () -> new MessageLcmsGroupAction(Type.CLEAR_REP));

    btnManifestSave = button("Save as manifest", MessageManifestSave::new);
    btnManifestLoad = button("Load manifest", MessageManifestLoad::new);

    numSelectedFilesLabel = new JLabel();

    createFileTable();

    mu.add(p, btnFilesAddFiles).split();
    mu.add(p, btnFilesAddFolder);
    mu.add(p, btnFilesRemove);

    final boolean addRecentButton = true;
    if (!addRecentButton) {
      mu.add(p, btnFilesClear).wrap();
    } else {
      uiTextLastAddedLcmsDir = UiUtils.uiTextBuilder().cols(20)
          .text(propsVarGet(ThisAppProps.LAST_RECURSIVE_FOLDER_ADDED, "")).create();
      JButton btnDebugFolderAdd = UiUtils.createButton("Add recent", e -> {
        //String add = "D:\\ms-data\\TMTIntegrator_v1.1.4\\TMT-I-Test\\tmti-test-data_5-min-cuts";
        Path existing = PathUtils.existing(uiTextLastAddedLcmsDir.getNonGhostText());
        if (existing == null) {
          SwingUtils.showInfoDialog(this, "Path does not exist:\n" + uiTextLastAddedLcmsDir.getNonGhostText(), "Warning");
        } else {
          Bus.post(new MessageLcmsAddFolder(Seq.of(existing).toList()));
        }
      });
      //btnDebugFolderAdd.setBackground(Color.PINK);
      mu.add(p, btnFilesClear);
      mu.add(p, btnDebugFolderAdd).gapLeft("20px");
      mu.add(p, uiTextLastAddedLcmsDir).growX().pushX().wrap();
    }

    mu.add(p, btnManifestSave).split();
    mu.add(p, btnManifestLoad).split().wrap();


    JLabel x = new JLabel("Assign files to Experiments/Bioreplicates:");
    Font font2 = x.getFont();
    Font fontBigger2 = font2.deriveFont(font2.getSize2D() * 1.2f);
    x.setFont(fontBigger2);

    mu.add(p, x).spanX().wrap();

    mu.add(p, new JLabel("Set experiments")).split();
    mu.add(p, btnGroupsConsecutiveExperiment);
    mu.add(p, btnGroupsByParentDir);
    mu.add(p, btnGroupsByFilename);
    mu.add(p, btnSetExp);
    mu.add(p, btnGroupsClearExperiment);

    UiText emptySpace = UiUtils.uiTextBuilder().cols(1).text("").create();
    emptySpace.setVisible(false);
    mu.add(p, emptySpace).growX().pushX();

    mu.add(p, btnSetDda);
    mu.add(p, btnSetDdaPlus);
    mu.add(p, btnSetDia).wrap();

    mu.add(p, new JLabel("Set bioreplicates")).split();
    mu.add(p, btnGroupsConsecutiveBioreplicate);
    mu.add(p, btnGroupsConsecutiveBioreplicateByExperiment);
    mu.add(p, btnSetRep);
    mu.add(p, btnGroupsClearBioreplicate);

    UiText emptySpace2 = UiUtils.uiTextBuilder().cols(1).text("").create();
    emptySpace2.setVisible(false);
    mu.add(p, emptySpace2).growX().pushX().split();
    mu.add(p, btnSetDiaQuant);
    mu.add(p, btnSetDiaLib);
    mu.add(p, btnSetGpfDia).wrap();
    mu.add(p, numSelectedFilesLabel).grow().pushX().wrap();


    p.add(scrollPaneRawFiles, mu.ccGx().wrap());

    return p;
  }

  private JButton button(String text, Supplier<Object> message) {
    return UiUtils.createButton(text, e -> Bus.post(message.get()));
  }

  private void createFileTable() {
    tableModelRawFiles = createTableModelRawFiles();
    tableModelRawFiles.addTableModelListener(e -> {
      List<InputLcmsFile> files = tableModelRawFiles.dataCopy();
      Bus.post(new MessageLcmsFilesList(MessageType.UPDATE, files));
    });
    tableRawFiles = new LcmsInputFileTable(tableModelRawFiles);
    tableRawFiles.addComponentsEnabledOnNonEmptyData(btnFilesClear);
    tableRawFiles.addComponentsEnabledOnNonEmptyData(btnGroupsConsecutiveExperiment);
    tableRawFiles.addComponentsEnabledOnNonEmptyData(btnGroupsConsecutiveBioreplicate);
    tableRawFiles.addComponentsEnabledOnNonEmptyData(btnGroupsConsecutiveBioreplicateByExperiment);
    tableRawFiles.addComponentsEnabledOnNonEmptyData(btnGroupsByParentDir);
    tableRawFiles.addComponentsEnabledOnNonEmptyData(btnGroupsByFilename);
    tableRawFiles.addComponentsEnabledOnNonEmptyData(btnSetDda);
    tableRawFiles.addComponentsEnabledOnNonEmptyData(btnSetDia);
    tableRawFiles.addComponentsEnabledOnNonEmptyData(btnSetGpfDia);
    tableRawFiles.addComponentsEnabledOnNonEmptyData(btnSetDiaQuant);
    tableRawFiles.addComponentsEnabledOnNonEmptyData(btnSetDiaLib);
    tableRawFiles.addComponentsEnabledOnNonEmptyData(btnSetDdaPlus);
    tableRawFiles.addComponentsEnabledOnNonEmptyData(btnGroupsClearExperiment);
    tableRawFiles.addComponentsEnabledOnNonEmptyData(btnGroupsClearBioreplicate);
    tableRawFiles.getSelectionModel().addListSelectionListener(e -> {
      if (!e.getValueIsAdjusting()) { // This check ensures we handle only the final event in a series
        updateSelectedFilesTextField();
      }
    });
    updateSelectedFilesTextField();
    tableRawFiles.addComponentsEnabledOnNonEmptySelection(btnFilesRemove);
    tableRawFiles.addComponentsEnabledOnNonEmptySelection(btnSetExp);
    tableRawFiles.addComponentsEnabledOnNonEmptySelection(btnSetRep);
    tableRawFiles.fireInitialization();
    tableRawFiles.setFillsViewportHeight(true);
    tableRawFiles.setComponentPopupMenu(createLcmsTablePopup(tableRawFiles, tableModelRawFiles));
    scrollPaneRawFiles = new JScrollPane();
    scrollPaneRawFiles.setViewportView(tableRawFiles);
  }

  private JPopupMenu createLcmsTablePopup(final JTable table, UniqueLcmsFilesTableModel tableModelRawFiles) {
    BiFunction<String, ActionListener, JMenuItem> make = (s, l) -> {
      JMenuItem item = new JMenuItem(s);
      item.addActionListener(l);
      return item;
    };

    final JPopupMenu pop = new JPopupMenu();
    pop.add(make.apply("Remove Selected", e -> Bus.post(new MessageLcmsRemoveSelected())));
    pop.add(make.apply("Remove All", e -> Bus.post(new MessageLcmsClearFiles())));
    pop.add(make.apply("Add folder", e -> Bus.post(new MessageLcmsAddFolder())));
    pop.add(make.apply("Add files", e -> Bus.post(new MessageLcmsAddFiles())));
    pop.add(make.apply("Open in file manager", e -> {
      SwingUtilities.invokeLater(() -> {
        Point pointRel = SwingUtilities.convertPoint(pop, new Point(0, 0), table);
        log.debug("Attempt to get point rel: {}", pointRel);
        log.debug("Attempt to get point rel row: {}", table.rowAtPoint(pointRel));

        int indexTable = table.getSelectedRow();
        int indexModel = table.convertRowIndexToModel(indexTable);
        Path path = tableModelRawFiles.dataGet(indexModel).getPath();
        log.debug("User clicked on row #{} in table, #{} in mode,, mapping to file: {}", indexTable, indexModel, path);

        Path openLoc = path;
        while (openLoc != null && !Files.isDirectory(openLoc)) {
          openLoc = openLoc.toAbsolutePath().getParent();
        }
        if (openLoc == null) {
          SwingUtils.showInfoDialog(TabWorkflow.this, "Could not locate parent directory to open", "Error opening in file manager");
          return;
        }
        log.debug("Trying to open location in explorer: {}", openLoc);

        Bus.post(new MessageOpenInExplorer(openLoc));
      });
    }));

    final PopupMenuListener popListener = new PopupMenuListener() {
      @Override
      public void popupMenuWillBecomeVisible(PopupMenuEvent e) {
        SwingUtilities.invokeLater(() -> {
          Point pointRel = SwingUtilities.convertPoint(pop, new Point(0, 0), table);
          int rowAtPoint = table.rowAtPoint(pointRel);
          if (rowAtPoint > -1) {
            table.addRowSelectionInterval(rowAtPoint, rowAtPoint);
          }
        });
      }

      @Override
      public void popupMenuWillBecomeInvisible(PopupMenuEvent e) {
      }

      @Override
      public void popupMenuCanceled(PopupMenuEvent e) {
      }
    };
    pop.addPopupMenuListener(popListener);

    return pop;
  }

  private FileDrop makeFileDrop() {
    return new FileDrop(this, true, files -> {
      Predicate<File> pred = CmdMsfragger.getSupportedFilePredicate();
      List<Path> accepted = new ArrayList<>(files.length);
      for (File f : files) {
        PathUtils.traverseDirectoriesAcceptingFiles(f, pred, accepted, false);
      }
      if (!accepted.isEmpty()) {
        Bus.post(new MessageLcmsFilesAdded(accepted));
      }
    });
  }

  private void sdrfSave(Path path, SDRFtable.SDRFtypes type, QuantLabel label, ArrayList<String> enzymes, ArrayList<String> mods, String precTol, String prodTol, List<String> proteinHeaders, Map<String, String> instrumentMap) throws IOException {
    ArrayList<InputLcmsFile> files = tableModelRawFiles.dataCopy();
    SDRFtable table = new SDRFtable(type, enzymes.size(), mods.size(), proteinHeaders);

    for (InputLcmsFile file : files) {
      if (label != null) {
        table.addSampleTMT(file.getPath().getFileName().toString(),
                file.getReplicate() != null ? file.getReplicate().toString() : "",
                enzymes, mods, label, precTol, prodTol, instrumentMap.get(file.getPath().getFileName().toString()));
      } else {
        table.addSampleLFQ(file.getPath().getFileName().toString(),
                file.getReplicate() != null ? file.getReplicate().toString() : "",
                enzymes, mods, precTol, prodTol, instrumentMap.get(file.getPath().getFileName().toString().trim()));
      }
    }

    table.printTable(path);
  }

  public void manifestSave(Path path) throws IOException {
    ArrayList<InputLcmsFile> files = tableModelRawFiles.dataCopy();
    String manifest = files.stream().map(f -> String.format("%s\t%s\t%s\t%s",
        f.getPath().toAbsolutePath().normalize(),
        (f.getExperiment() != null ? f.getExperiment() : ""),
        (f.getReplicate() != null ? f.getReplicate().toString() : ""),
        (f.getDataType() != null ? f.getDataType() : "")))
    .collect(Collectors.joining("\n"));
    FileUtils.write(path.toFile(), manifest, StandardCharsets.UTF_8, false);
  }

  private void manifestLoad(Path manifestPath) throws IOException {
    ArrayList<InputLcmsFile> inTable = tableModelRawFiles.dataCopy();
    List<String> lines = Files.readAllLines(manifestPath, StandardCharsets.UTF_8);
    ConcurrentLinkedDeque<String> badLines = new ConcurrentLinkedDeque<>();
    List<InputLcmsFile> loaded = lines.stream()
        .filter(StringUtils::isNotBlank)
        .filter(line -> !line.startsWith("//") && !line.startsWith("#"))
        .map(line -> {
          String[] split = line.trim().split("\t");
          Path p = null;
          String exp = null;
          Integer replicate = null;
          String dataType = null;
          try {
            if (split.length >= 1) {
              p = Paths.get(split[0]);
            }
            if (split.length >= 2) {
              exp = (split[1] == null || split[1].trim().isEmpty()) ? null : split[1].trim();
            }
            if (split.length >= 3) {
              replicate = (split[2] == null || split[2].trim().isEmpty()) ? null : Integer.parseInt(split[2]);
            }
            if (split.length >= 4) {
              dataType = (split[3] == null || split[3].trim().isEmpty()) ? null : split[3].trim();
            }
          } catch (Exception e) {
            badLines.add(line);
            return null;
          }
          return new InputLcmsFile(p, exp, replicate, dataType);
        }).filter(Objects::nonNull)
        .collect(Collectors.toList());

    if (!badLines.isEmpty()) {
      SwingUtils.showWarningDialog(this,
          "Manifest file contained some badly formatted lines\n\n" +
          Seq.seq(badLines).toString("\n"), "Malformed manifest");
    }

    List<Path> notExist = loaded.stream().map(InputLcmsFile::getPath).filter(p -> !Files.exists(p))
        .collect(Collectors.toList());
    Set<Path> inTablePaths = inTable.stream().map(InputLcmsFile::getPath).collect(Collectors.toSet());
    if (inTable.isEmpty()) {
      showSkippedFiles(notExist);
      tableModelRawFiles.dataAddAll(Seq.seq(loaded).filter(f -> !notExist.contains(f.getPath())).toList());
    } else {
      Set<Path> addedPaths = loaded.stream().map(f -> f.getPath().getFileName()).collect(Collectors.toSet());
      boolean hasMatchingPaths = inTable.stream().map(f -> f.getPath().getFileName()).anyMatch(addedPaths::contains);
      if (hasMatchingPaths) {
        String[] choices = {"Keep only from file", "Append new from file", "Keep current, but update exp/replicates"};
        String message = "Looks like the manifest you're adding contains\n"
            + "file names matching files already in the list.\n\n"
            + "What would you like to do with entries from the manifest?";
        int choice = SwingUtils.showChoiceDialog(this, "Action choice", message, choices, 0);
        if (choice == 0) {
          tableModelRawFiles.dataClear();
          showSkippedFiles(notExist);
          tableModelRawFiles.dataAddAll(Seq.seq(loaded).filter(f -> !notExist.contains(f.getPath())).toList());
        } else if (choice == 1) {
          showSkippedFiles(notExist);
          tableModelRawFiles.dataAddAll(Seq.seq(loaded)
              .filter(f -> !notExist.contains(f.getPath()))
              .filter(f -> !inTablePaths.contains(f.getPath()))
              .toList());
        } else {
          tableModelRawFiles.dataClear();
          List<InputLcmsFile> updated = new ArrayList<>();
          for (InputLcmsFile existing : inTable) {
            InputLcmsFile toCopyFrom = loaded.stream()
                .filter(f -> f.getPath().getFileName().equals(existing.getPath().getFileName()))
                .findFirst().orElse(existing);
            updated.add(new InputLcmsFile(existing.getPath(), toCopyFrom.getExperiment(), toCopyFrom.getReplicate(), toCopyFrom.getDataType()));
          }
          tableModelRawFiles.dataAddAll(updated);
        }
      } else {
        showSkippedFiles(notExist);
        tableModelRawFiles.dataAddAll(Seq.seq(loaded)
            .filter(f -> !notExist.contains(f.getPath()))
            .filter(f -> !inTablePaths.contains(f.getPath()))
            .toList());
      }
    }
  }

  private void showSkippedFiles(List<Path> skipped) {
    if (skipped.isEmpty())
      return;
    MigUtils mu = MigUtils.get();
    JPanel panel = mu.newPanel(new LC().fill());
    List<List<Path>> data = skipped.stream().map(Collections::singletonList)
        .collect(Collectors.toList());

    if (Fragpipe.headless) {
      log.error("Some loaded files don't exist. Please double check.");
      for (List<Path> paths : data) {
        for (Path p : paths) {
          log.error(p.toAbsolutePath().normalize().toString());
        }
      }
      System.exit(1);
    } else {
      JLabel label = SwingUtils.htmlLabel("Some loaded files don't exist. Will be skipped.");
      JScrollPane scroll = SwingUtils
          .wrapInScroll(SwingUtils.tableFromData(Arrays.asList("Paths not exist"), data));

      mu.add(panel, label).growX().wrap();
      mu.add(panel, scroll).growX().wrap();
      SwingUtils.showDialog(this, panel);
    }
  }

  private static final FileNameEndingFilter fileNameEndingFilter = new FileNameEndingFilter("Fragpipe manifest", manifestExt);

  @Subscribe(threadMode = ThreadMode.POSTING)
  public void on(MessageManifestSave m) {
    Path path = getSaveFilePath(m.path, ThisAppProps.CONFIG_SAVE_LOCATION, fileNameEndingFilter, manifestExt, m.quite, this);

    if (path != null) {
      propsVarSet(ThisAppProps.CONFIG_SAVE_LOCATION, path.toAbsolutePath().getParent().toString());
      try {
        manifestSave(path);
      } catch (IOException e) {
        SwingUtils.showErrorDialogWithStacktrace(e, this);
      }
    }
  }

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void on(MessageManifestLoad m) {
    String loc = propsVarGet(ThisAppProps.CONFIG_SAVE_LOCATION);
    JFileChooser fc = FileChooserUtils.builder("Load manifest").paths(Stream.of(loc)).mode(FcMode.ANY).approveButton("Load").multi(false).acceptAll(true).filters(Collections.singletonList(fileNameEndingFilter)).create();
    fc.setFileFilter(fileNameEndingFilter);
    if (Fragpipe.headless || JFileChooser.APPROVE_OPTION == fc.showOpenDialog(this)) {
      final File f = Fragpipe.headless ? Fragpipe.manifestFile.toFile() : fc.getSelectedFile();
      if (f == null)
        return;
      try {
        propsVarSet(ThisAppProps.CONFIG_SAVE_LOCATION, f.getAbsoluteFile().getParent());
        manifestLoad(f.toPath());
        adjustToolsBasedOnDataTypes();
      } catch (IOException e) {
        SwingUtils.showErrorDialogWithStacktrace(e, this);
      }
      Fragpipe.loadManifestDone.countDown();
    }
  }

  @Subscribe(threadMode = ThreadMode.POSTING)
  public void on(MessageSDRFsave m) {
    Path path = getSaveFilePath(m.path, ThisAppProps.CONFIG_SAVE_LOCATION, fileNameEndingFilter, sdrfExt, m.quiet, this);

    if (path != null) {
      propsVarSet(ThisAppProps.CONFIG_SAVE_LOCATION, path.toAbsolutePath().getParent().toString());
      TabMsfragger tabMsfragger = getStickyStrict(TabMsfragger.class);
      try {
        Map<String, String> instrumentMap = TabMsfragger.getInstrumentMap(m.logText);
        sdrfSave(path, m.type, m.label, tabMsfragger.getSDRFenzymes(), tabMsfragger.getSDRFmods(), tabMsfragger.getPrecTolString(), tabMsfragger.getProdTolString(m.logText), m.proteinHeaders, instrumentMap);
      } catch (IOException e) {
        SwingUtils.showErrorDialogWithStacktrace(e, this);
      }
    }
  }

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void on(MessageLcmsGroupAction m) {
    log.debug("Got MessageLcmsGroupAction of type: {}", m.type.toString());

    switch (m.type) {

      case CONSECUTIVE_EXP:
        this.actionConsecutiveExperiment();
        break;
      case CONSECUTIVE_REP:
        this.actionConsecutiveBioreplicate();
        break;
      case CONSECUTIVE_REP_BY_EXP:
        this.actionConsecutiveBioreplicateByExperiment();
        break;
      case BY_PARENT_DIR:
        this.actionByParentDir();
        break;
      case BY_FILE_NAME:
        this.actionByFileName();
        break;
      case SET_EXP:
        this.actionSetExt();
        break;
      case SET_REP:
        this.actionSetRep();
        break;
      case SET_DDA:
        this.actionSetDataType("DDA");
        break;
      case SET_DIA:
        this.actionSetDataType("DIA");
        break;
      case SET_GPF_DIA:
        this.actionSetDataType("GPF-DIA");
        break;
      case SET_DIA_QUANT:
        this.actionSetDataType("DIA-Quant");
        break;
      case SET_DIA_LIB:
        this.actionSetDataType("DIA-Lib");
        break;
      case SET_DDA_PLUS:
        this.actionSetDataType("DDA+");
        break;
      case CLEAR_EXP:
        this.actionClearGroupsExperiment();
        break;
      case CLEAR_REP:
        this.actionClearGroupsBioreplicate();
        break;
      default:
        throw new IllegalStateException("Unknown enum option: " + m.type);
    }

    postFileListUpdate();
    adjustToolsBasedOnDataTypes();
  }

  private void adjustToolsBasedOnDataTypes() {
    final NoteConfigDiann noteConfigDiann = getStickyStrict(NoteConfigDiann.class);

    if (hasDataType("DIA") || hasDataType("DIA-Lib") || hasDataType("GPF-DIA")) {
      Bus.post(new NoteConfigUmpire(true));
      UmpirePanel umpirePanel = getStickyStrict(UmpirePanel.class);
      DiaTracerPanel diaTracerPanel = getStickyStrict(DiaTracerPanel.class);
      if (umpirePanel.isRun() || diaTracerPanel.isRun()) {
        Bus.post(new NoteConfigCrystalC(true));
        Bus.post(new NoteConfigPeptideProphet(true));
      } else {
        Bus.post(new NoteConfigCrystalC(false));
        Bus.post(new NoteConfigPeptideProphet(false));
      }
      Bus.post(new NoteConfigTmtI(false));
      if (hasDataType("DIA")) {
        Bus.post(new NoteConfigDiann(noteConfigDiann, true));
      } else {
        Bus.post(new NoteConfigDiann(noteConfigDiann, false));
      }
    } else if (hasDataType("DDA+")) {
      Bus.post(new NoteConfigUmpire(false));
      Bus.post(new NoteConfigCrystalC(false));
      Bus.post(new NoteConfigPeptideProphet(false));
      Bus.post(new NoteConfigTmtI(false));
      Bus.post(new NoteConfigDiann(noteConfigDiann, false));
    } else {
      Bus.post(new NoteConfigUmpire(false));
      Bus.post(new NoteConfigCrystalC(true));
      Bus.post(new NoteConfigPeptideProphet(true));
      Bus.post(new NoteConfigTmtI(true));
      Bus.post(new NoteConfigDiann(noteConfigDiann, false));
    }

    if (hasDataType("DIA-Quant")) {
      Bus.post(new NoteConfigDiann(noteConfigDiann, true));
    }
  }

  private void actionClearGroupsExperiment() {
    UniqueLcmsFilesTableModel m = this.tableModelRawFiles;

    for (int i = 0, sz = m.dataSize(); i < sz; i++) {
      InputLcmsFile f = m.dataGet(i);
      m.dataSet(i, new InputLcmsFile(f.getPath(), ThisAppProps.DEFAULT_LCMS_EXP_NAME, f.getReplicate(), f.getDataType()));
    }
  }

  private void actionClearGroupsBioreplicate() {
    UniqueLcmsFilesTableModel m = this.tableModelRawFiles;

    for (int i = 0, sz = m.dataSize(); i < sz; i++) {
      InputLcmsFile f = m.dataGet(i);
      m.dataSet(i, new InputLcmsFile(f.getPath(), f.getExperiment(), null, f.getDataType()));
    }
  }

  private void actionSetExt() {
    final UniqueLcmsFilesTableModel m = this.tableModelRawFiles;
    List<Integer> selectedRows = Arrays.stream(this.tableRawFiles.getSelectedRows())
        .mapToObj(tableRawFiles::convertRowIndexToModel).collect(Collectors.toList());

    SetExpDialog setExpDialog = new SetExpDialog(SwingUtils.findParentFrame(this));
    setExpDialog.setVisible(true);
    if (setExpDialog.isOk()) {
      for (int selectedRow : selectedRows) {
        int i = tableRawFiles.convertRowIndexToModel(selectedRow);
        InputLcmsFile f = m.dataGet(i);
        m.dataSet(i, new InputLcmsFile(f.getPath(), setExpDialog.getExperimentName(), f.getReplicate(), f.getDataType()));
      }
    }
  }

  private void actionSetRep() {
    final UniqueLcmsFilesTableModel m = this.tableModelRawFiles;
    List<Integer> selectedRows = Arrays.stream(this.tableRawFiles.getSelectedRows())
        .mapToObj(tableRawFiles::convertRowIndexToModel).collect(Collectors.toList());

    SetRepDialog setRepDialog = new SetRepDialog(SwingUtils.findParentFrame(this));
    setRepDialog.setVisible(true);
    if (setRepDialog.isOk()) {
      for (int selectedRow : selectedRows) {
        int i = tableRawFiles.convertRowIndexToModel(selectedRow);
        InputLcmsFile f = m.dataGet(i);
        m.dataSet(i, new InputLcmsFile(f.getPath(), f.getExperiment(), setRepDialog.getReplicate(), f.getDataType()));
      }
    }
  }

  private void actionSetDataType(String dataType) {
    final UniqueLcmsFilesTableModel m = this.tableModelRawFiles;
    List<Integer> selectedRows = Arrays.stream(this.tableRawFiles.getSelectedRows()).mapToObj(tableRawFiles::convertRowIndexToModel).collect(Collectors.toList());
    if (selectedRows.isEmpty()) {
      for (int i = 0; i < m.dataSize(); ++i) {
        InputLcmsFile f = m.dataGet(i);
        m.dataSet(i, new InputLcmsFile(f.getPath(), f.getExperiment(), f.getReplicate(), dataType));
      }
    } else {
      for (int selectedRow : selectedRows) {
        int i = tableRawFiles.convertRowIndexToModel(selectedRow);
        InputLcmsFile f = m.dataGet(i);
        m.dataSet(i, new InputLcmsFile(f.getPath(), f.getExperiment(), f.getReplicate(), dataType));
      }
    }
  }

  private void actionByFileName() {
    UniqueLcmsFilesTableModel m = this.tableModelRawFiles;

    for (int i = 0, sz = m.dataSize(); i < sz; i++) {
      InputLcmsFile f = m.dataGet(i);
      String group = StringUtils.upToLastDot(f.getPath().getFileName().toString());
      m.dataSet(i, new InputLcmsFile(f.getPath(), group, null, f.getDataType()));
    }
  }

  private void actionByParentDir() {
    UniqueLcmsFilesTableModel m = this.tableModelRawFiles;

    for (int i = 0, sz = m.dataSize(); i < sz; i++) {
      InputLcmsFile f = m.dataGet(i);
      int count = f.getPath().getNameCount();
      String group = count - 2 >= 0
          ? f.getPath().getName(count - 2).toString()
          : f.getPath().getName(count - 1).toString();
      m.dataSet(i, new InputLcmsFile(f.getPath(), group, null, f.getDataType()));
    }
  }

  private void actionConsecutiveExperiment() {
    UniqueLcmsFilesTableModel m = this.tableModelRawFiles;
    for (int i = 0, sz = m.dataSize(); i < sz; i++) {
      InputLcmsFile f = m.dataGet(i);
      m.dataSet(i, new InputLcmsFile(f.getPath(), String.valueOf(i + 1), f.getReplicate(), f.getDataType()));
    }
  }

  private void actionConsecutiveBioreplicate() {
    UniqueLcmsFilesTableModel m = this.tableModelRawFiles;
    for (int i = 0, sz = m.dataSize(); i < sz; i++) {
      InputLcmsFile f = m.dataGet(i);
      m.dataSet(i, new InputLcmsFile(f.getPath(), f.getExperiment(), i + 1, f.getDataType()));
    }
  }

  private void actionConsecutiveBioreplicateByExperiment() {
    UniqueLcmsFilesTableModel m = this.tableModelRawFiles;
    Map<String, Integer> experimentReplicateMap = new HashMap<>();
    for (int i = 0, sz = m.dataSize(); i < sz; i++) {
      InputLcmsFile f = m.dataGet(i);
      String experiment = f.getExperiment();
      Integer replicate = experimentReplicateMap.get(experiment);
      if (replicate == null) {
        replicate = 1;
      } else {
        ++replicate;
      }
      experimentReplicateMap.put(experiment, replicate);
      m.dataSet(i, new InputLcmsFile(f.getPath(), f.getExperiment(), replicate, f.getDataType()));
    }
  }

  public Map<String, LcmsFileGroup> getLcmsFileGroups() {
    List<InputLcmsFile> lcmsInputs = tableModelRawFiles.dataCopy();
    Map<String, List<InputLcmsFile>> mapGroup2Files = lcmsInputs.stream()
        .collect(Collectors.groupingBy(InputLcmsFile::getGroup));

    Map<String, LcmsFileGroup> result = new TreeMap<>();
    for (Entry<String, List<InputLcmsFile>> e : mapGroup2Files.entrySet()) {
      result.put(e.getKey(), new LcmsFileGroup(e.getKey(), e.getValue()));
    }

    return result;
  }

  public Map<String, LcmsFileGroup> getLcmsFileGroups2() {
    List<InputLcmsFile> lcmsInputs = tableModelRawFiles.dataCopy();
    Map<String, List<InputLcmsFile>> mapGroup2Files = lcmsInputs.stream()
        .collect(Collectors.groupingBy(InputLcmsFile::getGroup2));

    Map<String, LcmsFileGroup> result = new TreeMap<>();
    for (Entry<String, List<InputLcmsFile>> e : mapGroup2Files.entrySet()) {
      result.put(e.getKey(), new LcmsFileGroup(e.getKey(), e.getValue()));
    }

    return result;
  }

  public List<InputLcmsFile> getLcmsFiles() {
    return tableModelRawFiles.dataCopy();
  }

  private void updateSelectedFilesTextField() {
    int totalRows = tableRawFiles.getRowCount();
    int selectedRows = tableRawFiles.getSelectedRows().length;
    numSelectedFilesLabel.setText("Total raw files: " + totalRows + ", Selected raw files: " + selectedRows);
  }

  public boolean hasDataType(String dataType) {
    for (InputLcmsFile inputLcmsFile : tableModelRawFiles.dataCopy()) {
      if (inputLcmsFile.getDataType().contentEquals(dataType)) {
        return true;
      }
    }
    return false;
  }

  public static Path getSaveFilePath(Path inputPath, String defaultSaveDir, FileNameEndingFilter filenameFilter, String fileExtension, boolean quiet, Component parent) {
    Path path = inputPath;
    if (path == null) {
      String loc = propsVarGet(defaultSaveDir);
      JFileChooser fc = FileChooserUtils.builder("Path to save file").paths(Stream.of(loc)).mode(FcMode.FILES_ONLY).approveButton("Save").multi(false).acceptAll(true).filters(Collections.singletonList(filenameFilter)).create();
      fc.setFileFilter(filenameFilter);
      if (JFileChooser.APPROVE_OPTION == fc.showSaveDialog(parent)) {
        String s = fc.getSelectedFile().getAbsolutePath();
        if (!s.endsWith(fileExtension)) {
          s += fileExtension;
        }
        path = Paths.get(s);
      }
    }

    if (path != null) {
      propsVarSet(defaultSaveDir, path.toAbsolutePath().getParent().toString());
      try {
        if (quiet) {
          Files.deleteIfExists(path);
        } else if (Files.exists(path)) {
          if (!SwingUtils.showConfirmDialogShort(parent, "File exists, overwrite?\n\n" + path)) {
            return null;
          } else {
            Files.deleteIfExists(path);
          }
        }
        return path;
      } catch (IOException e) {
        SwingUtils.showErrorDialogWithStacktrace(e, parent);
      }
    }
    return null;
  }

  private void actionLoadSelectedWorkflow(ActionEvent e) {
    uiComboWorkflows.hidePopup(); // Make the popup disappear in case it is still open when loading the (custom) workflow
    String workflow = (String) uiComboWorkflows.getSelectedItem();
    log.debug("Load workflow button clicked: {}", workflow);
    if (workflow == null || workflow.equalsIgnoreCase("Custom")) {
      final String propWorkflowDir = "workflow.last-save-dir";
      JFileChooser fc = FileChooserUtils.builder("Select the Workflow file to load").multi(false).mode(FcMode.FILES_ONLY).acceptAll(true).approveButton("Select workflow").paths(Stream.of(propsVarGet(propWorkflowDir), FragpipeLocations.get().getDirWorkflows().toString())).create();
      fc.setFileFilter(new FileNameExtensionFilter("workflow files", "workflow"));
      if (fc.showOpenDialog(this) != JFileChooser.APPROVE_OPTION) {
        log.debug("User cancelled workflow loading");
        return;
      }

      log.debug("Loading workflow/ui state: {}", workflow);
      loadCustomWorkflow(fc.getSelectedFile().toPath(), workflow);
    } else {
      log.debug("Loading workflow/ui state: {}", workflow);
      PropsFile propsFile = workflows.get(workflow);
      if (propsFile == null) {
        SwingUtils.showErrorDialog(this, "Couldn't load workflow file: " + workflow, "Workflow loading error");
        return;
      }

      log.debug("Reloading file from disk, in case it has changed: {}", propsFile.getPath());
      try {
        propsFile.load();
      } catch (IOException ex) {
        SwingUtils.showErrorDialogWithStacktrace(ex, this, true);
        log.error("Error re-loading workflow file", ex);
      }

      if (propsFile.containsKey("workflow.workflow-option")) {
        propsFile.setProperty("workflow.workflow-option", workflow);
      }
      Bus.post(new MessageLoadUi(propsFile, true, false));
    }
  }

  private void loadCustomWorkflow(Path workflowPath, String workflowName) {
    PropsFile propsFile = FragpipeLocations.get().tryLoadSilently(workflowPath, "user");
    if (propsFile == null) {
      SwingUtils.showErrorDialog(this, "Couldn't load workflow file: " + workflowName, "Workflow loading error");
      return;
    }

    if (propsFile.containsKey("workflow.workflow-option")) {
      propsFile.setProperty("workflow.workflow-option", workflowName);
    }

    // Do not load the config paths from the workflow, which likely to be the paths in another user's computer.
    propsFile.remove(TabConfig.TAB_PREFIX + "tools-folder");
    propsFile.remove(TabConfig.TAB_PREFIX + "bin-diann");
    propsFile.remove(TabConfig.TAB_PREFIX + "bin-python");

    epWorkflowsDesc.setText(propsFile.getProperty(PROP_WORKFLOW_DESC, "Description not present"));
    propsVarSet(PROP_WORKFLOW_SAVEDIR, propsFile.getPath().toAbsolutePath().getParent().toString());

    Bus.post(new MessageLoadUi(propsFile, true, false));
  }

  // load all settings from a previous saved job
  private void actionLoadJob(ActionEvent e) {
    // choose and load job file
    FileNameEndingFilter filter = new FileNameEndingFilter("Job file (.job)", "job");
    JFileChooser fc = FileChooserUtils.builder("Select the Job file to load").multi(false).mode(FcMode.FILES_ONLY).approveButton("Select Job").paths(Stream.of(FragpipeLocations.get().getDirJobs().toString())).create();
    fc.setFileFilter(filter);
    if (fc.showOpenDialog(this) != JFileChooser.APPROVE_OPTION) {
      log.debug("User cancelled job loading");
      return;
    }
    Path jobPath = fc.getSelectedFile().toPath();
    List<BatchRun> jobs = TabBatch.parseBatchTemplate(this, jobPath.toAbsolutePath().normalize().toString());
    BatchRun job;
    if (jobs.isEmpty()) {
      SwingUtils.showErrorDialog(this, "No job found in file: " + jobPath, "Job loading error");
      return;
    } else if (jobs.size() > 1) {
      SwingUtils.showWarningDialog(this, "Warning: Multiple jobs found in file: " + jobPath + ". The first job will be loaded.", "Multiple jobs in file");
    }
    job = jobs.get(0);

    // load workflow
    loadCustomWorkflow(job.workflowPath, "Custom");

    // load manifest
    try {
      Bus.post(new MessageLcmsClearFiles());
      manifestLoad(job.manifestPath);
    } catch (IOException ex) {
        SwingUtils.showErrorDialog(this, "IO Error when trying to load manifest file " + job.manifestPath + ": " + ex.getMessage(), "Error Loading Manifest");
        log.error("Error loading manifest file", ex);
        return;
    }

    // load output dir
    TabRun tabRun = getStickyStrict(TabRun.class);
    tabRun.uiTextWorkdir.setText(job.outputPath.toAbsolutePath().normalize().toString());

    // load fasta if provided
    if (job.fastaPath != null) {
      Bus.post(new MessageDbNewPath(job.fastaPath.toString()));
    }

    // load ram and threads
    uiSpinnerRam.setValue(job.ram);
    uiSpinnerThreads.setValue(job.threads);
  }

  public static class LcmsFileAddition {
    public List<Path> paths;
    public List<Path> toAdd;

    public LcmsFileAddition(List<Path> paths, List<Path> toAdd) {
      this.paths = paths;
      this.toAdd = toAdd;
    }
  }
}
