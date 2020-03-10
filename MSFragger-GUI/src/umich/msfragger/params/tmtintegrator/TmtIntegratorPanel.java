package umich.msfragger.params.tmtintegrator;

import com.github.chhh.utils.swing.UiCheck;
import com.github.chhh.utils.swing.UiCombo;
import com.github.chhh.utils.swing.UiUtils;
import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.border.EmptyBorder;
import javax.swing.border.TitledBorder;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.greenrobot.eventbus.EventBus;
import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.ThreadMode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import umich.msfragger.gui.InputLcmsFile;
import umich.msfragger.gui.dialogs.QuantLabelAnnotationDialog;
import umich.msfragger.gui.renderers.ButtonColumn;
import umich.msfragger.messages.MessageLcmsFilesList;
import umich.msfragger.messages.MessageLoadTmtIntegratorDefaults;
import umich.msfragger.messages.MessageTmtIntegratorRun;
import umich.msfragger.messages.MessageType;
import umich.msfragger.params.ThisAppProps;
import umich.msfragger.params.tmtintegrator.TmtAnnotationTable.ExpNameToAnnotationFile;
import umich.msfragger.util.StringUtils;
import umich.msfragger.util.SwingUtils;
import umich.msfragger.util.swing.FormEntry;
import umich.msfragger.util.swing.JPanelWithEnablement;

public class TmtIntegratorPanel extends JPanelWithEnablement {
  private static final Logger log = LoggerFactory.getLogger(TmtIntegratorPanel.class);

  private JPanel pTop;
  private UiCheck checkRun;
  private JPanel pContent;
  private JPanel pTable;
  private TmtAnnotationTable tmtAnnotationTable;
  private JScrollPane scrollPaneTmtTable;
  private JPanel pOpts;
  private Action actionBrowse;
  private Action actionCreate;
  private ButtonColumn colBrowse;
  private ButtonColumn colCreate;
  private UiCombo uiComboLabelNames;
  public static final String namePrefix = "ui.tmtintegrator.";

  public TmtIntegratorPanel() {
    init();
    // register on the bus only after all the components have been created to avoid NPEs
    EventBus.getDefault().register(this);
    initPostCreation();
  }

  private void initPostCreation() {
    EventBus.getDefault().post(new MessageLcmsFilesList(MessageType.REQUEST, null));
  }

  private void init() {
    this.setLayout(new BorderLayout());
//    this.setBorder(new EmptyBorder(0,0,0,0));
    this.setBorder(new TitledBorder("TMT Qunatitation"));

    // Top panel with run checkbox
    {
      // setting the insets allows the top panel to be shifted left of the options panel
      pTop = new JPanel(new MigLayout(new LC().insetsAll("0px")));//.debug()));

      checkRun = new UiCheck("Run TMT-Integrator", null, true);
      checkRun.setName("ui.name.downstream.run-tmtintegrator");
      checkRun.addActionListener(e -> {
        final boolean isSelected = checkRun.isSelected();
        enablementMapping.put(pContent, isSelected);
        updateEnabledStatus(pContent, isSelected);
        EventBus.getDefault().post(new MessageTmtIntegratorRun(isSelected));
      });
      pTop.add(checkRun, new CC().alignX("left"));
      JButton btnLoadDefaults = new JButton("Load TMT-Integrator defaults");
      btnLoadDefaults.addActionListener((e) -> EventBus.getDefault().post(new MessageLoadTmtIntegratorDefaults(true)));
      pTop.add(btnLoadDefaults, new CC().alignX("left"));

      pTop.setBorder(new EmptyBorder(0,0,0,0));
      this.add(pTop, BorderLayout.NORTH);
    }

    // Main content panel - container
    {
      pContent = new JPanel(new MigLayout(new LC().fillX()));
      pContent.setBorder(new EmptyBorder(0,0,0,0));

      pContent.addPropertyChangeListener("enabled", evt -> {
        log.debug("Tmt pContent panel property '{}' changed from '{}' to '{}'", evt.getPropertyName(),
            evt.getOldValue(), evt.getNewValue());
        boolean newValue = (Boolean)evt.getNewValue();
        boolean isSwitchToEnabled = (Boolean) evt.getNewValue() && !(Boolean) evt.getOldValue();
        boolean pContentIsEnabled = newValue && checkRun.isSelected();
        log.debug("Tmt pContent panel is switching to enabled? : {}, !checkRun.isSelected() : {}, final state should be: {}",
            isSwitchToEnabled, !checkRun.isSelected(), pContentIsEnabled);
        enablementMapping.put(pContent, pContentIsEnabled);
        updateEnabledStatus(pContent, pContentIsEnabled);
      });

//      scroll = new JScrollPane(pContent);
//      scroll.setBorder(new EmptyBorder(0, 0, 0, 0));
//      scroll.getVerticalScrollBar().setUnitIncrement(16);
    }

    {
      //pTable = new JPanel(new MigLayout(new LC()));
      pTable = new JPanel(new BorderLayout());
      //pPeakPicking.setBorder(new TitledBorder("PTMShepherd options"));
      pTable.setBorder(new EmptyBorder(0, 0, 0, 0));

      tmtAnnotationTable = new TmtAnnotationTable();
      actionBrowse = new AbstractAction()
      {
        public void actionPerformed(ActionEvent e)
        {
          int modelRow = Integer.parseInt( e.getActionCommand() );
          log.debug("Browse action running in TMT, model row number: {}", modelRow);
          ExpNameToAnnotationFile row = tmtAnnotationTable.fetchModel().dataGet(modelRow);
          JFileChooser fc = new JFileChooser();
          fc.setAcceptAllFileFilterUsed(true);
          fc.setMultiSelectionEnabled(false);
          Optional<Path> exisitngFile = Stream
              .of(row.path, ThisAppProps.load(ThisAppProps.PROP_LCMS_FILES_IN))
              .map(path -> {
                try {
                  return Paths.get(path);
                } catch (Exception ignore) {
                  return null;
                }
              })
              .filter(Objects::nonNull)
              .filter(path -> Files.exists(path))
              .findFirst();

          exisitngFile.ifPresent(path -> fc.setSelectedFile(path.toFile()));
          final int answer = fc.showDialog(TmtIntegratorPanel.this.scrollPaneTmtTable, "Select");
          if (JOptionPane.OK_OPTION == answer) {
            File selectedFile = fc.getSelectedFile();
            List<QuantLabelAnnotation> annotations;
            try {
              annotations = parseTmtAnnotationFile(selectedFile);
            } catch (TmtAnnotationValidationException ex) {
              SwingUtils.showErrorDialog(ex, TmtIntegratorPanel.this.scrollPaneTmtTable, false);
              return;
            }

            // do the annotations match anything known to us?
            Optional<QuantLabel> known = QuantLabel.LABELS.stream()
                .filter(ql -> ql.getReagentNames().size() == annotations.size()).findFirst();
            if (!known.isPresent()) throw new IllegalStateException("No known labels match");

            // maybe update selected Label Type (aka number of channels)?
            if (!getSelectedLabel().getName().equalsIgnoreCase(known.get().getName())) {
              int confirmation = SwingUtils.showConfirmDialog(TmtIntegratorPanel.this,
                  new JLabel(String.format("<html>Loaded file looks to be for %s.<br/>\n"
                      + "Load configuration preset for that label?", known.get().getName())));
              if (JOptionPane.OK_OPTION == confirmation) {
                log.debug("User selected to load config preset for known label");
                uiComboLabelNames.setSelectedItem(known.get().getName());
              }
            }

            row.setPath(selectedFile.toString());
            tmtAnnotationTable.fetchModel().fireTableDataChanged();

            // save the path
            ThisAppProps.save(ThisAppProps.PROP_LCMS_FILES_IN, selectedFile);
          }
        }

      };

      actionCreate = new AbstractAction() {
        @Override
        public void actionPerformed(ActionEvent e) {
          int modelRow = Integer.parseInt( e.getActionCommand() );
          log.debug("Create action running in TMT, model row number: {}", modelRow);
          ExpNameToAnnotationFile row = tmtAnnotationTable.fetchModel().dataGet(modelRow);
          try {
            if (!StringUtils.isNullOrWhitespace(row.getPath()) || Files.exists(Paths.get(row.getPath()))) {
              JFrame parent = SwingUtils.findParentFrame(TmtIntegratorPanel.this);
              QuantLabelAnnotationDialog d = new QuantLabelAnnotationDialog(parent, row, 11, Paths.get(row.getPath()));
              d.setVisible(true);
              log.debug("Dialog table model:\n{}", d.getModel().dataCopy().stream().map(qla -> qla.toString()).collect(Collectors.joining("\n")));
            }
          } catch (Exception ignore) {}
        }
      };
      colBrowse = new ButtonColumn(tmtAnnotationTable, actionBrowse, 2);
      colCreate = new ButtonColumn(tmtAnnotationTable, actionCreate, 3);

      pTable.add(new JLabel("TMT Annotations"), BorderLayout.NORTH);
      tmtAnnotationTable.fireInitialization();
      tmtAnnotationTable.setFillsViewportHeight(false);
      scrollPaneTmtTable = new JScrollPane();
      scrollPaneTmtTable.setViewportView(tmtAnnotationTable);

      pTable.add(scrollPaneTmtTable, BorderLayout.CENTER);
      pContent.add(pTable, new CC().growX());
    }

    {
      pOpts = new JPanel(new MigLayout(new LC()));//.debug()));
//      pOpts = new JPanel(new BorderLayout());
      //pOpts.setBorder(new TitledBorder("TMT options"));
      pOpts.setBorder(new EmptyBorder(0, 0, 0, 0));


      uiComboLabelNames = UiUtils.createUiCombo(
          QuantLabel.LABELS.stream().map(QuantLabel::getName).collect(Collectors.toList()));
      FormEntry feType = new FormEntry("ui.tmtintegrator.label-names", "Type", uiComboLabelNames);
      UiCheck doX = new UiCheck("Do x", null, false);
      FormEntry feDoX = new FormEntry("ui.tmtintegrator.do-x", "not shown", doX);
      pOpts.add(feType.label(), new CC().alignX("right"));
      pOpts.add(feType.comp, new CC().alignX("left"));
      pOpts.add(feDoX.comp, new CC().alignX("left").wrap());

      UiCheck doY = new UiCheck("Do y", null, false);
      FormEntry feDoY = new FormEntry("ui-name.downstream.tmtintegrator.do-y", "not shown", doY);
      FormEntry feDoZ = new FormEntry("ui-name.downstream.tmtintegrator.do-z", "not shown",
          new UiCheck("Do z", null, false));
      FormEntry feDoQ = new FormEntry("ui-name.downstream.tmtintegrator.do-q", "not shown",
          new UiCheck("Do q", null, false));

      pOpts.add(feDoY.comp, new CC().alignX("left"));
      pOpts.add(feDoZ.comp, new CC().alignX("left"));
      pOpts.add(feDoQ.comp, new CC().alignX("left").wrap());


      pContent.add(pOpts, new CC().alignY("top").growX().wrap());
    }

    this.add(pContent, BorderLayout.CENTER);
  }

  private QuantLabel getSelectedLabel() {
    String name  = (String) uiComboLabelNames.getSelectedItem();
    Optional<QuantLabel> label = QuantLabel.LABELS.stream()
        .filter(ql -> ql.getName().equalsIgnoreCase(name)).findFirst();
    if (!label.isPresent()) {
      throw new IllegalStateException("Unknown label name in combo box: " + name);
    }
    return label.get();
  }
  
  public static class TmtAnnotationValidationException extends Exception {

    public TmtAnnotationValidationException(String message) {
      super(message);
    }

    public TmtAnnotationValidationException(String message, Throwable cause) {
      super(message, cause);
    }

    public TmtAnnotationValidationException(Throwable cause) {
      super(cause);
    }
  }

  private static List<QuantLabelAnnotation> parseTmtAnnotationFile(File file)
      throws TmtAnnotationValidationException {
    if (file == null || !Files.exists(file.toPath()))
      throw new TmtAnnotationValidationException("File does not exist");
    List<String> lines;
    try {
      lines = Files.readAllLines(file.toPath(), StandardCharsets.UTF_8);
    } catch (IOException e) {
      throw new TmtAnnotationValidationException(e);
    }
    final long nonEmptyLinesCount = lines.stream()
        .filter(line -> !StringUtils.isNullOrWhitespace(line)).count();
    Optional<QuantLabel> label = QuantLabel.LABELS.stream()
        .filter(ql -> ql.getReagentNames().size() == nonEmptyLinesCount).findFirst();
    if (!label.isPresent()) {
      throw new TmtAnnotationValidationException("No known labels with " + nonEmptyLinesCount + " reagents.");
    }

    List<QuantLabelAnnotation> annotations = new ArrayList<>();
    for (int i = 0; i < lines.size(); i++) {
      String line = lines.get(i);
      if (StringUtils.isNullOrWhitespace(line))
        continue;
      String[] split = line.split("[ ]+", 2);
      if (!label.get().getReagentNames().contains(split[0])) {
        throw new TmtAnnotationValidationException(
            String.format("Line %d contains unknown labeling reagent name: %s", i + 1, split[0]));
      }
      annotations.add(new QuantLabelAnnotation(split[0], split[1]));
    }
    return annotations;
  }

  public boolean isRun() {
    return checkRun.isEnabled() && checkRun.isSelected();
  }

  @Subscribe
  public void onMessageTmtIntegratorRun(MessageTmtIntegratorRun m) {
    log.debug("Got MessageRunTmtIntegrator - is run: {}", m.isRun);
  }

  @Subscribe
  public void onMessageLoadTmtIntegratorDefaults(MessageLoadTmtIntegratorDefaults m) {
    log.debug("Got MessageLoadTmtIntegratorDefaults, it's an empty marker message");
  }

  @Subscribe(threadMode =  ThreadMode.MAIN_ORDERED)
  public void onMessageLcmsFilesList(MessageLcmsFilesList m) {
    if (m.type == MessageType.REQUEST)
      return;

    final Map<String, ExpNameToAnnotationFile> curRows = tmtAnnotationTable.fetchModel().dataCopy().stream()
        .collect(Collectors.toMap(row -> row.expName, row -> row));
    List<String> expNames = m.files.stream().map(InputLcmsFile::getExperiment)
        .distinct().sorted().collect(Collectors.toList());
    List<ExpNameToAnnotationFile> newRows = m.files.stream().map(InputLcmsFile::getExperiment)
        .distinct().sorted()
        .map(name -> curRows.getOrDefault(name, new ExpNameToAnnotationFile(name, "No path set yet")))
        .collect(Collectors.toList());

    tmtAnnotationTable.fetchModel().dataClear();
    tmtAnnotationTable.fetchModel().dataAddAll(newRows);

  }
}
