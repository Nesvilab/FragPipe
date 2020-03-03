package umich.msfragger.params.tmtintegrator;

import com.github.chhh.utils.swing.UiCheck;
import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JButton;
import javax.swing.JLabel;
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
import umich.msfragger.gui.renderers.ButtonColumn;
import umich.msfragger.messages.MessageLcmsFilesList;
import umich.msfragger.messages.MessageLoadTmtIntegratorDefaults;
import umich.msfragger.messages.MessageTmtIntegratorRun;
import umich.msfragger.messages.MessageType;
import umich.msfragger.params.tmtintegrator.TmtAnnotationTable.TmtAnnotationRow;
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
  private Action browseAction;
  private ButtonColumn buttonColumn;

  public TmtIntegratorPanel() {
    initMore();
    // register on the bus only after all the components have been created to avoid NPEs
    EventBus.getDefault().register(this);
    initPostCreation();
  }

  private void initPostCreation() {
    EventBus.getDefault().post(new MessageLcmsFilesList(MessageType.REQUEST, null));
  }

  private void initMore() {
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
      browseAction = new AbstractAction()
      {
        public void actionPerformed(ActionEvent e)
        {
          int modelRow = Integer.parseInt( e.getActionCommand() );
          log.debug("Browse action running in TMT, model row: {}", modelRow);
        }
      };
      buttonColumn = new ButtonColumn(tmtAnnotationTable, browseAction, 2);

      pTable.add(new JLabel("TMT Annotations"), BorderLayout.NORTH);
      tmtAnnotationTable.fireInitialization();
      tmtAnnotationTable.setFillsViewportHeight(false);
      scrollPaneTmtTable = new JScrollPane();
      scrollPaneTmtTable.setViewportView(tmtAnnotationTable);

      pTable.add(scrollPaneTmtTable, BorderLayout.CENTER);
      pContent.add(pTable, new CC().growX());
    }

    {
      pOpts = new JPanel(new MigLayout(new LC().debug()));
//      pOpts = new JPanel(new BorderLayout());
      //pOpts.setBorder(new TitledBorder("TMT options"));
      pOpts.setBorder(new EmptyBorder(0, 0, 0, 0));
      UiCheck doX = new UiCheck("Do x", null, false);
      FormEntry feDoX = new FormEntry("ui-name.downstream.tmtintegrator.do-x", "not shown", doX);
      UiCheck doY = new UiCheck("Do y", null, false);
      FormEntry feDoY = new FormEntry("ui-name.downstream.tmtintegrator.do-y", "not shown", doY);
      FormEntry feDoZ = new FormEntry("ui-name.downstream.tmtintegrator.do-z", "not shown",
          new UiCheck("Do z", null, false));
      FormEntry feDoQ = new FormEntry("ui-name.downstream.tmtintegrator.do-Q", "not shown",
          new UiCheck("Do q", null, false));
      pOpts.add(feDoX.comp, new CC().alignX("left"));
      pOpts.add(feDoY.comp, new CC().alignX("left").wrap());
      pOpts.add(feDoZ.comp, new CC().alignX("left"));
      pOpts.add(feDoQ.comp, new CC().alignX("left").wrap());
      pContent.add(pOpts, new CC().alignY("top").growX().wrap());
    }

    this.add(pContent, BorderLayout.CENTER);
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

    final Map<String, TmtAnnotationRow> curRows = tmtAnnotationTable.fetchModel().dataCopy().stream()
        .collect(Collectors.toMap(row -> row.expName, row -> row));
    List<String> expNames = m.files.stream().map(InputLcmsFile::getExperiment)
        .distinct().sorted().collect(Collectors.toList());
    List<TmtAnnotationRow> newRows = m.files.stream().map(InputLcmsFile::getExperiment)
        .distinct().sorted()
        .map(name -> curRows.getOrDefault(name, new TmtAnnotationRow(name, "No path set yet")))
        .collect(Collectors.toList());

    tmtAnnotationTable.fetchModel().dataClear();
    tmtAnnotationTable.fetchModel().dataAddAll(newRows);

  }
}
