package umich.msfragger.params.tmtintegrator;

import com.github.chhh.utils.swing.UiCheck;
import java.awt.BorderLayout;
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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import umich.msfragger.messages.MessageLoadShepherdDefaults;
import umich.msfragger.messages.MessageLoadTmtIntegratorDefaults;
import umich.msfragger.messages.MessageRunTmtIntegrator;
import umich.msfragger.util.swing.JPanelWithEnablement;

public class TmtIntegratorPanel extends JPanelWithEnablement {
  private static final Logger log = LoggerFactory.getLogger(TmtIntegratorPanel.class);

  private JPanel pTop;
  private UiCheck checkRun;
  private JPanel pContent;
  private JPanel pTable;
  private TmtAnnotationTable tmtAnnotationTable;
  private JScrollPane scrollPaneTmtTable;

  public TmtIntegratorPanel() {
    initMore();
    initPostCreation();
    // register on the bus only after all the components have been created to avoid NPEs
    EventBus.getDefault().register(this);
  }

  private void initPostCreation() {

  }

  private void initMore() {
    this.setLayout(new BorderLayout());
//    this.setBorder(new EmptyBorder(0,0,0,0));
    this.setBorder(new TitledBorder("TMT Qunatitation"));

    // Top panel with run checkbox
    {
      // setting the insets allows the top panel to be shifted left of the options panel
      pTop = new JPanel(new MigLayout(new LC().insetsAll("0px")));

      checkRun = new UiCheck("Run TMT-Integrator", null, true);
      checkRun.setName("ui.name.downstream.run-tmtintegrator");
      checkRun.addActionListener(e -> {
        final boolean isSelected = checkRun.isSelected();
        enablementMapping.put(pContent, isSelected);
        updateEnabledStatus(pContent, isSelected);
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
      pTable.add(new JLabel("TMT Annotations"), BorderLayout.NORTH);

      tmtAnnotationTable.fireInitialization();
      tmtAnnotationTable.setFillsViewportHeight(false);
      scrollPaneTmtTable = new JScrollPane();
      scrollPaneTmtTable.setViewportView(tmtAnnotationTable);

      pTable.add(scrollPaneTmtTable, BorderLayout.CENTER);
      pContent.add(pTable, new CC().growX());
    }

    this.add(pContent, BorderLayout.CENTER);
  }

  public boolean isRun() {
    return checkRun.isEnabled() && checkRun.isSelected();
  }

  @Subscribe
  public void OnRunTmtIntegratorChanged(MessageRunTmtIntegrator m) {
    log.debug("Got MessageRunTmtIntegrator - is run: {}", m.isRun);
  }

  @Subscribe
  public void OnLoadTmtIntegratorDefaults(MessageLoadTmtIntegratorDefaults m) {
    log.debug("Got MessageLoadTmtIntegratorDefaults, it's an empty marker message");
  }
}
