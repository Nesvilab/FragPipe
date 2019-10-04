package umich.msfragger.params.imquant;

import com.github.chhh.utils.swing.UiCheck;
import java.awt.BorderLayout;
import java.util.Map;
import java.util.Properties;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.border.EmptyBorder;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.greenrobot.eventbus.EventBus;
import org.greenrobot.eventbus.Subscribe;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import umich.msfragger.messages.MessageLoadQuantDefaults;
import umich.msfragger.messages.MessageLoadShepherdDefaults;
import umich.msfragger.util.swing.JPanelWithEnablement;
import umich.msfragger.util.PropertiesUtils;
import umich.msfragger.util.SwingUtils;

public class QuantJPanel extends JPanelWithEnablement {
  private static final Logger log = LoggerFactory.getLogger(QuantJPanel.class);
  private JPanel pTop;
  private UiCheck checkRun;


  public QuantJPanel() {
    initMore();
    initPostCreation();
    // register on the bus only after all the components have been created to avoid NPEs
    EventBus.getDefault().register(this);
  }

  private void initPostCreation() {
  }

  @Subscribe
  public void onMessageLoadQuantDefaults(MessageLoadQuantDefaults m) {
    log.debug("Got MessageLoadQuantDefaults");
    if (m.doAskUser) {
      int answer = SwingUtils.showConfirmDialog(this, new JLabel("<html>Load Quant default parameters?"));
      if (JOptionPane.OK_OPTION != answer) {
        log.debug("User cancelled Loading Quant defaults");
        return;
      }
    }

    try {
      Properties props = PropertiesUtils
          .loadPropertiesLocal(QuantParams.class, QuantParams.DEFAULT_PROPERTIES_FN);
      SwingUtils.valuesFromMap(this, PropertiesUtils.to(props));
    } catch (Exception e) {
      log.error("Error loading shepherd defaults", e);
      SwingUtils.showErrorDialog(e, this);
    }

  }

  public boolean isRun() {
    return SwingUtils.isEnabledAndChecked(checkRun);
  }
  public boolean isImquant() {
    return SwingUtils.isEnabledAndChecked(checkRunImquant);
  }
  public boolean isFreequant() {
    return SwingUtils.isEnabledAndChecked(checkRunFreequant);
  }


  public Map<String, String> toMap() {
    return SwingUtils.valuesToMap(this, (name) -> !name.startsWith("Spinner.formattedTextField"));
  }

  private void initMore() {
    this.setLayout(new BorderLayout());
    this.setBorder(new EmptyBorder(0,0,0,0));

    // Top panel with run checkbox
    {
      // setting the insets allows the top panel to be shifted left of the options panel
      pTop = new JPanel(new MigLayout(new LC().insetsAll("0px")));

      checkRun = new UiCheck("Run Qunatitation", null, true);
      checkRun.setName("ui.name.report.run-quantitation");
      checkRun.addActionListener(e -> {
        final boolean isSelected = checkRun.isSelected();

        enablementMapping.put(pContent, isSelected);
        updateEnabledStatus(pContent, isSelected);
      });
      pTop.add(checkRun, new CC().alignX("left"));
      JButton btnLoadDefaults = new JButton("Load PTMShepherd defaults");
      btnLoadDefaults.addActionListener((e) -> EventBus.getDefault().post(new MessageLoadShepherdDefaults(true)));
      pTop.add(btnLoadDefaults, new CC().alignX("left"));

      pTop.setBorder(new EmptyBorder(0,0,0,0));
      this.add(pTop, BorderLayout.NORTH);
    }

    
  }

}
