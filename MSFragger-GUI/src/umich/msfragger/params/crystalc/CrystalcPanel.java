package umich.msfragger.params.crystalc;

import com.github.chhh.utils.swing.UiCheck;
import com.github.chhh.utils.swing.UiSpinnerDouble;
import com.github.chhh.utils.swing.UiSpinnerInt;
import java.awt.BorderLayout;
import java.awt.Component;
import java.text.DecimalFormat;
import java.util.HashMap;
import java.util.Map;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.SwingConstants;
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
import umich.msfragger.messages.MessageLoadCrystalcDefaults;
import umich.msfragger.messages.MessageSearchType;
import umich.msfragger.util.SwingUtils;
import umich.msfragger.util.swing.FormEntry;
import umich.msfragger.util.swing.JPanelWithEnablement;

public class CrystalcPanel extends JPanelWithEnablement {
  private static final Logger log = LoggerFactory.getLogger(CrystalcPanel.class);
  public JCheckBox checkRun;
  private JPanel pTop;
  private JPanel pContent;
  private JPanel pParams;
  private static final String nameBase = "ui.name.crystalc.";
  private UiSpinnerInt uiSpinnerIntMaxCharge;
  private UiSpinnerInt uiSpinnerIntNumIsotopes;
  private UiSpinnerInt uiSpinnerIntMassTolPpm;
  private UiCheck uiCheckCorrectIsoErr;
  private UiSpinnerDouble uiSpinnerPrecIsol;

  public CrystalcPanel() {
    initMore();
    EventBus.getDefault().register(this);
  }

  private void initMore() {
    this.setLayout(new BorderLayout());
    this.setBorder(new EmptyBorder(0,0,0,0));
//    this.setBorder(new TitledBorder("Crystalc Analysis"));

    final String htmlTooltip = "<html>Crystal-C performs additional search results cleanup<br/>\n"
        + "Recommended for Open Searches only";

    // Top panel with run checkbox
    {
      // setting the insets allows the top panel to be shifted left of the options panel
      pTop = new JPanel(new MigLayout(new LC().insetsAll("0px").debug()));

      checkRun = new UiCheck("Run Crystal-C", null, false);

      checkRun.setName("ui.name.crystalc.run-crystalc");

      checkRun.addActionListener(e -> {
        final boolean isSelected = checkRun.isSelected();
        enablementMapping.put(pContent, isSelected);
        updateEnabledStatus(pContent, isSelected);
      });
      checkRun.addChangeListener(e -> {
        final boolean isSelected = checkRun.isSelected();
        enablementMapping.put(pContent, isSelected);
        updateEnabledStatus(pContent, isSelected);
      });


      pTop.add(checkRun, new CC().alignX("left"));
      JButton btnLoadDefaults = new JButton("Load Crystal-C defaults");
      btnLoadDefaults.addActionListener((e) -> EventBus
          .getDefault().post(new MessageLoadCrystalcDefaults(true)));
      pTop.add(btnLoadDefaults, new CC().alignX("left"));
      JLabel info = new JLabel(htmlTooltip);
      info.setHorizontalAlignment(SwingConstants.LEADING);
      info.setVerticalAlignment(SwingConstants.TOP);
      info.setAlignmentY(Component.TOP_ALIGNMENT);
      info.setVerticalTextPosition(SwingConstants.TOP);
      pTop.add(info, new CC().gapBefore("50px").alignX("right"));
      pTop.setBorder(new EmptyBorder(0,0,0,0));
      this.add(pTop, BorderLayout.NORTH);
    }

    // Main content panel - container
    {
      pContent = new JPanel(new MigLayout(new LC().insetsAll("0px").fillX()));
      pContent.setBorder(new EmptyBorder(0,0,0,0));

      pContent.addPropertyChangeListener("enabled", evt -> {
        log.debug("Crystalc pContent panel property '{}' changed from '{}' to '{}'", evt.getPropertyName(),
            evt.getOldValue(), evt.getNewValue());
        boolean newValue = (Boolean)evt.getNewValue();
        boolean isSwitchToEnabled = (Boolean) evt.getNewValue() && !(Boolean) evt.getOldValue();
        boolean pContentIsEnabled = newValue && checkRun.isSelected();
        log.debug("Crystalc pContent panel is switching to enabled? : {}, !checkRun.isSelected() : {}, final state should be: {}",
            isSwitchToEnabled, !checkRun.isSelected(), pContentIsEnabled);
        enablementMapping.put(pContent, pContentIsEnabled);
        updateEnabledStatus(pContent, pContentIsEnabled);
      });

//      scroll = new JScrollPane(pContent);
//      scroll.setBorder(new EmptyBorder(0, 0, 0, 0));
//      scroll.getVerticalScrollBar().setUnitIncrement(16);
    }

    {
      pParams = new JPanel(new MigLayout(new LC()));
      pParams.setBorder(new TitledBorder("Crystal-C Options"));
//      pParams.setBorder(new EmptyBorder(0, 0, 0, 0));

      uiSpinnerIntMaxCharge = new UiSpinnerInt(6, 1, 50, 1, 5);
      FormEntry feMaxCharge = new FormEntry("ui.name.crystalc.max-charge", "Max charge",
          uiSpinnerIntMaxCharge);
      uiSpinnerIntNumIsotopes = new UiSpinnerInt(3, 1, 50, 1, 5);
      FormEntry feNumIsotopes = new FormEntry(nameBase + CrystalcParams.PROP_isotope_number,
          "Number of isotopes", uiSpinnerIntNumIsotopes);
      uiSpinnerIntMassTolPpm = new UiSpinnerInt(20, 1, 1000, 1, 5);
      FormEntry feMassTolPpm = new FormEntry(nameBase + CrystalcParams.PROP_precursor_mass , "Mass tolerance (ppm)",
          uiSpinnerIntMassTolPpm);
      uiSpinnerPrecIsol = UiSpinnerDouble.builder(0.7,0.0,1000.0, 0.1)
          .setFormat(new DecimalFormat("0.#")).setNumCols(5).create();
      FormEntry fePrecIsol = new FormEntry(nameBase + CrystalcParams.PROP_precursor_isolation_window, "Precursor isolation window",
          uiSpinnerPrecIsol);
      uiCheckCorrectIsoErr = new UiCheck("Correct isotope error", null, false);
      FormEntry feCheckCorrectIsoErr = new FormEntry(nameBase + CrystalcParams.PROP_correct_isotope_error, "not-shown",
          uiCheckCorrectIsoErr,
          "Correct isotope error by updating precursor neutral mass with the monoisotopic mass");

      pParams.add(feMaxCharge.label(), new CC().alignX("right"));
      pParams.add(feMaxCharge.comp, new CC().alignX("left"));
      pParams.add(feNumIsotopes.label(), new CC().alignX("right"));
      pParams.add(feNumIsotopes.comp, new CC().alignX("left"));
      pParams.add(feCheckCorrectIsoErr.comp, new CC().alignX("left").wrap());
      pParams.add(feMassTolPpm.label(), new CC().alignX("right"));
      pParams.add(feMassTolPpm.comp, new CC().alignX("left"));
      pParams.add(fePrecIsol.label(), new CC().alignX("right"));
      pParams.add(fePrecIsol.comp, new CC().alignX("left").wrap());


      pContent.add(pParams, new CC().wrap().growX());
    }

    this.add(pContent, BorderLayout.CENTER);
    updateEnabledStatus(pContent, SwingUtils.isEnabledAndChecked(checkRun));
  }

  private void loadDefaults() {
    try {
      final Map<String, String> map = new HashMap<>();
      final CrystalcParams props = new CrystalcParams();
      props.loadDefault();
      props.getProps().getMap().keySet().forEach(key -> {
        map.put(nameBase + key, props.getProps().getProp(key).value);
      });
      map.put("ui.name.crystalc.max-charge", Integer.toString(props.getMaxZ()));
      SwingUtils.valuesFromMap(this, map);
    } catch (Exception e) {
      log.error("Error loading Crystal-C defaults", e);
      SwingUtils.showErrorDialog(e, this);
    }
  }

  public CrystalcParams toParams() {
    CrystalcParams p = new CrystalcParams();
    p.loadDefault();
    p.setMaxZ(uiSpinnerIntMaxCharge.getActualValue());
    p.setIsoNum(uiSpinnerIntNumIsotopes.getActualValue());
    p.setPrecursorMassTol(uiSpinnerIntMassTolPpm.getActualValue());
    p.setPrecursorIsolationWindow((Double)uiSpinnerPrecIsol.getValue());
    p.setCorrectIsotopeError(uiCheckCorrectIsoErr.isSelected());
    return p;
  }

  public boolean isRun() {
    return SwingUtils.isEnabledAndChecked(checkRun);
  }

  @Subscribe(threadMode = ThreadMode.MAIN)
  public void onMessageLoadCrystalcDefaults(MessageLoadCrystalcDefaults m) {
    if (m.doAskUser) {
      int answer = SwingUtils.showConfirmDialog(this, new JLabel("<html>Load Crystal-C defaults?"));
      if (JOptionPane.OK_OPTION != answer) {
        log.debug("User cancelled Loading Crystal-C defaults");
        return;
      }
    }

    loadDefaults();
  }

  @Subscribe
  public void onMessageSearchType(MessageSearchType m) {
    switch (m.type) {
      case open:
        checkRun.setSelected(true);
        break;
      case closed:
      case nonspecific:
        checkRun.setSelected(false);
        break;
    }
  }
}
