package umich.msfragger.params.philosopher;

import com.github.chhh.utils.swing.UiCheck;
import com.github.chhh.utils.swing.UiText;
import com.github.chhh.utils.swing.UiUtils.UiTextBuilder;
import java.awt.BorderLayout;
import java.util.ArrayList;
import java.util.List;
import javax.swing.JCheckBox;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSeparator;
import javax.swing.SwingConstants;
import javax.swing.border.EmptyBorder;
import javax.swing.border.TitledBorder;
import net.java.balloontip.BalloonTip;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.greenrobot.eventbus.EventBus;
import org.greenrobot.eventbus.Subscribe;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import umich.msfragger.messages.MessageSearchType;
import umich.msfragger.params.ThisAppProps;
import umich.msfragger.util.swing.FormEntry;
import umich.msfragger.util.swing.JPanelWithEnablement;

public class ReportPanel extends JPanelWithEnablement {

  private static final Logger log = LoggerFactory.getLogger(ReportPanel.class);

  public static final String PROP_ = "";

  private final List<BalloonTip> balloonTips = new ArrayList<>();
  private JCheckBox checkRun;
  private JPanel pTop;
  private JPanel pContent;
  private JScrollPane scroll;
  private JPanel pOptions;
  private JPanel pPrecursorSpectrum;
  private UiText uiTextFilter;
  private UiCheck uiCheckMultiexp;
  private UiCheck uiCheckPepSummary;
  private UiCheck uiCheckWriteMzid;
  private UiCheck uiCheckPrintDecoys;
  private UiCheck uiCheckDontUseProtProphFile;


  public ReportPanel() {
    initMore();
    initPostCreation();
    // register on the bus only after all the components have been created to avoid NPEs
    EventBus.getDefault().register(this);
  }

  private void initPostCreation() {
    this.addPropertyChangeListener("enabled", evt -> {
      log.debug("Report panel property '{}' changed from '{}' to '{}'", evt.getPropertyName(),
          evt.getOldValue(), evt.getNewValue());
      boolean isSwitchToEnabled = (Boolean) evt.getNewValue() && !(Boolean) evt.getOldValue();
      log.debug("Shepherd panel is switching to enabled? : {}, !checkRun.isSelected() : {}",
          isSwitchToEnabled, !checkRun.isSelected());
      if (isSwitchToEnabled && !checkRun.isSelected()) {
        enablementMapping.put(pContent, false);
        updateEnabledStatus(pContent, false);
      }
    });
  }

  @Subscribe
  public void onMessageSearchType(MessageSearchType m) {
    String key = "report.filter." + m.type.name();
//    String val = ThisAppProps.getRemotePropertiesWithLocalDefaults().getProperty(key);
    String val = ThisAppProps.getLocalProperties().getProperty(key);
    if (val == null) {
      throw new IllegalStateException("No Report Filter deafults found in bundle for key: " + key);
    }
    uiTextFilter.setText(val);
    uiCheckPepSummary.setSelected(false);
  }

  public void activate(boolean isActive) {
    updateEnabledStatus(pContent, isActive);
  }

  private void initMore() {

    this.setLayout(new BorderLayout());
//    this.setBorder(new EmptyBorder(0,0,0,0));
    this.setBorder(new TitledBorder("Report"));

    // Top panel with run checkbox
    {
      // setting the insets allows the top panel to be shifted left of the options panel
      pTop = new JPanel(new MigLayout(new LC().insetsAll("0px")));

      checkRun = new UiCheck("Generate Report", null, true);
      checkRun.setName("ui.name.report.generate-report");
      checkRun.addActionListener(e -> {
        final boolean isSelected = checkRun.isSelected();
        enablementMapping.put(pContent, isSelected);
        updateEnabledStatus(pContent, isSelected);
      });
      pTop.add(checkRun, new CC().alignX("left"));
//      JButton btnLoadDefaults = new JButton("Load PTMShepherd defaults");
//      btnLoadDefaults.addActionListener((e) -> EventBus.getDefault().post(new MessageLoadShepherdDefaults(true)));
//      pTop.add(btnLoadDefaults, new CC().alignX("left"));

      pTop.setBorder(new EmptyBorder(0, 0, 0, 0));
      this.add(pTop, BorderLayout.NORTH);
    }

    // Main content panel - container
    {
      pContent = new JPanel(new MigLayout(new LC().fillX()));
//      pContent = new JPanel(new MigLayout(new LC().fillX().insetsAll("0px")));
      pContent.setBorder(new EmptyBorder(0, 0, 0, 0));
    }

    {
      pOptions = new JPanel(new MigLayout(new LC()));//.debug()));
      //pPeakPicking.setBorder(new TitledBorder("PTMShepherd options"));
      pOptions.setBorder(new EmptyBorder(0, 0, 0, 0));

      uiTextFilter = new UiTextBuilder().cols(5).text("--sequential --razor --prot 0.01").create();
      FormEntry feFilter = new FormEntry("ui.name.report.text.filter", "Filter", uiTextFilter,
          "<html>A custom algorithm for MS/MS data filtering and multi-level false discovery rate estimation.<br/>\n"
              + "See: https://github.com/Nesvilab/philosopher/wiki/Filter<br/><br/>\n" +
              "philosopher filter [flags]<br>\n" +
              "Flags:<br/>\n" +
              "<ul>\n" +
              "<li>--ion float        peptide ion FDR level (default 0.01)</li>\n" +
              "<li>--mapmods          map modifications aquired by an open search</li>\n" +
              "<li>--models           print model distribution</li>\n" +
              "<li>--pep float        peptide FDR level (default 0.01)</li>\n" +
              "<li>--pepProb float    top peptide probability treshold for the FDR filtering (default 0.7)</li>\n"
              +
              "<li>--pepxml string    pepXML file or directory containing a set of pepXML files</li>\n"
              +
              "<li>--picked           apply the picked FDR algorithm before the protein scoring</li>\n"
              +
              "<li>--prot float       protein FDR level (default 0.01)</li>\n" +
              "<li>--protProb float   protein probability treshold for the FDR filtering (not used with the razor algorithm) (default 0.5)</li>\n"
              +
              "<li>--protxml string   protXML file path</li>\n" +
              "<li>--psm float        psm FDR level (default 0.01)</li>\n" +
              "<li>--razor            use razor peptides for protein FDR scoring</li>\n" +
              "<li>--sequential       alternative algorithm that estimates FDR using both filtered PSM and Protein lists</li>\n"
              +
              "<li>--tag string       decoy tag (default \"rev_\")</li>\n" +
              "<li>--weight float     threshold for defining peptide uniqueness (default 1)</li>\n"
              +
              "</ul>");

      pOptions.add(feFilter.label(), new CC().alignX("left").spanX().split());
      pOptions.add(feFilter.comp, new CC().alignX("left").growX().pushX().wrap());

      uiCheckMultiexp = new UiCheck("Multi-experiment Report", null, false);
      FormEntry feCheckMultiexp = new FormEntry("ui.name.report.check.multiexp", "not-shown",
          uiCheckMultiexp);
      uiCheckPepSummary = new UiCheck("Generate peptide level summary", null, false);
      FormEntry feCheckPepSummary = new FormEntry("ui.name.reoirt.pep-level-summary", "not-shown",
          uiCheckPepSummary,
          "<html>Optional generation of combined.pep.xml files for multi-experiment setup.");

      pOptions.add(feCheckMultiexp.comp, new CC().alignX("left").spanX().split());
      pOptions.add(feCheckPepSummary.comp, new CC().alignX("left").wrap());
      pOptions.add(new JSeparator(SwingConstants.HORIZONTAL), new CC().growX().spanX().wrap());

      uiCheckWriteMzid = new UiCheck("Write mzID output (experimental)", null, false);
      uiCheckPrintDecoys = new UiCheck("Print decoys", null, false);
      uiCheckDontUseProtProphFile = new UiCheck("Do not use ProteinProphet file", null, false);

      FormEntry feCheckWriteMzid = new FormEntry("ui.name.report.check.mzid", "not-shown",
          uiCheckWriteMzid);
      FormEntry feCheckPrintDecoys = new FormEntry("ui.name.report.check.printdecoys", "not-shown",
          uiCheckPrintDecoys);
      FormEntry feCheckDontUseProtProphFile = new FormEntry(
          "ui.name.report.check.dontuseprotprophfile", "not-shown", uiCheckDontUseProtProphFile,
          "<html>Only to be used in rare cases.<br/>\n" +
              "Consider turning off Protein Prophet instead of using this checkbox.");
      pOptions.add(feCheckWriteMzid.comp, new CC().alignX("left"));
      pOptions.add(feCheckPrintDecoys.comp, new CC().alignX("left"));
      pOptions.add(feCheckDontUseProtProphFile.comp, new CC().alignX("left").wrap());

      pContent.add(pOptions, new CC().wrap().growX());
    }

    this.add(pContent, BorderLayout.CENTER);
  }

  public boolean isGenerateReport() {
    return checkRun.isEnabled() && checkRun.isSelected();
  }

  public boolean isMultiExpReport() {
    return uiCheckMultiexp.isSelected();
  }

  public boolean isPepSummary() {
    return uiCheckPepSummary.isSelected();
  }

  public boolean isNoProtXml() {
    return uiCheckDontUseProtProphFile.isSelected();
  }

  public String getFilterCmdText() {
    return uiTextFilter.getNonGhostText();
  }

  public boolean isPrintDecoys() {
    return uiCheckPrintDecoys.isSelected();
  }

  public boolean isWriteMzid() {
    return uiCheckWriteMzid.isSelected();
  }

  private void clearBalloonTips() {
    for (BalloonTip balloonTip : balloonTips) {
      if (balloonTip != null) {
        try {
          balloonTip.closeBalloon();
        } catch (Exception ignore) {
        }
      }
    }
    balloonTips.clear();
  }

//  public boolean validateForm() {
//
//    Pattern reVarMods = Pattern.compile("[^\\s]+:-?\\d+(?:\\.\\d+)?(?:\\s*,\\s*[^\\s]+:-?\\d+(?:\\.\\d+)?)*");
//    String text = uiTextVarMods.getNonGhostText().trim();
//    boolean ok = true;
//    if (!StringUtils.isNullOrWhitespace(text) && !reVarMods.matcher(text).matches()) {
//      BalloonTip tip = new BalloonTip(uiTextVarMods,
//          "<html>Does not match allowed format \"&lt;name&gt;:&lt;mass&gt;\"");
//      tip.setVisible(true);
//      balloonTips.add(tip);
//      ok = false;
//    }
//    return ok;
//  }
}
