package com.dmtavt.fragpipe.tools.diann;

import static com.dmtavt.fragpipe.Fragpipe.fe;
import static com.github.chhh.utils.SwingUtils.createClickableHtml;

import com.dmtavt.fragpipe.messages.NoteConfigDiann;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.swing.FormEntry;
import com.github.chhh.utils.swing.HtmlStyledJEditorPane;
import com.github.chhh.utils.swing.JPanelBase;
import com.github.chhh.utils.swing.UiCheck;
import com.github.chhh.utils.swing.UiCombo;
import com.github.chhh.utils.swing.UiSpinnerDouble;
import com.github.chhh.utils.swing.UiText;
import com.github.chhh.utils.swing.UiUtils;
import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.ItemSelectable;
import java.awt.image.BufferedImage;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.imageio.ImageIO;
import javax.swing.ImageIcon;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.border.TitledBorder;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.ThreadMode;


public class DiannPanel extends JPanelBase {

  private static final String PREFIX = "diann.";

  private JCheckBox checkRun;
  private JPanel pContent;
  private JPanel pTop;
  private UiCombo uiComboQuantificationStrategy;
  private UiText uiTextCmdOpts;
  private UiSpinnerDouble uiSpinnerQvalue;
  private JPanel panelDiann;

  @Override
  protected void initMore() {
    super.initMore();
  }

  @Subscribe(sticky = true, threadMode = ThreadMode.MAIN_ORDERED)
  public void on(NoteConfigDiann m) {
    if (m.isValid()) {
      updateEnabledStatus(this, true);
      updateEnabledStatus(pContent, isRunDiann());
    } else {
      updateEnabledStatus(this, false);
    }
  }

  @Override
  protected ItemSelectable getRunCheckbox() {
    return checkRun;
  }

  @Override
  protected Component getEnablementToggleComponent() {
    return pContent;
  }

  @Override
  protected String getComponentNamePrefix() {
    return PREFIX;
  }

  private JPanel createPanelTop() {
    // setting the insets allows the top panel to be shifted left of the options panel
    JPanel p = new JPanel(new MigLayout(new LC().insetsAll("0px")));
    mu.borderEmpty(p);

    checkRun = new UiCheck("Quantifying using DIA-NN", null, false);
    checkRun.setName("run-dia-nn");

    String message = "For full DIA-NN functionality, including using the latest version, we recommend downloading and installing DIA-NN from <a href=\"https://github.com/vdemichev/DiaNN/releases\">DIA-NN GitHub repository</a>.<br/><br/>"
        + "<b>Main reference:</b><br/>"
        + "Demichev, V., et al. DIA-NN: neural networks and interference correction enable deep proteome coverage in high throughput. Nat Methods 17, 41–44 (2020).<br/><br/>"
        + "<b>When using DIA-NN in combination with FragPipe-generated spectral libraries, please also cite</b><br/>"
        + "Demichev, V., et al. High sensitivity dia-PASEF proteomics with DIA-NN and FragPipe. bioRxiv 2021.03.08.434385, doi: 2021.03.08.434385<br/>";

    HtmlStyledJEditorPane messagePane = createClickableHtml(message);

    JPanel pIcon = new JPanel(new MigLayout(new LC().fillX()));
    try {
      BufferedImage image = ImageIO.read(getClass().getResource("/com/dmtavt/fragpipe/icons/icon-diann-100.png"));
      JLabel imageLabel = new JLabel(new ImageIcon(image));
      pIcon.add(imageLabel, mu.ccL().wrap());
    } catch (Exception ex) {
      ex.printStackTrace();
    }

    mu.add(p, checkRun).wrap();
    mu.add(p, messagePane).pushX();
    mu.add(p, pIcon).wrap();

    return p;
  }

  private JPanel createPanelContent() {
    JPanel p = new JPanel(new MigLayout(new LC().fillX()));
    mu.borderEmpty(p);

    panelDiann = createPanelDiann();

    mu.add(p, panelDiann).growX().pushX().wrap();

    return p;
  }

  private JPanel createPanelDiann() {
    final JPanel p = mu.newPanel(mu.lcFillX());
    mu.border(p, 1);

    uiSpinnerQvalue = UiUtils.spinnerDouble(0.01, 0.0, 1.0, 0.01).setCols(5).setFormat("#.##").create();
    FormEntry feQvalue = mu.feb(uiSpinnerQvalue).name("q-value").label("Precursor FDR              ").create();

    uiComboQuantificationStrategy = UiUtils.createUiCombo(Arrays.asList("Any LC (high accuracy)", "Any LC (high precision)", "Robust LC (high accuracy)", "Robust LC (high precision)"));
    FormEntry feQuantificationStrategy = new FormEntry("quantification-strategy", "Quantification strategy", uiComboQuantificationStrategy);
    uiComboQuantificationStrategy.setSelectedIndex(0);

    uiTextCmdOpts = UiUtils.uiTextBuilder().cols(20).text("").create();
    FormEntry feCmdOpts = fe(uiTextCmdOpts, "cmd-opts")
        .label("Cmd line opts:              ")
        .tooltip("These options will be passed on to DIA-NN.\n"
            + "This set will be merged with precursor FDR and quantification strategy\n"
            + "To set --threads, please adjust the Parallelism setting in the Workflow tab.\n"
            + "See output log (e.g. dry-run results) for the complete command.").create();

    mu.add(p, feQvalue.label(), mu.ccL()).split();
    mu.add(p, feQvalue.comp).wrap();
    mu.add(p, feQuantificationStrategy.label(), mu.ccL()).split();
    mu.add(p, feQuantificationStrategy.comp).wrap();
    mu.add(p, feCmdOpts.label(), mu.ccL()).split();
    mu.add(p, feCmdOpts.comp).growX().pushX().wrap();

    updateEnabledStatus(p, true);
    return p;
  }

  @Override
  protected void init() {
    this.setLayout(new BorderLayout());
    this.setBorder(new TitledBorder("DIA Quantification"));

    pTop = createPanelTop();
    pContent = createPanelContent();

    this.add(pTop, BorderLayout.NORTH);
    this.add(pContent, BorderLayout.CENTER);
  }

  public boolean isRunDiann() {
    return SwingUtils.isEnabledAndChecked(checkRun);
  }

  public float getDiannQvalue() {
    return (float) uiSpinnerQvalue.getActualValue();
  }

  public String getCmdOpts() {
    return uiTextCmdOpts.getNonGhostText().trim();
  }

  public Set<String> getDiannQuantificationStrategy() {
    switch (uiComboQuantificationStrategy.getSelectedIndex()) {
      case 1:
        return Stream.of("--no-ifs-removal").collect(Collectors.toSet());
      case 2:
        return Stream.of("--peak-center").collect(Collectors.toSet());
      case 3:
        return Stream.of("--peak-center", "--no-ifs-removal").collect(Collectors.toSet());
      default:
        return new HashSet<>();
    }
  }
}