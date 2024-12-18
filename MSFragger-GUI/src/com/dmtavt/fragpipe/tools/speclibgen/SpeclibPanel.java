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

package com.dmtavt.fragpipe.tools.speclibgen;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.exceptions.NoStickyException;
import com.dmtavt.fragpipe.exceptions.ValidationException;
import com.dmtavt.fragpipe.messages.NoteConfigPython;
import com.dmtavt.fragpipe.messages.NoteConfigSpeclibgen;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.swing.FileChooserUtils;
import com.github.chhh.utils.swing.FileChooserUtils.FcMode;
import com.github.chhh.utils.swing.FormEntry;
import com.github.chhh.utils.swing.JPanelBase;
import com.github.chhh.utils.swing.UiCheck;
import com.github.chhh.utils.swing.UiCombo;
import com.github.chhh.utils.swing.UiSpinnerDouble;
import com.github.chhh.utils.swing.UiText;
import com.github.chhh.utils.swing.UiUtils;
import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.ItemSelectable;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.ButtonGroup;
import javax.swing.JRadioButton;
import javax.swing.border.TitledBorder;
import javax.swing.filechooser.FileNameExtensionFilter;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.ThreadMode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class SpeclibPanel extends JPanelBase {

  private static final Logger log = LoggerFactory.getLogger(SpeclibPanel.class);
  private static final String PREFIX = "speclibgen.";

  private JCheckBox checkRun;
  private JPanel pContent;
  private JPanel pTop;
  public JCheckBox checkKeepIntermediateFiles;
  private List<String> pqpType;
  private List<String> pqpCal;
  private UiText uiTextPqpCalFile;
  private UiText uiTextPqpIMCalFile;
  private UiCombo uiComboPqpType;
  private UiCombo uiComboPqpCal;
  private UiCombo uiComboPqpIMCal;
  private UiSpinnerDouble uiSpinnerLowess;
  private UiSpinnerDouble uiSpinner_max_delta_unimod;
  private UiSpinnerDouble uiSpinner_max_delta_ppm;
  private JCheckBox check_fragment_type_a;
  private JCheckBox check_fragment_type_b;
  private JCheckBox check_fragment_type_c;
  private JCheckBox check_fragment_type_x;
  private JCheckBox check_fragment_type_y;
  private JCheckBox check_fragment_type_z;
  private JCheckBox uiCheckNeutralLoss;
  private UiCombo uiComboGlycoMode;
  private UiSpinnerDouble uiSpinner_max_glycan_qval;
  private ButtonGroup convertButtonGroup;
  private JRadioButton psmConvertButton;
  private JRadioButton pepxmlConvertButton;
  private JPanel panelEasypqp;
  public static final String EASYPQP_TIMSTOF = "timsTOF";
  public static final String EASYPQP_EXTRAS_PREFIX = "easypqp.extras.";

  @Override
  protected void initMore() {
    super.initMore();
  }

  @Subscribe(sticky = true, threadMode = ThreadMode.MAIN_ORDERED)
  public void on(NoteConfigSpeclibgen m) {
    log.debug("SpeclibPanel got NoteConfigSpeclibgen, instance not null? - {}", m.instance != null);
    if (m.instance == null || !m.isValid()) {
      updateEnabledStatus(this, false);
      return;
    }

    updateEnabledStatus(this, true);
    updateEnabledStatus(panelEasypqp, m.instance.isEasypqpOk());
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

    checkRun = new UiCheck("Generate spectral library from search results", null, false);
    checkRun.setName("run-speclibgen");

    mu.add(p, checkRun);
    return p;
  }

  private JPanel createPanelContent() {
    JPanel p = new JPanel(new MigLayout(new LC().fillX()));
    mu.borderEmpty(p);

    panelEasypqp = createPanelEasypqp();

    mu.add(p, panelEasypqp).spanX().growX().wrap();

    return p;
  }

  private JPanel createPanelEasypqp() {
    final JPanel p = mu.newPanel(mu.lcFillXNoInsetsTopBottom());
    mu.border(p, 1);

    checkKeepIntermediateFiles = new UiCheck("keep intermediate files", null, false);
    checkKeepIntermediateFiles.setName("keep-intermediate-files");

    convertButtonGroup = new ButtonGroup();
    pepxmlConvertButton = new JRadioButton("pepXML");
    pepxmlConvertButton.setName("convert-pepxml");
    pepxmlConvertButton.setSelected(true);
    psmConvertButton = new JRadioButton("psm.tsv");
    psmConvertButton.setName("convert-psm");
    psmConvertButton.setSelected(false);
    convertButtonGroup.add(pepxmlConvertButton);
    convertButtonGroup.add(psmConvertButton);
    JLabel convertTypeLabel = new JLabel("Filetype to Convert:");
    convertTypeLabel.setToolTipText("Use pepXML or psm.tsv files as input to library conversion. Note: Glyco mode requires psm.tsv conversion");

    final String optionAuto = "Automatic selection of a run as reference RT";
    final String optionManual = "User provided RT calibration file";
    pqpCal = Arrays.asList(optionAuto, "Biognosys_iRT", "ciRT", "Pierce_iRT", optionManual);
    pqpType = Arrays.asList(EASYPQP_TIMSTOF, "non-timsTOF");

    uiComboPqpCal = UiUtils.createUiCombo(pqpCal);
    FormEntry fePqpCal = new FormEntry("easypqp.rt-cal",
        "RT calibration", uiComboPqpCal, SwingUtils.makeHtml(
        "<b>Automatic selection of a run as reference RT</b> selects the run with the most identified peptides as a reference. Overlapped peptides are used for the alignment\n"
            + "<b>Biognosys_iRT</b>, <b>ciRT</b>, and <b>Pierce_iRT</b> uses the peptides from the Biognosys iRT kit, common human peptides, and Pierce iRT kit to perform the alignment.\n"
            + "For unfractionated data, <b>Automatic selection of a run as reference RT</b> is recommended.\n"
            + "When building the library from fractionated data, there may be not enough overlapped peptides for the alignment. Should consider using the other options.\n"
            + "<b>ciRT</b> is overall the safest option for human samples.\n"
            + "Users can also provide their own iRT peptides by using the <b>User provided RT calibration file</b> option.\n"));
    final String optionIMManual = "User provided IM calibration file";
    uiComboPqpIMCal = UiUtils.createUiCombo(Arrays.asList("Automatic selection of a run as reference IM", optionIMManual));
    FormEntry fePqpIMCal = new FormEntry("easypqp.im-cal",
        "IM calibration", uiComboPqpIMCal);
    uiTextPqpCalFile = UiUtils.uiTextBuilder().create();
    uiTextPqpIMCalFile = UiUtils.uiTextBuilder().create();
    FormEntry fePqpCalFile = mu.feb(uiTextPqpCalFile)
        .name("easypqp.select-file.text").label("Calibration file").create();
    JLabel labelPqpCalFile = fePqpCalFile.label();
    Fragpipe.renameNoCache(labelPqpCalFile, "easypqp.select-file.label");
    FormEntry fePqpIMCalFile = mu.feb(uiTextPqpIMCalFile)
        .name("easypqp.select-im-file.text").label("Calibration file").create();
    JLabel labelPqpIMCalFile = fePqpIMCalFile.label();
    Fragpipe.renameNoCache(labelPqpIMCalFile, "easypqp.select-im-file.label");
    final JButton btnPqpCalFile = fePqpCalFile.browseButton("Browse",
        "Select calibration file", () -> {
          final FileNameExtensionFilter fileNameExtensionFilter = new FileNameExtensionFilter("TSV files", "tsv", "txt");
          JFileChooser fc = FileChooserUtils
              .create("Calibration file", "Select", false, FcMode.FILES_ONLY, true, fileNameExtensionFilter);
          fc.setFileFilter(fileNameExtensionFilter);
          FileChooserUtils.setPath(fc, Stream.of(uiTextPqpCalFile.getNonGhostText()));
          return fc;
        },
        paths -> {
          log.debug("User selected PQP file: {}",
              paths.stream().map(Path::toString).collect(Collectors.joining(", ")));
          Path path = paths.get(0); // we only allowed selection of a single file in the file chooser
          try {
            validateCalFile(path);
          } catch (ValidationException e) {
            SwingUtils.showErrorDialog(this, SwingUtils.makeHtml(e.getMessage()), "Cal file error");
            return;
          }
          // validation went without exceptions
          uiTextPqpCalFile.setText(path.toString());
        });
    btnPqpCalFile.setName("easypqp.select-file.button." + Fragpipe.PROP_NOCACHE);
    final JButton btnPqpIMCalFile = fePqpIMCalFile.browseButton("Browse",
        "Select calibration file", () -> {
          final FileNameExtensionFilter fileNameExtensionFilter = new FileNameExtensionFilter("TSV files", "tsv", "txt");
          JFileChooser fc = FileChooserUtils
              .create("Calibration file", "Select", false, FcMode.FILES_ONLY, true, fileNameExtensionFilter);
          fc.setFileFilter(fileNameExtensionFilter);
          FileChooserUtils.setPath(fc, Stream.of(uiTextPqpIMCalFile.getNonGhostText()));
          return fc;
        },
        paths -> {
          log.debug("User selected PQP file: {}",
              paths.stream().map(Path::toString).collect(Collectors.joining(", ")));
          Path path = paths.get(0); // we only allowed selection of a single file in the file chooser
          try {
            validateIMCalFile(path);
          } catch (ValidationException e) {
            SwingUtils.showErrorDialog(this, SwingUtils.makeHtml(e.getMessage()), "Cal file error");
            return;
          }
          // validation went without exceptions
          uiTextPqpIMCalFile.setText(path.toString());
        });
    btnPqpIMCalFile.setName("easypqp.select-im-file.button." + Fragpipe.PROP_NOCACHE);

    uiComboPqpType = UiUtils.createUiCombo(pqpType);
    uiSpinnerLowess = UiUtils.spinnerDouble(0.01, 0.0, 1.0, 0.01)
        .setCols(5).setFormat("#.##").create();
    FormEntry feLowess = mu.feb(uiSpinnerLowess).name("easypqp.extras.rt_lowess_fraction")
        .label("RT Lowess fraction")
        .tooltip("Fraction of data points to use for RT lowess regression. If set to 0, cross validation is used.").create();

    uiSpinner_max_delta_unimod = UiUtils.spinnerDouble(0.02, 0.0001, 1.0, 0.01)
            .setCols(5).setFormat("#.##").create();
    FormEntry fe_max_delta_unimod = mu.feb(uiSpinner_max_delta_unimod).name("easypqp.extras.max_delta_unimod")
        .label("UniMod annotation tol (Da)")
        .tooltip("Maximum delta mass (Dalton) for UniMod annotation.  [default: 0.02]").create();

    uiSpinner_max_delta_ppm = UiUtils.spinnerDouble(15, 1, 500, 1)
            .setCols(5).setFormat("#.##").create();
    FormEntry fe_max_delta_ppm = mu.feb(uiSpinner_max_delta_ppm).name("easypqp.extras.max_delta_ppm")
        .label("Fragment annotation tol (ppm)")
        .tooltip("Maximum delta mass (PPM) for annotation. [default: 15]").create();

    check_fragment_type_a = new UiCheck("a", null, false);
    FormEntry feFragmentTypeA = mu.feb(check_fragment_type_a).name("easypqp.fragment.a").label("a").create();

    check_fragment_type_b = new UiCheck("b", null, true);
    FormEntry feFragmentTypeB = mu.feb(check_fragment_type_b).name("easypqp.fragment.b").label("b").create();

    check_fragment_type_c = new UiCheck("c", null, false);
    FormEntry feFragmentTypeC = mu.feb(check_fragment_type_c).name("easypqp.fragment.c").label("c").create();

    check_fragment_type_x = new UiCheck("x", null, false);
    FormEntry feFragmentTypeX = mu.feb(check_fragment_type_x).name("easypqp.fragment.x").label("x").create();

    check_fragment_type_y = new UiCheck("y", null, true);
    FormEntry feFragmentTypeY = mu.feb(check_fragment_type_y).name("easypqp.fragment.y").label("y").create();

    check_fragment_type_z = new UiCheck("z", null, false);
    FormEntry feFragmentTypeZ = mu.feb(check_fragment_type_z).name("easypqp.fragment.z").label("z").create();

    uiCheckNeutralLoss = new UiCheck("neutral loss", null, false);
    FormEntry feNeutralLoss = mu.feb(uiCheckNeutralLoss).name("easypqp.neutral_loss").label("neutral loss").tooltip("Add neutral loss fragments to the spectral library.").create();

    uiComboGlycoMode = UiUtils.createUiCombo(Arrays.asList("Regular (not glyco)", "O-glyco", "N-glyco", "N-glyco+HexNAc"));
    String glycoTooltip = "Labile glyco search modes:\n" +
            "Regular = standard, non-glyco search (all modifications included intact on fragment ions in the library)\n" +
            "O-glyco = modifications larger than 140 Da on S,T residues are considered labile and are not placed on fragment ions\n" +
            "N-glyco = modifications larger than 140 Da on N residues are considered labile and are not placed on fragment ions\n" +
            "N-glyco+HexNAc = modifications larger than 140 Da on N residues have a HexNAc fragment remainder ion (203.08 Da) placed instead of the intact modification mass";
    FormEntry feComboGlycoMode = mu.feb(uiComboGlycoMode).name("easypqp.labile_mode").label("Glyco Mode").tooltip(glycoTooltip).create();

    uiSpinner_max_glycan_qval = UiUtils.spinnerDouble(1, 0, 1, 0.01)
            .setCols(5).setFormat("#.##").create();
    FormEntry fe_max_glycan_qval = mu.feb(uiSpinner_max_glycan_qval).name("easypqp.extras.max_glycan_qval")
            .label("Max glycan q-value")
            .tooltip("Maximum glycan q-value from glycan composition assignment to use a glycopeptide PSM when building the library. Set to 1 to ignore.").create();

    uiComboGlycoMode.addItemListener(e -> {
      updateEnabledStatus(uiSpinner_max_glycan_qval, uiComboGlycoMode.getSelectedIndex() > 0);
    });

    mu.add(p, convertTypeLabel);
    mu.add(p, pepxmlConvertButton).split();
    mu.add(p, psmConvertButton).split();
    mu.add(p, checkKeepIntermediateFiles).gapLeft("80").wrap();

    mu.add(p, fePqpCal.label(), ccR());
    mu.add(p, fePqpCal.comp).split();
    mu.add(p, labelPqpCalFile);
    mu.add(p, btnPqpCalFile);
    mu.add(p, fePqpCalFile.comp).pushX().growX().wrap();

    mu.add(p, fePqpIMCal.label(), ccR());
    mu.add(p, fePqpIMCal.comp).split();
    mu.add(p, labelPqpIMCalFile);
    mu.add(p, btnPqpIMCalFile);
    mu.add(p, fePqpIMCalFile.comp).pushX().growX().wrap();

    mu.add(p, feLowess.label(), mu.ccR());
    mu.add(p, feLowess.comp).split();
    mu.add(p, new JLabel("Fragment types:")).gapLeft("40");
    mu.add(p, feFragmentTypeA.comp);
    mu.add(p, feFragmentTypeB.comp);
    mu.add(p, feFragmentTypeC.comp);
    mu.add(p, feFragmentTypeX.comp);
    mu.add(p, feFragmentTypeY.comp);
    mu.add(p, feFragmentTypeZ.comp);
    mu.add(p, feNeutralLoss.comp).wrap();

    mu.add(p, fe_max_delta_unimod.label(), mu.ccR());
    mu.add(p, fe_max_delta_unimod.comp).split().wrap();

    mu.add(p, fe_max_delta_ppm.label(), mu.ccR());
    mu.add(p, fe_max_delta_ppm.comp).wrap();

    mu.add(p, feComboGlycoMode.label(), mu.ccR());
    mu.add(p, feComboGlycoMode.comp).split();
    mu.add(p, fe_max_glycan_qval.label(), mu.ccR());
    mu.add(p, fe_max_glycan_qval.comp).wrap();

    uiComboPqpCal.addItemListener(e -> {
      String selected = (String) e.getItem();
      final boolean show = optionManual.equals(selected);
      final AtomicBoolean visibilityChanged = new AtomicBoolean(false);
      SwingUtils.traverse(p, false, c -> {
        String name = c.getName();
        if (name != null && name.contains("easypqp.select-file.")) {
          log.debug("Traversing easyPQP options panel, found matching component: {}", name);
          if (c.isVisible() != show) {
            visibilityChanged.set(show);
            c.setVisible(show);
          }
        }
      });
      if (visibilityChanged.get()) {
        p.revalidate();
      }
    });
    uiComboPqpCal.setSelectedIndex(1);
    uiComboPqpCal.setSelectedIndex(0);

    uiComboPqpIMCal.addItemListener(e -> {
      String selected = (String) e.getItem();
      final boolean show = optionIMManual.equals(selected);
      final AtomicBoolean visibilityChanged = new AtomicBoolean(false);
      SwingUtils.traverse(p, false, c -> {
        String name = c.getName();
        if (name != null && name.contains("easypqp.select-im-file.")) {
          log.debug("Traversing easyPQP options panel, found matching component: {}", name);
          if (c.isVisible() != show) {
            visibilityChanged.set(show);
            c.setVisible(show);
          }
        }
      });
      if (visibilityChanged.get()) {
        p.revalidate();
      }
    });
    uiComboPqpIMCal.setSelectedIndex(1);
    uiComboPqpIMCal.setSelectedIndex(0);

    updateEnabledStatus(p, false);
    return p;
  }

  /* Additional parameters for easypqp configuration that don't each need separate getters. */
  public Map<String, String> getMapArgsExtras() {
    return SwingUtils.valuesGet(this, name -> name.contains(EASYPQP_EXTRAS_PREFIX));
  }

  @Override
  protected void init() {
    this.setLayout(new BorderLayout());
    this.setBorder(new TitledBorder("Spectral library generation"));

    pTop = createPanelTop();
    pContent = createPanelContent();

    this.add(pTop, BorderLayout.NORTH);
    this.add(pContent, BorderLayout.CENTER);
  }

  private void validateCalFile(Path path) throws ValidationException {
    NoteConfigPython configPython;
    try {
      configPython = Fragpipe.getSticky(NoteConfigPython.class);
    } catch (NoStickyException e) {
      throw new ValidationException(e);
    }
    final String s = configPython.pi.validateCalFile(path);
    if (s.trim().endsWith("ok"))
      return;
    throw new ValidationException(s);
  }

  private void validateIMCalFile(Path path) throws ValidationException {
    NoteConfigPython configPython;
    try {
      configPython = Fragpipe.getSticky(NoteConfigPython.class);
    } catch (NoStickyException e) {
      throw new ValidationException(e);
    }
    final String s = configPython.pi.validateIMCalFile(path);
    if (s.trim().endsWith("ok"))
      return;
    throw new ValidationException(s);
  }

  private static CC ccL() {
    return new CC().alignX("left");
  }

  private static CC ccR() {
    return new CC().alignX("right");
  }

  @Override
  public boolean isRun() {
    return SwingUtils.isEnabledAndChecked(checkRun);
  }

  public boolean isChecked() {
    return checkRun.isSelected();
  }

  public boolean isCheckRunEnabled() {
    return checkRun.isEnabled();
  }

  public String getEasypqpDataType() {
    return (String)uiComboPqpType.getSelectedItem();
  }

  public String getEasypqpCalOption() {
    return new String[]{"noiRT", "Biognosys_iRT", "ciRT", "Pierce_iRT", "a tsv file"}[uiComboPqpCal.getSelectedIndex()];
  }
  public String getEasypqpIMCalOption() {
    return new String[]{"noIM", "a tsv file"}[uiComboPqpIMCal.getSelectedIndex()];
  }
  public String getEasypqpGlycoOption() {
    return new String[]{"", "oglyc", "nglyc", "nglyc+"}[uiComboGlycoMode.getSelectedIndex()];
  }

  public double getEasypqpMaxGlycanQ() {
    return uiSpinner_max_glycan_qval.getActualValue();
  }

  public Path getEasypqpCalFilePath() {
    return Paths.get(uiTextPqpCalFile.getNonGhostText());
  }

  public Path getEasypqpIMCalFilePath() {
    return Paths.get(uiTextPqpIMCalFile.getNonGhostText());
  }

  public String getEasypqpFileType() {
    return (String) uiComboPqpType.getSelectedItem();
  }

  public double getEasypqpRTLowessFraction() {
    return uiSpinnerLowess.getActualValue();
  }

  public double getEasypqp_max_delta_unimod() {
    return uiSpinner_max_delta_unimod.getActualValue();
  }

  public double getEasypqp_max_delta_ppm() {
    return uiSpinner_max_delta_ppm.getActualValue();
  }

  public String getEasypqp_fragment_types() {
    final boolean[] a = new boolean[]{
            check_fragment_type_a.isSelected(),
            check_fragment_type_b.isSelected(),
            check_fragment_type_c.isSelected(),
            check_fragment_type_x.isSelected(),
            check_fragment_type_y.isSelected(),
            check_fragment_type_z.isSelected(),
    };
    final String[] chars = new String[]{"a", "b", "c", "x", "y", "z"};
    final StringBuilder ret = new StringBuilder("[");
    for (int i = 0; i < chars.length; i++)
      if (a[i])
        ret.append("'").append(chars[i]).append("',");
    return ret.append("]").toString();
  }

  public boolean hasNeutralLoss() {
    return uiCheckNeutralLoss.isSelected();
  }
  public boolean isConvertPSM() {
    return psmConvertButton.isSelected();
  }
  public boolean checkGlycoMode() {
    if (!getEasypqpGlycoOption().equals("")) {
      // glyco mode enabled -> require psm.tsv conversion
      return isConvertPSM();
    } else {
      return true;
    }
  }
}