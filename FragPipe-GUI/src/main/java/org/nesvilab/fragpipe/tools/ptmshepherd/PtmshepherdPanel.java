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

package org.nesvilab.fragpipe.tools.ptmshepherd;

import org.nesvilab.fragpipe.Fragpipe;
import org.nesvilab.fragpipe.api.Bus;
import org.nesvilab.fragpipe.api.SearchTypeProp;
import org.nesvilab.fragpipe.messages.MessageLoadShepherdDefaults;
import org.nesvilab.fragpipe.messages.MessageSearchType;
import org.nesvilab.fragpipe.messages.NoteConfigPtmShepherd;
import org.nesvilab.fragpipe.tools.enums.MassTolUnits;
import org.nesvilab.utils.MapUtils;
import org.nesvilab.utils.PropertiesUtils;
import org.nesvilab.utils.StringUtils;
import org.nesvilab.utils.SwingUtils;
import org.nesvilab.utils.swing.FileChooserUtils;
import org.nesvilab.utils.swing.FileChooserUtils.FcMode;
import org.nesvilab.utils.swing.FormEntry;
import org.nesvilab.utils.swing.JPanelBase;
import org.nesvilab.utils.swing.UiCheck;
import org.nesvilab.utils.swing.UiCombo;
import org.nesvilab.utils.swing.UiSpinnerDouble;
import org.nesvilab.utils.swing.UiSpinnerInt;
import org.nesvilab.utils.swing.UiText;
import org.nesvilab.utils.swing.UiUtils;
import org.nesvilab.utils.swing.UiUtils.UiTextBuilder;
import java.awt.Component;
import java.awt.ItemSelectable;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.awt.image.BufferedImage;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Properties;
import java.util.function.Function;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.imageio.ImageIO;
import javax.swing.ButtonGroup;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import net.java.balloontip.BalloonTip;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.ThreadMode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class PtmshepherdPanel extends JPanelBase {
  private static final Logger log = LoggerFactory.getLogger(PtmshepherdPanel.class);
  private static final String PREFIX = "ptmshepherd.";

  public static final String PROP_histo_smoothbins = "histo_smoothbins";
  public static final String PROP_histo_normalizeTo = "histo_normalizeTo";
  public static final String PROP_peakpicking_promRatio = "peakpicking_promRatio";
  public static final String PROP_peakpicking_width = "peakpicking_width";
  public static final String PROP_peakpicking_mass_units = "peakpicking_mass_units";
  public static final String PROP_peakpicking_minPsm = "peakpicking_minPsm";

  public static final String PROP_precursor_mass_units = "precursor_mass_units";
  public static final String PROP_precursor_tol = "precursor_tol";
  public static final String PROP_annotation_tol = "annotation_tol";
  public static final String PROP_annotate_assigned_mods = "annotate_assigned_mods";
  public static final String PROP_use_msfragger_localization = "use_msfragger_localization";

  public static final String PROP_spectra_ppmtol = "spectra_ppmtol";
  public static final String PROP_localization_background = "localization_background";
  public static final String PROP_output_extended = "output_extended";
  private static final String PROP_varmod_masses = "varmod_masses";
  private static final String PROP_annotation_file = "annotation_file";
  private static final String PROP_iontype_a = "iontype_a";
  private static final String PROP_iontype_x = "iontype_x";
  private static final String PROP_iontype_b = "iontype_b";
  private static final String PROP_iontype_y = "iontype_y";
  private static final String PROP_iontype_c = "iontype_c";
  private static final String PROP_iontype_z = "iontype_z";
  private static final String PROP_cap_y_ions = "cap_y_ions";
  private static final String PROP_diag_ions = "diag_ions";
  private static final String PROP_remainder_masses = "remainder_masses";
  private static final String PROP_iterloc_mode = "iterloc_mode";
  private static final String PROP_iterloc_maxEpoch = "iterloc_maxEpoch";
  private static final String PROP_spectra_maxfragcharge = "spectra_maxfragcharge";
  private static final String PROP_spectra_maxPrecCharge = "spectra_maxPrecursorCharge";
  private static final String PROP_spectra_condPeaks = "spectra_condPeaks";
  private static final String PROP_spectra_condRatio = "spectra_condRatio";
  private static final String PROP_restrict_loc = "localization_allowed_res";

  private static final String PROP_custom_modlist_loc = "ptmshepherd.path.modlist";

  private static final String PROP_run_diagMine_mode = "run_diagmine_mode";
  private static final String PROP_diagMine_minIonsPerSpec = "diagmine_minIonsPerSpec";
  private static final String PROP_diagMine_diagMinSpecPct = "diagmine_diagMinSpecDiff";
  private static final String PROP_diagMine_diagMinFoldChange = "diagmine_diagMinFoldChange";
  private static final String PROP_diagMine_pepMinSpecPct = "diagmine_pepMinSpecDiff";
  private static final String PROP_diagMine_pepMinFoldChange = "diagmine_pepMinFoldChange";
  private static final String PROP_diagMine_fragMinSpecPct = "diagmine_fragMinSpecDiff";
  private static final String PROP_diagMine_fragMinFoldChange = "diagmine_fragMinFoldChange";
  private static final String PROP_diagMine_fragMinPropensity = "diagmine_fragMinPropensity";
  private static final String PROP_diagMine_minIons = "diagmine_minPeps";
  private static final String PROP_run_diagExtract_mode = "run_diagextract_mode";

  private final List<BalloonTip> balloonTips = new ArrayList<>();
  private JCheckBox checkRun;
  private JPanel pContent;
  private JPanel pTop;
  private JPanel pIterativeLocalization;
  private JPanel pDiagnosticMining;
  private JPanel pDiagnosticExtraction;
  private UiText uiTextVarMods;

  private static Map<String, Function<String, String>> CONV_TO_GUI = new HashMap<>();
  private static Map<String, Function<String, String>> CONV_TO_FILE = new HashMap<>();
  private ButtonGroup btnGroupNormalizations;
  private JRadioButton btnNormPsm;
  private JRadioButton btnNormScan;
  private ButtonGroup btnGroupAnnotations;
  private JRadioButton btnAnnUnimod;
  private JRadioButton btnAnnCommon;
  private JRadioButton btnAnnCustom;
  private JRadioButton btnAnnGlyco;
  private UiText uiTextAnnotationFile;
  private UiText uiTextLocalizationAAs;
  private UiCheck uiCheckDiagnostic; //this is for KNOWN diagnostic ions and glyco mode
  private UiCheck uiCheckDiagnosticMining;
  private JPanel pDiagnosticMiningContent;
  private JPanel pDiagnosticKnownContent;
  private UiCheck uiCheckIterativeLocalization;
  private JPanel pIterativeLocalizationContent;

  private static String itos(int i) {
    return Integer.toString(i);
  }

  static {
    CONV_TO_GUI.put(PROP_peakpicking_mass_units, s -> MassTolUnits.fromFileToUi(s).name());
    CONV_TO_GUI.put(PROP_precursor_mass_units, s -> MassTolUnits.fromFileToUi(s).name());
    CONV_TO_GUI.put(PROP_iontype_a, s -> s.equalsIgnoreCase("1") ? "true" : "false");
    CONV_TO_GUI.put(PROP_iontype_x, s -> s.equalsIgnoreCase("1") ? "true" : "false");
    CONV_TO_GUI.put(PROP_iontype_b, s -> s.equalsIgnoreCase("1") ? "true" : "false");
    CONV_TO_GUI.put(PROP_iontype_y, s -> s.equalsIgnoreCase("1") ? "true" : "false");
    CONV_TO_GUI.put(PROP_iontype_c, s -> s.equalsIgnoreCase("1") ? "true" : "false");
    CONV_TO_GUI.put(PROP_iontype_z, s -> s.equalsIgnoreCase("1") ? "true" : "false");

    CONV_TO_FILE.put(PROP_peakpicking_mass_units, s -> itos(MassTolUnits.valueOf(s).valueInParamsFile()));
    CONV_TO_FILE.put(PROP_precursor_mass_units, s -> itos(MassTolUnits.valueOf(s).valueInParamsFile()));
    CONV_TO_FILE.put(PROP_iontype_a, s -> s.equalsIgnoreCase("true") ? "1" : "0");
    CONV_TO_FILE.put(PROP_iontype_x, s -> s.equalsIgnoreCase("true") ? "1" : "0");
    CONV_TO_FILE.put(PROP_iontype_b, s -> s.equalsIgnoreCase("true") ? "1" : "0");
    CONV_TO_FILE.put(PROP_iontype_y, s -> s.equalsIgnoreCase("true") ? "1" : "0");
    CONV_TO_FILE.put(PROP_iontype_c, s -> s.equalsIgnoreCase("true") ? "1" : "0");
    CONV_TO_FILE.put(PROP_iontype_z, s -> s.equalsIgnoreCase("true") ? "1" : "0");
  }

  public PtmshepherdPanel() {
    super();
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

  @Subscribe
  public void on(MessageLoadShepherdDefaults m) {
    log.debug("Got MessageLoadShepherdDefaults: {}", m);
    loadDefaults(2, m.type);
  }

  @Subscribe(sticky = true, threadMode = ThreadMode.MAIN_ORDERED)
  public void on(NoteConfigPtmShepherd m) {
    updateEnabledStatus(this, m.isValid());
  }

  private Properties loadBaseDefaults() {
    return PropertiesUtils.loadPropertiesLocal(PtmshepherdParams.class, PtmshepherdParams.DEFAULT_PROPERTIES_FN);
  }

  public Map<String, String> toPtmsParamsMap() {
    Map<String, String> map0 = super.toMap();
    Map<String, String> map1 = MapUtils.remapKeys(map0, s -> StringUtils.stripLeading(s, PREFIX));

    //"annotation-unimod", "annotation-common", "annotation-custom"
    Map<String, String> map2 = new HashMap<>();
    for (Entry<String, String> kv : map1.entrySet()) {

      if (kv.getKey().startsWith(PROP_annotation_file)) { // special treatment of radio buttons for annotations
        continue;
      } else if (kv.getKey().startsWith("annotation-unimod") && kv.getValue().equalsIgnoreCase("true")) {
        map2.put(PROP_annotation_file, "unimod");
      } else if (kv.getKey().startsWith("annotation-common") && kv.getValue().equalsIgnoreCase("true")) {
        map2.put(PROP_annotation_file, "common");
      } else if (kv.getKey().startsWith("annotation-glyco") && kv.getValue().equalsIgnoreCase("true")) {
        map2.put(PROP_annotation_file, map1.get(PROP_annotation_file));
      } else if (kv.getKey().startsWith("annotation-custom") && kv.getValue().equalsIgnoreCase("true")) {
        map2.put(PROP_annotation_file, map1.get(PROP_annotation_file));
      } else {  // copy everything else
        map2.put(kv.getKey(), kv.getValue());
      }

      if (kv.getKey().startsWith("normalization-scans") && kv.getValue().equalsIgnoreCase("true")) {
        map2.put(PROP_histo_normalizeTo, "scans");
      } else if (kv.getKey().startsWith("normalization-psms") && kv.getValue().equalsIgnoreCase("true")) {
        map2.put(PROP_histo_normalizeTo, "psms");
      }
    }

    // remap remaining values
    Map<String, String> map3 = MapUtils.remapValues(map2, (k, v) -> CONV_TO_FILE.getOrDefault(k, Function.identity()).apply(v));
    return map3;
  }

  private void loadDefaults(int debugInvocationId, SearchTypeProp type) {
    try {
      Map<String, Component> comps = SwingUtils.mapComponentsByName(this, true);

      Properties props = PropertiesUtils
          .loadPropertiesLocal(PtmshepherdParams.class, PtmshepherdParams.configFn(type.name()));
      if (props == null) {
        log.warn("No PTMShepherd default config file for type [{}]", type.name());
        return;
      }
      Map<String, String> asMap = PropertiesUtils.toMap(props);


      // special treatment of annotations
      btnGroupAnnotations.clearSelection();
      String annotations = asMap.get(PROP_annotation_file);
      if (annotations == null) {
        btnAnnUnimod.setSelected(true);
      } else if (annotations.equalsIgnoreCase("unimod")) {
        btnAnnUnimod.setSelected(true);
        asMap.put(PROP_annotation_file, "");
      } else if (annotations.equalsIgnoreCase("common")) {
        btnAnnCommon.setSelected(true);
        asMap.put(PROP_annotation_file, "");
      } else if (annotations.equalsIgnoreCase("glyco")) {
        btnAnnGlyco.setSelected(true);
        asMap.put(PROP_annotation_file, "");
      } else if (StringUtils.isNotBlank(annotations)) {
        btnAnnCustom.setSelected(true);
        uiTextAnnotationFile.setText(annotations);
      }


      asMap = MapUtils.remapValues(asMap, (k,v) -> CONV_TO_GUI.getOrDefault(k, Function.identity()).apply(v));
      asMap = MapUtils.remapKeys(asMap, k -> StringUtils.prependOnce(k, PREFIX));

      List<String> intersect = MapUtils.keysIntersection(asMap, comps).collect(Collectors.toList());
      if (intersect.isEmpty()) {
        log.error("PTMS panel loadDefaults({}) - empty keyset intersection between panel and loaded properties.\n"
                + "Keys in panel: {}\n"
                + "Keys in loaded props: {}", debugInvocationId,
            MapUtils.keysDiffRight(asMap, comps).collect(Collectors.toList()),
            MapUtils.keysDiffLeft(asMap, comps).collect(Collectors.toList()));
      } else {
        log.debug("PTMS panel loading defaults, key intersection: {}", intersect);
      }

      SwingUtils.valuesSet(this, asMap);

    } catch (Exception e) {
      log.error("Error loading shepherd defaults", e);
      SwingUtils.showErrorDialogWithStacktrace(e, this);
    }
  }

  @Subscribe
  public void on(MessageSearchType m) {
    switch (m.type) {
      case open:
      case offset:
      case glyco:
        checkRun.setSelected(true);
        loadDefaults(3, m.type);
        break;
      case closed:
      case nonspecific:
        checkRun.setSelected(false);
        break;
      default:
        throw new IllegalStateException("Not covered enum option: " + m.type.name());
    }
  }

  private JPanel createPanelTop() {
    JPanel pTop = new JPanel(new MigLayout(new LC().fillX()));

    checkRun = new UiCheck("Run PTM-Shepherd", null, false);
    checkRun.setName("run-shepherd");
    checkRun.addActionListener(e -> {
      final boolean isSelected = checkRun.isSelected();
      enablementMapping.put(pContent, isSelected);
      updateEnabledStatus(pContent, isSelected);
    });

    JLabel labelDefaults = new JLabel("Defaults for:");
    final LinkedHashMap<String, SearchTypeProp> defaults = new LinkedHashMap<>();
    defaults.put("Open Search", SearchTypeProp.open);
    defaults.put("Offset Search", SearchTypeProp.offset);
    defaults.put("Glyco Search", SearchTypeProp.glyco);
    final UiCombo uiComboDefaults = UiUtils.createUiCombo(new ArrayList<>(defaults.keySet()));
    uiComboDefaults.addItemListener(e -> {
      SearchTypeProp type = defaults.get((String) uiComboDefaults.getSelectedItem());
      Bus.post(new MessageLoadShepherdDefaults(type));
    });

    FormEntry feExtendedOut = new FormEntry(PROP_output_extended, "not-shown",
            new UiCheck("Extended output", null, false),
            "<html>Write additional files with more detailed information.");

    JLabel imageLabel = new JLabel();
    try {
      BufferedImage image = ImageIO.read(Objects.requireNonNull(getClass().getResource("/org/nesvilab/fragpipe/icons/ptm-s_logo.png")));
      imageLabel = new JLabel(new ImageIcon(image));
    } catch (Exception ex) {
      ex.printStackTrace();
    }

    mu.add(pTop, checkRun);
    mu.add(pTop, imageLabel, mu.ccR()).gapRight("50").wrap();

    mu.add(pTop, labelDefaults).split(4);
    mu.add(pTop, uiComboDefaults);
    mu.add(pTop, feExtendedOut.comp).wrap();

    return pTop;
  }

  private JPanel createPanelDiagnosticExtraction() {
    JPanel p = mu.newPanel("Diagnostic Feature Extraction", mu.lcFillXNoInsetsTopBottom());
    pDiagnosticKnownContent = mu.newPanel(null, true);

    // known diag ion params
    uiCheckDiagnostic = UiUtils.createUiCheck("Extract known diagnostic ions from spectra", false);
    uiCheckDiagnostic.setName(PROP_run_diagExtract_mode);
    uiCheckDiagnostic.setToolTipText("Look for the ions listed below in spectra. Note: required for glycan assignment");

    // labile/glyco main params
    FormEntry feDiagnosticFragmentMasses = mu.feb(PROP_diag_ions, UiUtils.uiTextBuilder().create())
            .label("Diagnostic fragment masses")
            .tooltip("Checked for directly in the MS2 spectrum. Assumed to have a +1 charge state. "
                    + "Space, comma, or slash separated values accepted.").create();
    FormEntry feYIonMasses = mu.feb(PROP_cap_y_ions, UiUtils.uiTextBuilder().create())
            .label("Peptide remainder masses")
            .tooltip("Partially fragmented modification mass added to peptide mass and searched for in MS2 spectrum. "
                    + "Space, comma, or slash separated values accepted.").create();
    FormEntry feRemainderMasses = mu.feb(PROP_remainder_masses, UiUtils.uiTextBuilder().create())
            .label("Fragment remainder masses")
            .tooltip("Partial modification masses localized to the peptide sequence. "
                    + "Space, comma, or slash separated values accepted.").create();
    mu.add(pDiagnosticKnownContent, feYIonMasses.label(), mu.ccR());
    mu.add(pDiagnosticKnownContent, feYIonMasses.comp).spanX().growX().pushX().wrap();
    mu.add(pDiagnosticKnownContent, feDiagnosticFragmentMasses.label(), mu.ccR());
    mu.add(pDiagnosticKnownContent, feDiagnosticFragmentMasses.comp).spanX().growX().pushX().wrap();
    mu.add(pDiagnosticKnownContent, feRemainderMasses.label(), mu.ccR());
    mu.add(pDiagnosticKnownContent, feRemainderMasses.comp).spanX().growX().pushX().wrap();

    mu.add(p, uiCheckDiagnostic).spanX().wrap();
    mu.add(p, pDiagnosticKnownContent).growX().wrap();

    return p;
  }

  private JPanel createPanelIterativeLocalization() {
    JPanel p = mu.newPanel("Iterative Localization of PSMs (experimental feature)",  mu.lcFillXNoInsetsTopBottom());
    pIterativeLocalizationContent = mu.newPanel(null, true);

    // enabling checkbox
    uiCheckIterativeLocalization = UiUtils.createUiCheck("Iterative localization of PSMs (experimental feature)", false);
    uiCheckIterativeLocalization.setName(PROP_iterloc_mode);
    uiCheckIterativeLocalization.setToolTipText("Perform PSM-level iterative localization (experimental feature)");

    // main Params
    FormEntry feIterLocMaxEpoch = new FormEntry(PROP_iterloc_maxEpoch, "Max epochs",
            new UiSpinnerInt(100, 1, 1000, 1, 5),
            "<html>Maximum number of epochs before stopping localization");

    // add to iterloc content
    mu.add(pIterativeLocalizationContent, feIterLocMaxEpoch.label(), mu.ccR());
    mu.add(pIterativeLocalizationContent, feIterLocMaxEpoch.comp).spanX().pushX().wrap();

    // add check box and iterloc content to panel
    mu.add(p, uiCheckIterativeLocalization).spanX().wrap();
    mu.add(p, pIterativeLocalizationContent).growX().wrap();

    return p;
  }

  private JPanel createPanelDiagnosticMining() {
    JPanel p = mu.newPanel("Diagnostic Feature Discovery", mu.lcFillXNoInsetsTopBottom());
    pDiagnosticMiningContent = mu.newPanel(null, true);
    // pDiagnosticKnownContent = mu.newPanel(null, mu.lcFillXNoInsetsTopBottom());

    // label diagnostic ion mining params
    uiCheckDiagnosticMining = UiUtils.createUiCheck("Mine for diagnostic ions and fragments", false);
    uiCheckDiagnosticMining.setName(PROP_run_diagMine_mode);
    uiCheckDiagnosticMining.setToolTipText("Look for new diagnostic ions and fragments for each modification");

    // Global parameters
    FormEntry feDiagMinIons = new FormEntry(PROP_diagMine_minIons, "Min. peptide ions per MS1 delta mass peak",
            new UiSpinnerInt(25, 1, 1000, 1, 5),
            "<html>Number of peptide ions required for PTM to undergo diagnostic ion mining");
    mu.add(pDiagnosticMiningContent, feDiagMinIons.label(), mu.ccR());
    mu.add(pDiagnosticMiningContent, feDiagMinIons.comp).spanX().pushX().wrap();


    // Diagnostic ions
    JPanel p1 = mu.newPanel("Diagnostic ions", true);
    UiSpinnerDouble uiSpinnerDiagMinSpecPct = UiSpinnerDouble.builder(25.0, 0.0, 100.0, 0.1)
            .setFormat(new DecimalFormat("00.#")).setCols(5).create();
    FormEntry feDiagMinSpecPct = new FormEntry(PROP_diagMine_diagMinSpecPct, "Min. % of spectra with " +
            "ion", uiSpinnerDiagMinSpecPct,
            "<html>Used to filter what is considered a peak for downstream analyses.\n" +
                    "Portion of spectra that potential ion must be present in.");
    UiSpinnerDouble uiSpinnerDiagMinFoldChange = UiSpinnerDouble.builder(3.0, 0.0, 10.0, 0.01)
            .setFormat(new DecimalFormat("0.0#")).setCols(5).create();
    FormEntry feDiagMinFoldChange = new FormEntry(PROP_diagMine_diagMinFoldChange, "Min. intensity fold change",
            uiSpinnerDiagMinFoldChange,
            "<html>Used to filter what is considered a peak for downstream analyses.\n" +
                    "Ratio of avg. diagnostic ion intensity compared to its avg. intensity\n" +
                    "among unmodified spectra.");

    mu.add(p1, feDiagMinSpecPct.label(), mu.ccR());
    mu.add(p1, feDiagMinSpecPct.comp);
    mu.add(p1, feDiagMinFoldChange.label(), mu.ccR());
    mu.add(p1, feDiagMinFoldChange.comp).split(2).spanX().pushX().wrap();

    mu.add(pDiagnosticMiningContent, p1).spanX().growX().wrap();

    // Peptide ions
    JPanel p2 = mu.newPanel("Peptide ions", true);
    UiSpinnerDouble uiSpinnerPepMinSpecPct = UiSpinnerDouble.builder(25.0, 0.0, 100.0, 0.1)
            .setFormat(new DecimalFormat("00.#")).setCols(5).create();
    FormEntry fePepMinSpecPct = new FormEntry(PROP_diagMine_pepMinSpecPct, "Min. % of spectra with " +
            "ion", uiSpinnerPepMinSpecPct,
            "<html>Used to filter what is considered a peak for downstream analyses.\n" +
                    "Portion of spectra that potential peptide ion must be present in.");
    UiSpinnerDouble uiSpinnerPepMinFoldChange = UiSpinnerDouble.builder(3.0, 0.0, 10.0, 0.01)
            .setFormat(new DecimalFormat("0.0#")).setCols(5).create();
    FormEntry fePepMinFoldChange = new FormEntry(PROP_diagMine_pepMinFoldChange, "Min. intensity fold change",
            uiSpinnerPepMinFoldChange,
            "<html>Used to filter what is considered a peak for downstream analyses.\n" +
                    "Ratio of avg. summed peptide ion intensity compared to its avg. \n" +
                    "summed intensity among unmodified spectra.");

    mu.add(p2, fePepMinSpecPct.label(), mu.ccR());
    mu.add(p2, fePepMinSpecPct.comp);
    mu.add(p2, fePepMinFoldChange.label(), mu.ccR());
    mu.add(p2, fePepMinFoldChange.comp).split(2).spanX().pushX().wrap();

    mu.add(pDiagnosticMiningContent, p2).spanX().growX().wrap();

    // Fragment ions
    JPanel p3 = mu.newPanel("Fragment ions", true);
    UiSpinnerDouble uiSpinnerFragMinSpecPct = UiSpinnerDouble.builder(15.0, 0.0, 100.0, 0.1)
            .setFormat(new DecimalFormat("00.#")).setCols(5).create();
    FormEntry feFragMinSpecPct = new FormEntry(PROP_diagMine_fragMinSpecPct, "Min. % of spectra with " +
            "ion", uiSpinnerFragMinSpecPct,
            "<html>Used to filter what is considered a peak for downstream analyses.\n" +
                    "Portion of spectra that potential fragment ion must be present in.\n" +
                    "Spectra only count as a hit when there are >= \"Min ions per spec\" instances present.");
    FormEntry feFragMinIonsPerSpec = new FormEntry(PROP_diagMine_minIonsPerSpec, "Min. fragment ions per spec",
            new UiSpinnerInt(2, 1, 2, 1, 5),
            "<html>Reduce to make make fragment ion detection more sensitive by reducing the number\n" +
                    "of hits per spectrum required. Decrease this to increase sensitivity and noise.");
    UiSpinnerDouble uiSpinnerFragMinFoldChange = UiSpinnerDouble.builder(3.0, 0.0, 10.0, 0.01)
            .setFormat(new DecimalFormat("0.0#")).setCols(5).create();
    FormEntry feFragMinFoldChange = new FormEntry(PROP_diagMine_fragMinFoldChange, "Min. intensity fold change",
            uiSpinnerFragMinFoldChange,
            "<html>Used to filter what is considered a peak for downstream analyses.\n" +
                    "Ratio of avg. fragment ion intensity compared to its avg. \n" +
                    "intensity among unmodified spectra.");
    UiSpinnerDouble uiSpinnerFragMinPropensity = UiSpinnerDouble.builder(12.5, 0.0, 100.0, 0.1)
            .setFormat(new DecimalFormat("00.#")).setCols(5).create();
    FormEntry feFragMinPropensity = new FormEntry(PROP_diagMine_fragMinPropensity, "Min. fragment propensity",
            uiSpinnerFragMinPropensity,
            "<html>Used to filter what is considered a peak for downstream analyses.\n" +
                    "Portion of identified ions within the ion series that are shifted by the\n" +
                    "fragment ion mass.");

    mu.add(p3, feFragMinSpecPct.label(), mu.ccR());
    mu.add(p3, feFragMinSpecPct.comp);
    mu.add(p3, feFragMinFoldChange.label(), mu.ccR());
    mu.add(p3, feFragMinFoldChange.comp).split(2).spanX().pushX().wrap();
    mu.add(p3, feFragMinIonsPerSpec.label(), mu.ccR());
    mu.add(p3, feFragMinIonsPerSpec.comp);
    mu.add(p3, feFragMinPropensity.label(), mu.ccR());
    mu.add(p3, feFragMinPropensity.comp).split(2).spanX().pushX().wrap();

    mu.add(pDiagnosticMiningContent, p3).spanX().growX().wrap();

    mu.add(p, uiCheckDiagnosticMining).spanX().wrap();
    mu.add(p, pDiagnosticMiningContent).growX().wrap();

    return p;
  }


  private JPanel createPanelContent() {
    JPanel p =  mu.newPanel("PTM Profiling", mu.lcFillXNoInsetsTopBottom());

    FormEntry feHistoSmoothBins = new FormEntry(PROP_histo_smoothbins, "Smoothing factor",
        new UiSpinnerInt(2, 0, 5, 1, 5),
        "<html>Increase to make peakpicking less sensitive by distributing weight to adjacent histogram bins.\n" +
                "Histogram smoothing: 0 = No smoothing, 1 = smooth using +/-1 bin, etc.");

    UiSpinnerDouble uiSpinnerPromRatio = UiSpinnerDouble.builder(0.3,0.0,1.0, 0.1)
        .setFormat(new DecimalFormat("0.#")).setCols(5).create();
    FormEntry fePromRatio = new FormEntry(PROP_peakpicking_promRatio, "Prominence ratio", uiSpinnerPromRatio,
        "Used to filter what is considered a peak for downstream analyses.\n" +
                "Ratio of peak shoulder to peak height.");

    UiSpinnerDouble uiSpinnerWidth = UiSpinnerDouble.builder(0.002, 0.0, 500, 0.001)
        .setFormat(new DecimalFormat("0.####")).setCols(5).create();
    FormEntry feWidth = mu.feb(PROP_peakpicking_width, uiSpinnerWidth)
        .label("Peak picking width").tooltip("+/- signal width during peakpicking").create();
    FormEntry fePeakPickingUnits = mu.feb(PROP_peakpicking_mass_units, UiUtils.createUiCombo(MassTolUnits.values()))
        .tooltip("+/- signal width during peakpicking")
        .create();


    UiSpinnerDouble uiSpinnerPrecTol = UiSpinnerDouble.builder(0.01, 0.001, 1e6, 0.01)
        .setFormat(new DecimalFormat("0.###")).setCols(5).create();
    uiSpinnerPrecTol.setColumns(5);
    FormEntry fePrecTol = new FormEntry(PROP_precursor_tol, "Precursor tolerance", uiSpinnerPrecTol);
    FormEntry fePrecUnits = mu.fe(PROP_precursor_mass_units, UiUtils.createUiCombo(MassTolUnits.values()));


    FormEntry feMinPsms = new FormEntry(PROP_peakpicking_minPsm, "Peak minimum PSMs",
            new UiSpinnerInt(10, 0, 1000, 1, 5),
            "<html>Filters out mass shift peaks below the minimum threshold");

    btnGroupNormalizations = new ButtonGroup();
    btnNormPsm = new JRadioButton("PSMs", true);
    btnNormPsm.setName("normalization-psms");
    btnNormScan = new JRadioButton("MS2 scans", false);
    btnNormScan.setName("normalization-scans");
    btnGroupNormalizations.add(btnNormPsm);
    btnGroupNormalizations.add(btnNormScan);

    btnGroupAnnotations = new ButtonGroup();
    btnAnnUnimod = new JRadioButton("Unimod", true);
    btnAnnUnimod.setName("annotation-unimod");
    btnAnnCommon = new JRadioButton("Common mass shifts", false);
    btnAnnCommon.setName("annotation-common");
    btnAnnGlyco = new JRadioButton("Glyco mass shifts", false);
    btnAnnGlyco.setName("annotation-glyco");
    btnAnnCustom = new JRadioButton("Custom annotation file", false);
    btnAnnCustom.setName("annotation-custom");
    btnGroupAnnotations.add(btnAnnUnimod);
    btnGroupAnnotations.add(btnAnnCommon);
    btnGroupAnnotations.add(btnAnnGlyco);
    btnGroupAnnotations.add(btnAnnCustom);

    String tooltipAnnotationFile = "Custom mass shift annotation file. Will not map to Unimod if provided.";
    uiTextAnnotationFile = UiUtils.uiTextBuilder().create();
    FormEntry feAnnotationFile = mu.feb(PROP_annotation_file, uiTextAnnotationFile)
        .label("Custom mass shift annotation file").tooltip(tooltipAnnotationFile).create();
    JButton btnBrosweAnnotationFile = feAnnotationFile.browseButton("Browse", tooltipAnnotationFile,
        () -> FileChooserUtils.builder("Select custom mass shift annotation file")
            .approveButton("Select").mode(FcMode.FILES_ONLY).acceptAll(true).multi(false)
            .paths(Stream.of(Fragpipe.propsVarGet(PROP_custom_modlist_loc))).create(),
        paths -> {
          if (paths != null && !paths.isEmpty()) {
            String path = paths.get(0).toString();
            Fragpipe.propsVarSet(PROP_custom_modlist_loc, path);
            uiTextAnnotationFile.setText(path);
            btnAnnCustom.setSelected(true);
          }
        });

    UiCheck uiCheckAnnotateAssignedMods = UiUtils.createUiCheck("Include Variable Mods in PTM Profile", false);
    uiCheckAnnotateAssignedMods.setName(PROP_annotate_assigned_mods);
    uiCheckAnnotateAssignedMods.setToolTipText("If selected, variable mods (Assigned Modifications in the psm.tsv) will be included" +
            "in the PTM profile and modification summaries.");

    JPanel p1 = mu.newPanel(null, true);
    mu.border(p1, 1);

    mu.add(p1, feHistoSmoothBins.label(), mu.ccR());
    mu.add(p1, feHistoSmoothBins.comp);
    mu.add(p1, fePrecTol.label(), mu.ccR());
    mu.add(p1, fePrecTol.comp).split(2);
    mu.add(p1, fePrecUnits.comp).wrap();

    mu.add(p1, fePromRatio.label(), mu.ccR());
    mu.add(p1, fePromRatio.comp);
    mu.add(p1, feWidth.label(), mu.ccR());
    mu.add(p1, feWidth.comp).split(2);
    mu.add(p1, fePeakPickingUnits.comp);

    mu.add(p1, feMinPsms.label(), mu.ccR());
    mu.add(p1, feMinPsms.comp).wrap();


    mu.add(p1, new JLabel("Normalize data to: ")).spanX().split();
    mu.add(p1, btnNormPsm);
    mu.add(p1, btnNormScan);
    mu.add(p1, uiCheckAnnotateAssignedMods).wrap();

    mu.add(p, p1).spanX().growX().wrap();

    uiTextVarMods = new UiTextBuilder().text("").create();
    uiTextVarMods.addFocusListener(new FocusAdapter() {
      @Override
      public void focusLost(FocusEvent e) {
        super.focusLost(e);
        PtmshepherdPanel.this.validateForm();
      }

      @Override
      public void focusGained(FocusEvent e) {
        super.focusGained(e);
        PtmshepherdPanel.this.clearBalloonTips();
      }
    });


    FormEntry feVarMods = new FormEntry(PROP_varmod_masses, "Custom mass shifts", uiTextVarMods,
        "<html>Mass shifts to be added and prioritized during annotation,\n" +
                "use if modifications will be found overwhelmingly as variable mods, e.g. phospho-enriched data.<br/>\n"
            + "Comma-separated entries in the format \"&lt;name&gt;:&lt;mass&gt;\"<br/>\n"
            + "Example:<br/>\n"
            + "&nbsp;&nbsp;&nbsp;&nbsp;Phospho:79.9663, Failed_Carbamidomethylation:-57.02146");

    UiSpinnerDouble uiSpinnerAnnotTol = UiSpinnerDouble.builder(0.01, 0.001, 0.999, 0.01)
            .setFormat(new DecimalFormat("0.###")).setCols(5).create();
    FormEntry feAnnotTol = mu.feb(PROP_annotation_tol, uiSpinnerAnnotTol)
            .label("Annotation tolerance (Da)").tooltip("+/- distance from peak to annotated mass").create();

    JPanel p2 = mu.newPanel("Annotation", true);

    mu.add(p2, feAnnotTol.label(), mu.ccR());
    mu.add(p2, feAnnotTol.comp).spanX().wrap();

    mu.add(p2, feVarMods.label(), mu.ccR());
    mu.add(p2, feVarMods.comp).spanX().growX().pushX().wrap();

    mu.add(p2, new JLabel("Annotation source: ")).spanX().split();
    mu.add(p2, btnAnnUnimod);
    mu.add(p2, btnAnnCommon);
    mu.add(p2, btnAnnGlyco).wrap();

    mu.add(p2, btnAnnCustom, mu.ccR());
    mu.add(p2, feAnnotationFile.comp).split().spanX().growX().pushX();
    mu.add(p2, btnBrosweAnnotationFile).wrap();

    mu.add(p, p2).spanX().growX().wrap();

    // Label localization parameters
    FormEntry feIonA = mu.feb(PROP_iontype_a, UiUtils.createUiCheck("a", false)).create();
    FormEntry feIonX = mu.feb(PROP_iontype_x, UiUtils.createUiCheck("x", false)).create();
    FormEntry feIonB = mu.feb(PROP_iontype_b, UiUtils.createUiCheck("b", true)).create();
    FormEntry feIonY = mu.feb(PROP_iontype_y, UiUtils.createUiCheck("y", true)).create();
    FormEntry feIonC = mu.feb(PROP_iontype_c, UiUtils.createUiCheck("c", false)).create();
    FormEntry feIonZ = mu.feb(PROP_iontype_z, UiUtils.createUiCheck("z", false)).create();


    uiTextLocalizationAAs = UiUtils.uiTextBuilder().create();
    FormEntry feRestrictLoc = new FormEntry(PROP_restrict_loc, "Restrict localization to", uiTextLocalizationAAs,
            "<html>Restricts localization to specified residues.\n" +
                    "Includes glyco mode localization of remainder masses. Example: STYNP");

    UiCheck uiCheckUseMSFraggerLoc = UiUtils.createUiCheck("Use MSFragger Localization", false);
    uiCheckUseMSFraggerLoc.setName(PROP_use_msfragger_localization);
    uiCheckUseMSFraggerLoc.setToolTipText("Do not perform PTM localization, read the MSFragger delta mass localization instead. " +
            "Requires that localize_delta_mass was enabled in MSFragger");

    JPanel p3 = mu.newPanel("Amino acid propensity analysis", true);

    mu.add(p3, uiCheckUseMSFraggerLoc).spanX().wrap();
    mu.add(p3, feIonA.comp).spanX().split();
    mu.add(p3, feIonX.comp);
    mu.add(p3, feIonB.comp);
    mu.add(p3, feIonY.comp);
    mu.add(p3, feIonC.comp);
    mu.add(p3, feIonZ.comp).spanX().split().wrap();
    mu.add(p3, feRestrictLoc.label(), mu.ccR());
    mu.add(p3, feRestrictLoc.comp).spanX().growX().pushX().wrap();

    mu.add(p, p3).spanX().growX().wrap();

    UiSpinnerDouble uiSpinnerSpectraTol = UiSpinnerDouble.builder(20.0, 1.0, 1000.0, 1)
            .setFormat(new DecimalFormat("0.#")).setCols(5).create();
    FormEntry feSpectraTol = new FormEntry(PROP_spectra_ppmtol, "Fragment mass tolerance (PPM)",
            uiSpinnerSpectraTol);
    FormEntry feMaxFragCharge = mu
            .feb(PROP_spectra_maxfragcharge,
                    UiUtils.spinnerInt(2, 1, 100, 1).setCols(4).create())
            .label("Max fragment charge")
            .tooltip("max fragment charge for localization")
            .create();
    FormEntry feMaxPrecCharge = mu
            .feb(PROP_spectra_maxPrecCharge,
                    UiUtils.spinnerInt(4, 1, 100, 1).setCols(4).create())
            .label("Max precursor charge")
            .tooltip("max precursor charge (used only for spectra without a recorded precursor charge)")
            .create();
    FormEntry feCondPeaks = mu
            .feb(PROP_spectra_condPeaks,
                    UiUtils.spinnerInt(150, 1, 100000, 1).setCols(6).create())
            .label("Use top N peaks")
            .tooltip("Consider topN peaks per MS2 spectrum")
            .create();
    UiSpinnerDouble uiSpinnerSpectraRatio = UiSpinnerDouble.builder(0.0001, 0, 1, 0.0001)
            .setFormat(new DecimalFormat("0.#######")).setCols(10).create();
    FormEntry feSpectraRatio = new FormEntry(PROP_spectra_condRatio, "Min ratio",
            uiSpinnerSpectraRatio);

    JPanel p4 = mu.newPanel("Spectrum Preprocessing", true);

    mu.add(p4, feMaxFragCharge.label(), mu.ccR());
    mu.add(p4, feMaxFragCharge.comp);
    mu.add(p4, feSpectraTol.label(), mu.ccR());
    mu.add(p4, feSpectraTol.comp).wrap();
    mu.add(p4, feMaxPrecCharge.label(), mu.ccR());
    mu.add(p4, feMaxPrecCharge.comp);
    mu.add(p4, feCondPeaks.label(), mu.ccR());
    mu.add(p4, feCondPeaks.comp);
    mu.add(p4, feSpectraRatio.label(), mu.ccR());
    mu.add(p4, feSpectraRatio.comp).wrap();

    mu.add(p, p4).spanX().growX().wrap();

    return p;
  }

  @Override
  protected void init() {
    this.setLayout(new MigLayout(new LC().fillX()));
    pTop = createPanelTop();

    // 4 Sub-panels within main PTM-S panel: PTM-Profiling, Iterative Localization, Diagnostic Ion Discovery,
    // Diagnostic Ion Exctraction
    pContent = createPanelContent();
    pDiagnosticMining = createPanelDiagnosticMining();
    pDiagnosticExtraction = createPanelDiagnosticExtraction();
    pIterativeLocalization = createPanelIterativeLocalization();

    mu.add(this, pTop).spanX().growX().wrap();
    mu.add(this, pContent).spanX().growX().wrap();
    mu.add(this, pDiagnosticMining).spanX().growX().wrap();
    mu.add(this, pDiagnosticExtraction).spanX().growX().wrap();
    mu.add(this, pIterativeLocalization).spanX().growX().wrap();
  }

  @Override
  protected void initMore() {
    super.initMore();
    loadDefaults(1, SearchTypeProp.open); // pre-populate, but only after renaming has happened in super.initMore()

    SwingUtils.setEnablementUpdater(this, pIterativeLocalization, checkRun);
    SwingUtils.setEnablementUpdater(this, pDiagnosticMining, checkRun);
    SwingUtils.setEnablementUpdater(this, pDiagnosticExtraction, checkRun);

    // enable/disable second-level parameter areas when boxes are checked
    SwingUtils.setEnablementUpdater(this, pIterativeLocalizationContent, uiCheckIterativeLocalization);
    SwingUtils.setEnablementUpdater(this, pDiagnosticMiningContent, uiCheckDiagnosticMining);
    SwingUtils.setEnablementUpdater(this, pDiagnosticKnownContent, uiCheckDiagnostic);
  }

  @Override
  public boolean isRun() {
    return SwingUtils.isEnabledAndChecked(checkRun);
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

  public boolean validateForm() {

    Pattern reVarMods = Pattern.compile("[^\\s]+:-?\\d+(?:\\.\\d+)?(?:\\s*,\\s*[^\\s]+:-?\\d+(?:\\.\\d+)?)*");
    String text = uiTextVarMods.getNonGhostText().trim();
    boolean ok = true;
    if (!StringUtils.isNullOrWhitespace(text) && !reVarMods.matcher(text).matches()) {
      BalloonTip tip = new BalloonTip(uiTextVarMods,
          "<html>Does not match allowed format \"&lt;name&gt;:&lt;mass&gt;\"");
      tip.setVisible(true);
      balloonTips.add(tip);
      ok = false;
    }
    return ok;
  }
}
