package com.dmtavt.fragpipe.tools.ptmshepherd;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.api.Bus;
import com.dmtavt.fragpipe.api.SearchTypeProp;
import com.dmtavt.fragpipe.messages.MessageLoadShepherdDefaults;
import com.dmtavt.fragpipe.messages.MessageSearchType;
import com.dmtavt.fragpipe.messages.NoteConfigPtmShepherd;
import com.dmtavt.fragpipe.tools.enums.MassTolUnits;
import com.github.chhh.utils.MapUtils;
import com.github.chhh.utils.PropertiesUtils;
import com.github.chhh.utils.StringUtils;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.swing.FileChooserUtils;
import com.github.chhh.utils.swing.FileChooserUtils.FcMode;
import com.github.chhh.utils.swing.FormEntry;
import com.github.chhh.utils.swing.JPanelBase;
import com.github.chhh.utils.swing.UiCheck;
import com.github.chhh.utils.swing.UiCombo;
import com.github.chhh.utils.swing.UiSpinnerDouble;
import com.github.chhh.utils.swing.UiSpinnerInt;
import com.github.chhh.utils.swing.UiText;
import com.github.chhh.utils.swing.UiUtils;
import com.github.chhh.utils.swing.UiUtils.UiTextBuilder;
import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.ItemSelectable;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.function.Function;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.border.TitledBorder;
import javax.swing.filechooser.FileFilter;
import javax.swing.filechooser.FileNameExtensionFilter;

import net.java.balloontip.BalloonTip;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.ThreadMode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class PtmshepherdPanel extends JPanelBase {
  private static final Logger log = LoggerFactory.getLogger(PtmshepherdPanel.class);
  private static final String PREFIX = "ptmshepherd.";

  public static final String PROP_threads = "threads";
  public static final String PROP_histo_bindivs = "histo_bindivs";
  public static final String PROP_histo_smoothbins = "histo_smoothbins";
  public static final String PROP_histo_normalizeTo = "histo_normalizeTo";
  public static final String PROP_peakpicking_promRatio = "peakpicking_promRatio";
  public static final String PROP_peakpicking_width = "peakpicking_width";
  public static final String PROP_peakpicking_mass_units = "peakpicking_mass_units";
  public static final String PROP_peakpicking_background = "peakpicking_background";
  public static final String PROP_peakpicking_topN = "peakpicking_topN";
  public static final String PROP_peakpicking_minPsm = "peakpicking_minPsm";

  public static final String PROP_precursor_mass_units = "precursor_mass_units";
  public static final String PROP_precursor_tol = "precursor_tol";
  public static final String PROP_annotation_tol = "annotation_tol";

  public static final String PROP_spectra_ppmtol = "spectra_ppmtol";
  public static final String PROP_spectra_condPeaks = "spectra_condPeaks";
  public static final String PROP_spectra_condRatio = "spectra_condRatio";
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
  private static final String PROP_glyco_mode = "glyco_mode";
  private static final String PROP_cap_y_ions = "cap_y_ions";
  private static final String PROP_diag_ions = "diag_ions";
  private static final String PROP_remainder_masses = "remainder_masses";
  private static final String PROP_spectra_maxfragcharge = "spectra_maxfragcharge";
  private static final String PROP_restrict_loc = "localization_allowed_res";

  private static final String PROP_custom_modlist_loc = "ptmshepherd.path.modlist";

  private static final String PROP_run_glycan_assignment = "assign_glycans";
  private static final String PROP_glycan_fdr = "glyco_fdr";
  private static final String PROP_glyco_mass_error_ppm = "glyco_ppm_tol";
  private static final String PROP_glyco_isotope_error_low = "glyco_isotope_min";
  private static final String PROP_glyco_isotope_error_high = "glyco_isotope_max";
  private static final String PROP_adduct_names = "glyco_adducts";
  private static final String PROP_max_adducts = "max_adducts";
  private static final String PROP_neuac_probs = "prob_neuacOx";
  private static final String PROP_neugc_probs = "prob_neugcOx";
  private static final String PROP_fucOx_probs = "prob_dhexOx";
  private static final String PROP_phospho_probs = "prob_phosphoOx";
  private static final String PROP_sulfo_probs = "prob_sulfoOx";
  private static final String PROP_regY_probs = "prob_regY";
  private static final String PROP_fucY_probs = "prob_dhexY";
  private static final String PROP_decoy_type = "decoy_type";
  private static final String PROP_glycan_database = "glycodatabase";
  private static final String PROP_remove_glyco_deltamass = "remove_glycan_delta_mass";
  private static final String PROP_print_decoys = "print_decoys";

  private static final String PROP_glycan_to_assigned_mods = "put_glycans_to_assigned_mods";
  private static final String PROP_nglyco_mode = "n_glyco";

  private final List<BalloonTip> balloonTips = new ArrayList<>();
  private JCheckBox checkRun;
  private JPanel pContent;
  private JPanel pTop;
  private JPanel pRegularShepherd;
  private JPanel pDiagnosticDiscovery;
  private JPanel pGlycanAssignment;
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
  private UiText uiTextGlycanDBFile;
  private UiCheck uiCheckDiagnostic;
  private UiCheck uiCheckGlycoAssign;
  private UiCheck uiCheckGlycoAdvParams;
  private JPanel pDiagnosticConent;
  private JPanel pGlycoAssignContent;
  private JPanel pGlycoAdvParams;

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
    if (m.doAskUser) {
      int answer = SwingUtils.showConfirmDialog(this, new JLabel(String.format("<html>Load PTMShepherd defaults for %s?", m.type.name())));
      if (JOptionPane.OK_OPTION != answer) {
        log.debug("User cancelled Loading Shepherd defaults");
        return;
      }
    }
    loadDefaults(2, m.type);
  }

  @Subscribe(sticky = true, threadMode = ThreadMode.MAIN_ORDERED)
  public void on(NoteConfigPtmShepherd m) {
    updateEnabledStatus(this, m.isValid());
    checkRun.setSelected(false);
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
    JPanel p = mu.newPanel(new LC());
    mu.borderEmpty(p);

    checkRun = new UiCheck("Run PTM-Shepherd", null, false);
    checkRun.setName("run-shepherd");
    checkRun.addActionListener(e -> {
      final boolean isSelected = checkRun.isSelected();
      enablementMapping.put(pContent, isSelected);
      updateEnabledStatus(pContent, isSelected);
    });
    p.add(checkRun, new CC().alignX("left"));

    JLabel labelDefaults = new JLabel("Defaults for:");
    final LinkedHashMap<String, SearchTypeProp> defaults = new LinkedHashMap<>();
    defaults.put("Open Search", SearchTypeProp.open);
    defaults.put("Offset Search", SearchTypeProp.offset);
    defaults.put("Glyco Search", SearchTypeProp.glyco);
    final UiCombo uiComboDefaults = UiUtils.createUiCombo(new ArrayList<>(defaults.keySet()));
    JButton btnLoadDefaults = UiUtils
        .createButton("Load", "Load PTM-Shepherd settings for given search type", e -> {
          SearchTypeProp type = defaults.get(uiComboDefaults.getSelectedItem());
          Bus.post(new MessageLoadShepherdDefaults(true, type));
        });

    FormEntry feExtendedOut = new FormEntry(PROP_output_extended, "not-shown",
            new UiCheck("Extended output", null, false),
            "<html>Write additional files with more detailed information.");

    mu.add(p, labelDefaults).split().spanX();
    mu.add(p, uiComboDefaults);
    mu.add(p, btnLoadDefaults);
    mu.add(p, feExtendedOut.comp).pushX().wrap();

    return p;
  }

  private JPanel createPanelDiagnostic() {
    JPanel p = mu.newPanel("Diagnostic Ion Discovery", mu.lcFillXNoInsetsTopBottom());
    pDiagnosticConent = mu.newPanel(null, mu.lcFillXNoInsetsTopBottom());

    uiCheckDiagnostic = UiUtils.createUiCheck("Enable Diagnostic Ion Search", false);
    uiCheckDiagnostic.setName("glyco_mode");

    // labile/glyco main params
    FormEntry feYIonMasses = mu.feb(PROP_cap_y_ions, UiUtils.uiTextBuilder().create())
            .label("Y Ion Masses")
            .tooltip("Partially fragmented modification mass added to peptide mass and searched for in MS2 spectrum. "
                    + "Space, comma, or slash separated values accepted.").create();
    FormEntry feDiagnosticFragmentMasses = mu.feb(PROP_diag_ions, UiUtils.uiTextBuilder().create())
            .label("Diagnostic Fragment Masses")
            .tooltip("Checked for directly in the MS2 spectrum. Assumed to have a +1 charge state. "
                    + "Space, comma, or slash separated values accepted.").create();
    FormEntry feRemainderMasses = mu.feb(PROP_remainder_masses, UiUtils.uiTextBuilder().create())
            .label("Remainder Masses")
            .tooltip("Partial modification masses localized to the peptide sequence. "
                    + "Space, comma, or slash separated values accepted.").create();
    mu.add(pDiagnosticConent, feYIonMasses.label(), mu.ccR());
    mu.add(pDiagnosticConent, feYIonMasses.comp).spanX().growX().pushX().wrap();
    mu.add(pDiagnosticConent, feDiagnosticFragmentMasses.label(), mu.ccR());
    mu.add(pDiagnosticConent, feDiagnosticFragmentMasses.comp).spanX().growX().pushX().wrap();
    mu.add(pDiagnosticConent, feRemainderMasses.label(), mu.ccR());
    mu.add(pDiagnosticConent, feRemainderMasses.comp).spanX().growX().pushX().wrap();

    mu.add(p, uiCheckDiagnostic).spanX().wrap();
    mu.add(p, pDiagnosticConent).growX().wrap();
    return p;
  }

  private JPanel createpanelGlycanAssignment() {
    JPanel p = mu.newPanel("Glycan Assignment and FDR", mu.lcFillXNoInsetsTopBottom());

    // glycan assignment params
    pGlycoAssignContent = mu.newPanel(null, mu.lcFillXNoInsetsTopBottom());
    pGlycoAdvParams = mu.newPanel(null, mu.lcFillXNoInsetsTopBottom());

    uiCheckGlycoAssign = UiUtils.createUiCheck("Assign Glycans with FDR", true);
    uiCheckGlycoAssign.setName("assign_glycans");
    uiCheckGlycoAdvParams = UiUtils.createUiCheck("Edit Advanced Parameters", false);
    uiCheckGlycoAdvParams.setName("adv_params");

    UiSpinnerDouble uiSpinnerGlycanFDR = UiSpinnerDouble.builder(0.01, 0, 1.0, 0.01)
            .setFormat(new DecimalFormat("0.00#")).setCols(3).create();
    FormEntry feGlycanFDR = new FormEntry(PROP_glycan_fdr, "Glycan FDR", uiSpinnerGlycanFDR,
            "Glycan assignment FDR. Default 0.01 (1%)\n");

    UiSpinnerDouble uiSpinnerGlycanMassErr = UiSpinnerDouble.builder(50.0, 0.0, 10000.0, 5.0)
            .setFormat(new DecimalFormat("0.#")).setCols(5).create();
    FormEntry feGlycanMassErr = new FormEntry(PROP_glyco_mass_error_ppm, "Glycan mass tolerance (ppm)", uiSpinnerGlycanMassErr,
            "Mass tolerance for finding possible glycan candidates to consider in glycan assignment (ppm).\n");

    FormEntry feGlycanIsotopesLow = new FormEntry(PROP_glyco_isotope_error_low, "Isotope Error Range Min:",
            new UiSpinnerInt(-1, -5, 0, 1, 3),
            "Lowest isotope error to consider. Allowed isotope errors will go from this value to Isotope Error Range Max (inclusive).");
    FormEntry feGlycanIsotopesHigh = new FormEntry(PROP_glyco_isotope_error_high, "Max:",
            new UiSpinnerInt(3, 0, 5, 1, 3),
            "Highest isotope error to consider. Allowed isotope errors will go from Isotope Error Range Min to this value (inclusive).");

    FormEntry feAdductNames = mu.feb(PROP_adduct_names, UiUtils.uiTextBuilder().create())
            .label("Adduct Type(s)")
            .tooltip("Added to possible glycan compositions as noncovalent adducts. "
                    + "Space, comma, or slash separated values accepted. " +
                    "Possible values: NH3, Na, Fe3, Fe2, Al, Ca").create();
    FormEntry feMaxAdducts = new FormEntry(PROP_max_adducts, "Max Adducts",
            new UiSpinnerInt(0, 0, 5, 1, 1),
            "Maximum number of each specified adduct to allow");

    String tooltipGlycanDBFile = "Custom glycan database file (.glyc). Will use internal default N-glycan list if not provided.";
    uiTextGlycanDBFile = UiUtils.uiTextBuilder().create();
    List<FileFilter> glycFilters = new ArrayList<>();
    FileFilter filter = new FileNameExtensionFilter("Glycan Database file", "glyc", ".glyc");
    glycFilters.add(filter);
    FormEntry feGlycanDBFile = mu.feb(PROP_glycan_database, uiTextGlycanDBFile)
            .label("Custom Glycan Database").tooltip(tooltipGlycanDBFile).create();
    JButton btnBrosweGlycanDBFile = feGlycanDBFile.browseButton("Browse", tooltipGlycanDBFile,
            () -> FileChooserUtils.builder("Select custom glycan database file")
                    .approveButton("Select").mode(FcMode.FILES_ONLY).acceptAll(false).multi(false).filters(glycFilters)
                    .paths(Stream.of(Fragpipe.propsVarGet(PROP_glycan_database))).create(),
            paths -> {
              if (paths != null && !paths.isEmpty()) {
                String path = paths.get(0).toString();
                Fragpipe.propsVarSet(PROP_glycan_database, path);
                uiTextGlycanDBFile.setText(path);
              }
            });

    FormEntry feNeuAcProbs = mu.feb(PROP_neuac_probs, UiUtils.uiTextBuilder().create())
            .label("NeuAc Oxonium Ratios")
            .tooltip("Likelihood ratios for NeuAc oxonium ions. Hit ratio, miss ratio, expected intensity, separated by commas. " +
                    "Default 2,0.05,0.2").create();
    FormEntry feNeuGcProbs = mu.feb(PROP_neugc_probs, UiUtils.uiTextBuilder().create())
            .label("NeuGc Oxonium Ratios")
            .tooltip("Likelihood ratios for NeuGc oxonium ions. Hit ratio, miss ratio, expected intensity, separated by commas. " +
                    "Default 2,0.05,0.2").create();
    FormEntry feFucOxProbs = mu.feb(PROP_fucOx_probs, UiUtils.uiTextBuilder().create())
            .label("Fucose Oxonium Ratios")
            .tooltip("Likelihood ratios for Fucose oxonium ions. Hit ratio, miss ratio, expected intensity, separated by commas. " +
                    "Default 2,0.5,0.1").create();
    FormEntry fePhosphoProbs = mu.feb(PROP_phospho_probs, UiUtils.uiTextBuilder().create())
            .label("Phospho Oxonium Ratios")
            .tooltip("Likelihood ratios for Phospho oxonium ions. Hit ratio, miss ratio, expected intensity, separated by commas. " +
                    "Default 2,0.05,0.2").create();
    FormEntry feSulfoProbs = mu.feb(PROP_sulfo_probs, UiUtils.uiTextBuilder().create())
            .label("Sulfo Oxonium Ratios")
            .tooltip("Likelihood ratios for Sulfo oxonium ions. Hit ratio, miss ratio, expected intensity, separated by commas. " +
                    "Default 2,0.1,0.1").create();
    FormEntry feRegYProbs = mu.feb(PROP_regY_probs, UiUtils.uiTextBuilder().create())
            .label("Y-ion Ratios")
            .tooltip("Likelihood ratios for Y-ions not containing Fucose. Hit ratio, miss ratio, separated by commas. " +
                    "Default 5,0.5").create();
    FormEntry feFucYProbs = mu.feb(PROP_fucY_probs, UiUtils.uiTextBuilder().create())
            .label("Fucose Y-ion Ratios")
            .tooltip("Likelihood ratios for for Y-ions containing Fucose. Hit ratio, miss ratio, separated by commas. " +
                    "Default 2,0.5").create();
    FormEntry feDecoyType = new FormEntry(PROP_decoy_type, "Decoy Type",
            new UiSpinnerInt(1, 0, 3, 1, 1),
            "How to generate decoy glycan intact mass.\n " +
                    "0: Random mass shift within +/- 3 Da\n" +
                    "1: Random mass shift within glycan mass error tolerance, random isotope error (DEFAULT)\n" +
                    "2: Random mass shift within glycan mass error tolerance, no isotope error\n" +
                    "3: exact same mass as target");

    FormEntry fePrintGlycoDecoys = mu.feb(PROP_print_decoys, UiUtils.createUiCheck("Print Decoy Glycans", false)).create();
    FormEntry feRemoveGlycoDeltaMass = mu.feb(PROP_remove_glyco_deltamass, UiUtils.createUiCheck("Remove Glycan Delta Mass", false)).create();
    FormEntry feNGlycanMode = mu.feb(PROP_nglyco_mode, UiUtils.createUiCheck("N-Glycan Mode", true)).create();

    mu.add(pGlycoAssignContent, feGlycanFDR.label(), mu.ccR());
    mu.add(pGlycoAssignContent, feGlycanFDR.comp).split();

    mu.add(pGlycoAssignContent, feGlycanMassErr.label(), mu.ccR());
    mu.add(pGlycoAssignContent, feGlycanMassErr.comp).split();
    mu.add(pGlycoAssignContent, feGlycanIsotopesLow.label(), mu.ccR());
    mu.add(pGlycoAssignContent, feGlycanIsotopesLow.comp).split();
    mu.add(pGlycoAssignContent, feGlycanIsotopesHigh.label(), mu.ccR());
    mu.add(pGlycoAssignContent, feGlycanIsotopesHigh.comp).split();
    mu.add(pGlycoAssignContent, feNGlycanMode.comp).split().spanX().pushX().wrap();

    mu.add(pGlycoAssignContent, feMaxAdducts.label(), mu.ccR());
    mu.add(pGlycoAssignContent, feMaxAdducts.comp).split();
    mu.add(pGlycoAssignContent, feAdductNames.label(), mu.ccR());
    mu.add(pGlycoAssignContent, feAdductNames.comp).growX(200).split();
    mu.add(pGlycoAssignContent, feGlycanDBFile.label(), mu.ccR());
    mu.add(pGlycoAssignContent, btnBrosweGlycanDBFile, mu.ccR());
    mu.add(pGlycoAssignContent, feGlycanDBFile.comp).split().growX().spanX().pushX().wrap();

    mu.add(p, uiCheckGlycoAssign).spanX().wrap();
    mu.add(p, pGlycoAssignContent).growX().wrap();

    // advanced params panel
    mu.add(pGlycoAdvParams, feNeuAcProbs.label(), mu.ccR());
    mu.add(pGlycoAdvParams, feNeuAcProbs.comp).split();
    mu.add(pGlycoAdvParams, feNeuGcProbs.label(), mu.ccR());
    mu.add(pGlycoAdvParams, feNeuGcProbs.comp).split();
    mu.add(pGlycoAdvParams, feFucOxProbs.label(), mu.ccR());
    mu.add(pGlycoAdvParams, feFucOxProbs.comp).split();
    mu.add(pGlycoAdvParams, fePhosphoProbs.label(), mu.ccR());
    mu.add(pGlycoAdvParams, fePhosphoProbs.comp).split();
    mu.add(pGlycoAdvParams, feSulfoProbs.label(), mu.ccR());
    mu.add(pGlycoAdvParams, feSulfoProbs.comp).split().spanX().pushX().wrap();

    mu.add(pGlycoAdvParams, feRegYProbs.label(), mu.ccR());
    mu.add(pGlycoAdvParams, feRegYProbs.comp).split();
    mu.add(pGlycoAdvParams, feFucYProbs.label(), mu.ccR());
    mu.add(pGlycoAdvParams, feFucYProbs.comp).split();
    mu.add(pGlycoAdvParams, feDecoyType.label(), mu.ccR());
    mu.add(pGlycoAdvParams, feDecoyType.comp).split();
    mu.add(pGlycoAdvParams, fePrintGlycoDecoys.comp).split();
    mu.add(pGlycoAdvParams, feRemoveGlycoDeltaMass.comp).split().growX().spanX().pushX().wrap();

    mu.add(p, uiCheckGlycoAdvParams).split().spanX().wrap();
    mu.add(p, pGlycoAdvParams).growX().wrap();

    return p;
  }

  private JPanel createPanelContent() {
    JPanel p =  mu.newPanel("PTM Profiling", mu.lcFillXNoInsetsTopBottom());
//    mu.borderEmpty(p);

    FormEntry feHistoSmoothBins = new FormEntry(PROP_histo_smoothbins, "Smoothing factor",
        new UiSpinnerInt(2, 0, 5, 1, 5),
        "<html>Increase to make peakpicking less sensitive by distributing weight to adjacent histogram bins.\n" +
                "Histogram smoothing: 0 = No smoothing, 1 = smooth using +/-1 bin, etc.");
    FormEntry feLocBackground = new FormEntry(PROP_localization_background, "Localization background",
        new UiSpinnerInt(4, 1, 4, 1, 5),
            "<html>Defines background residue counts for calculating localization enrichment scores.\n" +
            "Residue background probabilities: 1 = bin-wise peptides, 2 = bin-wise PSMs, 3 = all pepides, 4 = all PSMs");

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
    UiSpinnerDouble uiSpinnerAnnotTol = UiSpinnerDouble.builder(0.01, 0.001, 0.999, 0.01)
        .setFormat(new DecimalFormat("0.###")).setCols(5).create();
    FormEntry feAnnotTol = mu.feb(PROP_annotation_tol, uiSpinnerAnnotTol)
        .label("Annotation tolerance (Da)").tooltip("+/- distance from peak to annotated mass").create();


    FormEntry feMinPsms = new FormEntry(PROP_peakpicking_minPsm, "Peak minimum PSMs",
            new UiSpinnerInt(10, 0, 1000, 1, 5),
            "<html>Filters out mass shift peaks below the minimum threshold");
    UiSpinnerDouble uiSpinnerSpectraTol = UiSpinnerDouble.builder(20.0, 1.0, 1000.0, 1)
            .setFormat(new DecimalFormat("0.#")).setCols(5).create();
    FormEntry feSpectraTol = new FormEntry(PROP_spectra_ppmtol, "Fragment mass tolerance (PPM)",
            uiSpinnerSpectraTol);

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

    //mu.add(p1, feLocBackground.label(), mu.ccR());
    //mu.add(p1, feLocBackground.comp);
    mu.add(p1, feAnnotTol.label(), mu.ccR());
    mu.add(p1, feAnnotTol.comp);
    mu.add(p1, feMinPsms.label(), mu.ccR());
    mu.add(p1, feMinPsms.comp).wrap();
    mu.add(p1, feSpectraTol.label(), mu.ccR());
    mu.add(p1, feSpectraTol.comp).wrap();


    mu.add(p1, new JLabel("Normalize data to: ")).spanX().split();
    mu.add(p1, btnNormPsm);
    mu.add(p1, btnNormScan).wrap();

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
            + "&nbsp;&nbsp;&nbsp;&nbsp;Phospho:79.9663, Failed_Carbamidomethylation:-57.021464");

    JPanel p2 = mu.newPanel(null, true);
    mu.border(p2, 1);

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


    FormEntry feIonA = mu.feb(PROP_iontype_a, UiUtils.createUiCheck("a", false)).create();
    FormEntry feIonX = mu.feb(PROP_iontype_x, UiUtils.createUiCheck("x", false)).create();
    FormEntry feIonB = mu.feb(PROP_iontype_b, UiUtils.createUiCheck("b", true)).create();
    FormEntry feIonY = mu.feb(PROP_iontype_y, UiUtils.createUiCheck("y", true)).create();
    FormEntry feIonC = mu.feb(PROP_iontype_c, UiUtils.createUiCheck("c", false)).create();
    FormEntry feIonZ = mu.feb(PROP_iontype_z, UiUtils.createUiCheck("z", false)).create();
    FormEntry feMaxFragCharge = mu
        .feb(PROP_spectra_maxfragcharge,
            UiUtils.spinnerInt(2, 1, 100, 1).setCols(4).create())
        .label("Max fragment charge")
        .tooltip("max fragment charge for localization")
        .create();

    uiTextLocalizationAAs = UiUtils.uiTextBuilder().create();
    FormEntry feRestrictLoc = new FormEntry(PROP_restrict_loc, "Restrict localization to", uiTextLocalizationAAs,
            "<html>Restricts localization to specified residues.\n" +
                    "Includes glyco mode localization of remainder masses. Example: STYNP");

    JPanel p3 = mu.newPanel("Ion types for localization:", true);
    //mu.border(p3, 1);

    //mu.add(p3, new JLabel("Ion Types for Localization:")).spanX().wrap();
    mu.add(p3, feIonA.comp).spanX().split();
    mu.add(p3, feIonX.comp);
    mu.add(p3, feIonB.comp);
    mu.add(p3, feIonY.comp);
    mu.add(p3, feIonC.comp);
    mu.add(p3, feIonZ.comp);
    mu.add(p3, feMaxFragCharge.label());
    mu.add(p3, feMaxFragCharge.comp).wrap();
    mu.add(p3, feLocBackground.label()).spanX().split();
    mu.add(p3, feLocBackground.comp).wrap();
    mu.add(p3, feRestrictLoc.label(), mu.ccR());
    mu.add(p3, feRestrictLoc.comp).spanX().growX().pushX().wrap();

    mu.add(p, p3).spanX().growX().wrap();
    return p;
  }

  @Override
  protected void init() {
    this.setLayout(new BorderLayout());
    JPanel mainPanel = mu.newPanel("PTM-Shepherd", mu.lcFillXNoInsetsTopBottom());
    pTop = createPanelTop();

    // 3 Sub-panels within main PTM-S panel: PTM-Profiling, Diagnostic Ion Discovery, and Glycan Assignment/FDR
    pContent = createPanelContent();
    pDiagnosticDiscovery = createPanelDiagnostic();
    pGlycanAssignment = createpanelGlycanAssignment();

    mu.add(mainPanel, pTop).spanX().growX().wrap();
    mu.add(mainPanel, pContent).spanX().growX().wrap();
    mu.add(mainPanel, pDiagnosticDiscovery).spanX().growX().wrap();
    mu.add(mainPanel, pGlycanAssignment).spanX().growX().wrap();

    this.add(mainPanel);
  }

  @Override
  protected void initMore() {
    super.initMore();
    loadDefaults(1, SearchTypeProp.open); // pre-populate, but only after renaming has happened in super.initMore()

    SwingUtils.setEnablementUpdater(this, pDiagnosticDiscovery, checkRun);
    SwingUtils.setEnablementUpdater(this, pGlycanAssignment, checkRun);

    // enable/disable Glycan Assignment areas when the overall diagnostic box is changed because glyco depends on diagnostic (for now)
    SwingUtils.setEnablementUpdater(this, pDiagnosticConent, uiCheckDiagnostic);
    SwingUtils.setEnablementUpdater(this, pGlycoAssignContent, uiCheckDiagnostic);
    SwingUtils.setEnablementUpdater(this, uiCheckGlycoAssign, uiCheckDiagnostic);
    SwingUtils.setEnablementUpdater(this, uiCheckGlycoAdvParams, uiCheckDiagnostic);

    // enable/disable the Glycan Assignment sub-area specifically when the glycan assignment box is changed
    SwingUtils.setEnablementUpdater(this, pGlycoAssignContent, uiCheckGlycoAssign);
    SwingUtils.setEnablementUpdater(this, uiCheckGlycoAdvParams, uiCheckGlycoAssign);

    // enable/disable advanced params for glycan assignment when the corresponding checkbox is changed
    SwingUtils.setEnablementUpdater(this, pGlycoAdvParams, uiCheckGlycoAdvParams);
  }

  public boolean isRunShepherd() {
    return checkRun.isEnabled() && checkRun.isSelected();
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
