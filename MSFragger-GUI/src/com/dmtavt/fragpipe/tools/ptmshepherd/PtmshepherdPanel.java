package com.dmtavt.fragpipe.tools.ptmshepherd;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.api.Bus;
import com.dmtavt.fragpipe.api.SearchTypeProp;
import com.dmtavt.fragpipe.messages.MessageLoadShepherdDefaults;
import com.dmtavt.fragpipe.messages.MessageSearchType;
import com.dmtavt.fragpipe.tools.enums.MassTolUnits;
import com.github.chhh.utils.MapUtils;
import com.github.chhh.utils.PropertiesUtils;
import com.github.chhh.utils.StringUtils;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.swing.FileChooserUtils;
import com.github.chhh.utils.swing.FileChooserUtils.FcMode;
import com.github.chhh.utils.swing.FormEntry;
import com.github.chhh.utils.swing.GhostText;
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
import net.java.balloontip.BalloonTip;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.greenrobot.eventbus.Subscribe;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class PtmshepherdPanel extends JPanelBase {
  private static final Logger log = LoggerFactory.getLogger(PtmshepherdPanel.class);
  private static final String PREFIX = "ptmshepherd.";

  public static final String PROP_threads = "threads";
  public static final String PROP_histo_bindivs = "histo_bindivs";
  public static final String PROP_histo_smoothbins = "histo_smoothbins";
  public static final String PROP_peakpicking_promRatio = "peakpicking_promRatio";
  public static final String PROP_peakpicking_width = "peakpicking_width";
  public static final String PROP_peakpicking_mass_units = "peakpicking_mass_units";
  public static final String PROP_peakpicking_background = "peakpicking_background";
  public static final String PROP_peakpicking_topN = "peakpicking_topN";

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

  private static final String PROP_custom_modlist_loc = "ptmshepherd.path.modlist";

  private final List<BalloonTip> balloonTips = new ArrayList<>();
  private JCheckBox checkRun;
  private JPanel pContent;
  private JPanel pTop;
  private UiText uiTextVarMods;

  private static Map<String, Function<String, String>> CONV_TO_GUI = new HashMap<>();
  private static Map<String, Function<String, String>> CONV_TO_FILE = new HashMap<>();
  private ButtonGroup btnGroupAnnotations;
  private JRadioButton btnAnnUnimod;
  private JRadioButton btnAnnCommon;
  private JRadioButton btnAnnCustom;
  private UiText uiTextAnnotationFile;
  private UiCheck uiCheckGlyco;

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
      } else if (kv.getKey().startsWith("annotation-custom") && kv.getValue().equalsIgnoreCase("true")) {
        map2.put(PROP_annotation_file, map1.get(PROP_annotation_file));
      } else {  // copy everything else
        map2.put(kv.getKey(), kv.getValue());
      }
    }

    // remap remaining values
    Map<String, String> map3 = MapUtils.remapValues(map2, (k, v) -> CONV_TO_FILE.getOrDefault(k, Function.identity()).apply(v));
//    map = MapUtils.remap(map,
//        (k,v) -> StringUtils.stripLeading(k, PREFIX),
//        (k,v) -> CONV_TO_FILE.getOrDefault(k, Function.identity()).apply(v));
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
          SearchTypeProp type = defaults.get((String) uiComboDefaults.getSelectedItem());
          Bus.post(new MessageLoadShepherdDefaults(true, type));
        });

    mu.add(p, labelDefaults).split().spanX();
    mu.add(p, uiComboDefaults);
    mu.add(p, btnLoadDefaults).wrap();

    return p;
  }

  private JPanel createpanelGlyco() {
    JPanel p = mu.newPanel("Glyco options", mu.lcFillXNoInsetsTopBottom());

    uiCheckGlyco = UiUtils.createUiCheck("Enable Glyco Mode", false);
    uiCheckGlyco.setName("glyco_mode");

    FormEntry feYIonMasses = mu.feb(PROP_cap_y_ions, UiUtils.uiTextBuilder().create())
        .label("Y Ion Masses")
        .tooltip("Added to peptide precursor and searched for in MS2 spectrum. "
            + "Space, comma, or slash separated values accepted.").create();
    FormEntry feDiagnosticFragmentMasses = mu.feb(PROP_diag_ions, UiUtils.uiTextBuilder().create())
        .label("Diagnostic Fragment Masses")
        .tooltip("Checked for directly in the MS2 spectrum. Assumed to have a +1 charge state. "
            + "Space, comma, or slash separated values accepted.").create();
    FormEntry feRemainderMasses = mu.feb(PROP_remainder_masses, UiUtils.uiTextBuilder().create())
        .label("Remainder Masses")
        .tooltip("Partial glycan masses localized to the peptide sequence. "
            + "Space, comma, or slash separated values accepted.").create();

    mu.add(p, uiCheckGlyco).spanX().wrap();
    mu.add(p, feYIonMasses.label(), mu.ccR());
    mu.add(p, feYIonMasses.comp).spanX().growX().pushX().wrap();
    mu.add(p, feDiagnosticFragmentMasses.label(), mu.ccR());
    mu.add(p, feDiagnosticFragmentMasses.comp).spanX().growX().pushX().wrap();
    mu.add(p, feRemainderMasses.label(), mu.ccR());
    mu.add(p, feRemainderMasses.comp).spanX().growX().pushX().wrap();

    return p;
  }

  private JPanel createPanelContent() {
    JPanel p = new JPanel(new MigLayout(new LC().fillX()));
    mu.borderEmpty(p);

    FormEntry feHistoSmoothBins = new FormEntry(PROP_histo_smoothbins, "Smoothing factor",
        new UiSpinnerInt(2, 0, 5, 1, 5),
        "<html>Histogram smoothing. 0 = No smoothing, 1 = smooth using +/-1 bin, etc.");
    FormEntry feLocBackground = new FormEntry(PROP_localization_background, "Localization background",
        new UiSpinnerInt(4, 1, 4, 1, 5));

    UiSpinnerDouble uiSpinnerPromRatio = UiSpinnerDouble.builder(0.3,0.0,1.0, 0.1)
        .setFormat(new DecimalFormat("0.#")).setCols(5).create();
    FormEntry fePromRatio = new FormEntry(PROP_peakpicking_promRatio, "Prominence ratio", uiSpinnerPromRatio,
        "Ratio of peak prominence to total peak height.");

    UiSpinnerDouble uiSpinnerWidth = UiSpinnerDouble.builder(0.002, 0.0, 500, 0.001)
        .setFormat(new DecimalFormat("0.####")).setCols(5).create();
    FormEntry feWidth = mu.feb(PROP_peakpicking_width, uiSpinnerWidth)
        .label("Peak picking width").tooltip("+/- signal width during peakpicking").create();
    FormEntry fePeakPickingUnits = mu.feb(PROP_peakpicking_mass_units, UiUtils.createUiCombo(MassTolUnits.values()))
        .tooltip("+/- signal width during peakpicking")
        .create();

    FormEntry feExtendedOut = new FormEntry(PROP_output_extended, "not-shown",
        new UiCheck("Extended output", null, false),
        "<html>Write additional files with more detailed information.");

    UiSpinnerDouble uiSpinnerPrecTol = UiSpinnerDouble.builder(0.01, 0.001, 1e6, 0.01)
        .setFormat(new DecimalFormat("0.###")).setCols(5).create();
    uiSpinnerPrecTol.setColumns(5);
    FormEntry fePrecTol = new FormEntry(PROP_precursor_tol, "Precursor tolerance", uiSpinnerPrecTol);
    FormEntry fePrecUnits = mu.fe(PROP_precursor_mass_units, UiUtils.createUiCombo(MassTolUnits.values()));
    UiSpinnerDouble uiSpinnerAnnotTol = UiSpinnerDouble.builder(0.01, 0.001, 0.999, 0.01)
        .setFormat(new DecimalFormat("0.###")).setCols(5).create();
    FormEntry feAnnotTol = mu.feb(PROP_annotation_tol, uiSpinnerAnnotTol)
        .label("Annotation tolerance (Da)").tooltip("+/- distance from peak to annotated mass").create();

    btnGroupAnnotations = new ButtonGroup();
    btnAnnUnimod = new JRadioButton("Unimod", true);
    btnAnnUnimod.setName("annotation-unimod");
    btnAnnCommon = new JRadioButton("Common mass shifts", false);
    btnAnnCommon.setName("annotation-common");
    btnAnnCustom = new JRadioButton("Custom annotation file", false);
    btnAnnCustom.setName("annotation-custom");
    btnGroupAnnotations.add(btnAnnUnimod);
    btnGroupAnnotations.add(btnAnnCommon);
    btnGroupAnnotations.add(btnAnnCustom);


    String tooltipAnnotationFile = "Custom mass shift annotation file. Will not map to UniMod if provided.";
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

    mu.add(p, feHistoSmoothBins.label(), mu.ccR());
    mu.add(p, feHistoSmoothBins.comp);
    mu.add(p, fePrecTol.label(), mu.ccR());
    mu.add(p, fePrecTol.comp).split(2);
    mu.add(p, fePrecUnits.comp).wrap();

    mu.add(p, fePromRatio.label(), mu.ccR());
    mu.add(p, fePromRatio.comp);
    mu.add(p, feWidth.label(), mu.ccR());
    mu.add(p, feWidth.comp).split(2);
    mu.add(p, fePeakPickingUnits.comp);
    mu.add(p, feExtendedOut.comp).pushX().wrap();

    mu.add(p, feLocBackground.label(), mu.ccR());
    mu.add(p, feLocBackground.comp);
    mu.add(p, feAnnotTol.label(), mu.ccR());
    mu.add(p, feAnnotTol.comp).wrap();

    final String ghost = "Phospho:79.9663, Something-else:-20.123";
    uiTextVarMods = new UiTextBuilder().text("Failed_Carbamidomethylation:-57.021464")
        .ghost(ghost).create();
    GhostText.register(uiTextVarMods, ghost);
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
        "<html>Variable modification masses.<br/>\n"
            + "Comma separated entries of form \"&lt;name&gt;:&lt;mass&gt;\"<br/>\n"
            + "Example:<br/>\n"
            + "&nbsp;&nbsp;&nbsp;&nbsp;Phospho:79.9663,Something-else:-20.123");
    mu.add(p, feVarMods.label(), mu.ccR());
    mu.add(p, feVarMods.comp).spanX().growX().wrap();

    mu.add(p, new JLabel("Annotation source: ")).spanX().split();
    mu.add(p, btnAnnUnimod);
    mu.add(p, btnAnnCommon).wrap();

    mu.add(p, btnAnnCustom, mu.ccR());
    mu.add(p, feAnnotationFile.comp).split().spanX().growX();
    mu.add(p, btnBrosweAnnotationFile).wrap();

    FormEntry feIonA = mu.feb(PROP_iontype_a, UiUtils.createUiCheck("a", false)).create();
    FormEntry feIonX = mu.feb(PROP_iontype_x, UiUtils.createUiCheck("x", false)).create();
    FormEntry feIonB = mu.feb(PROP_iontype_b, UiUtils.createUiCheck("b", true)).create();
    FormEntry feIonY = mu.feb(PROP_iontype_y, UiUtils.createUiCheck("y", true)).create();
    FormEntry feIonC = mu.feb(PROP_iontype_c, UiUtils.createUiCheck("c", false)).create();
    FormEntry feIonZ = mu.feb(PROP_iontype_z, UiUtils.createUiCheck("z", false)).create();

    mu.add(p, new JLabel("Ion Types for Localization:")).spanX().wrap();
    mu.add(p, feIonA.comp).spanX().split();
    mu.add(p, feIonX.comp);
    mu.add(p, feIonB.comp);
    mu.add(p, feIonY.comp);
    mu.add(p, feIonC.comp);
    mu.add(p, feIonZ.comp).wrap();

    JPanel pGlyco = createpanelGlyco();
    mu.add(p, pGlyco).spanX().growX().wrap();

    return p;
  }

  @Override
  protected void init() {
    this.setLayout(new BorderLayout());
    this.setBorder(new TitledBorder("PTM-Shepherd"));

    pTop = createPanelTop();
    pContent = createPanelContent();

    this.add(pTop, BorderLayout.NORTH);
    this.add(pContent, BorderLayout.CENTER);
  }

  @Override
  protected void initMore() {
    super.initMore();

    loadDefaults(1, SearchTypeProp.open); // pre-populate, but only after renaming has happened in super.initMore()
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
