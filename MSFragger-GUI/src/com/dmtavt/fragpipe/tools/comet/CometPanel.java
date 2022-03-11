package com.dmtavt.fragpipe.tools.comet;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.api.Bus;
import com.dmtavt.fragpipe.api.FragpipeCacheUtils;
import com.dmtavt.fragpipe.api.ModsTable;
import com.dmtavt.fragpipe.api.ModsTableModel;
import com.dmtavt.fragpipe.api.SearchTypeProp;
import com.dmtavt.fragpipe.messages.MessageCometParamsUpdate;
import com.dmtavt.fragpipe.messages.MessagePrecursorSelectionMode;
import com.dmtavt.fragpipe.messages.MessageSearchType;
import com.dmtavt.fragpipe.messages.NoteConfigComet;
import com.dmtavt.fragpipe.messages.NoteConfigSearchEngine;
import com.dmtavt.fragpipe.params.Props;
import com.dmtavt.fragpipe.params.ThisAppProps;
import com.dmtavt.fragpipe.tabs.TabComet;
import com.dmtavt.fragpipe.tabs.TabWorkflow;
import com.dmtavt.fragpipe.tools.enums.CleavageType;
import com.dmtavt.fragpipe.tools.enums.FraggerOutputType;
import com.dmtavt.fragpipe.tools.enums.FraggerPrecursorMassMode;
import com.dmtavt.fragpipe.tools.enums.RemovePrecursorPeak;
import com.dmtavt.fragpipe.tools.fragger.Mod;
import com.dmtavt.fragpipe.tools.fragger.MsfraggerEnzyme;
import com.github.chhh.utils.MapUtils;
import com.github.chhh.utils.PathUtils;
import com.github.chhh.utils.StringUtils;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.swing.FileChooserUtils;
import com.github.chhh.utils.swing.FormEntry;
import com.github.chhh.utils.swing.JPanelBase;
import com.github.chhh.utils.swing.MigUtils;
import com.github.chhh.utils.swing.UiCheck;
import com.github.chhh.utils.swing.UiCombo;
import com.github.chhh.utils.swing.UiSpinnerDouble;
import com.github.chhh.utils.swing.UiSpinnerInt;
import com.github.chhh.utils.swing.UiText;
import com.github.chhh.utils.swing.UiUtils;
import com.github.chhh.utils.swing.renderers.TableCellDoubleRenderer;
import com.github.chhh.utils.swing.renderers.TableCellIntRenderer;
import com.github.chhh.utils.swing.renderers.TableCellIntSpinnerEditor;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.ThreadMode;
import org.jooq.lambda.Seq;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.*;
import javax.swing.border.TitledBorder;
import javax.swing.filechooser.FileNameExtensionFilter;
import javax.swing.table.TableCellEditor;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.awt.event.HierarchyEvent;
import java.awt.event.HierarchyListener;
import java.awt.event.ItemEvent;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.DecimalFormat;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static com.dmtavt.fragpipe.tabs.TabRun.LAST_WORK_DIR;

//import static com.dmtavt.fragpipe.tools.fragger.MsfraggerParams.*;
//import static com.dmtavt.fragpipe.tools.comet.CometParams.*;

public class CometPanel extends JPanelBase {

    public static final boolean showSimplePanel = true;
    private JPanel pSimpleParams;

    private static final Logger log = LoggerFactory.getLogger(CometPanel.class);
    private final static MigUtils mu = MigUtils.get();
    private AtomicBoolean hasBeenShown = new AtomicBoolean(false);
    public static final String TAB_PREFIX = TabComet.TAB_PREFIX;
    public static final String PROP_FILECHOOSER_LAST_PATH = "comet.filechooser.path";
    public static final String CACHE_FORM = "comet-form" + ThisAppProps.TEMP_FILE_EXT;
    public static final String CACHE_PROPS = "comet-props" + ThisAppProps.TEMP_FILE_EXT;
    public static final Map<String, String> MAP_COMET_ISO_ERROR_CODES = new HashMap<>();

    public static final LinkedHashMap<String, SearchTypeProp> SEARCH_TYPE_NAME_MAPPING;
    private static final String[] TABLE_VAR_MODS_COL_NAMES = {"Enabled", "Site (editable)",
            "Mass Delta (editable)", "Max occurrences (editable)"};
    private static final String[] TABLE_FIX_MODS_COL_NAMES = {"Enabled", "Site",
            "Mass Delta (editable)"};
    private static final String PROP_misc_adjust_precurosr_mass = "misc.adjust-precursor-mass";
    private static final String PROP_misc_slice_db = "misc.slice-db";
    private static final String PROP_misc_ram = "misc.ram";
    private static final String PROP_MISC_comet_remove_precursor_range_lo = "misc.comet.remove-precursor-range-lo";
    private static final String PROP_MISC_comet_remove_precursor_range_hi = "misc.comet.remove-precursor-range-hi";
    private static final String PROP_misc_comet_digest_mass_lo = "misc.comet.digest-mass-lo";
    private static final String PROP_misc_comet_digest_mass_hi = "misc.comet.digest-mass-hi";
    private static final String PROP_MISC_comet_clear_mz_lo = "misc.comet.clear-mz-lo";
    private static final String PROP_MISC_comet_clear_mz_hi = "misc.comet.clear-mz-hi";
    private static final String PROP_MISC_comet_precursor_charge_lo = "misc.comet.precursor-charge-lo";
    private static final String PROP_MISC_comet_precursor_charge_hi = "misc.comet.precursor-charge-hi";
    public static final String PROP_misc_comet_enzyme_dropdown_1 = "misc.comet.enzyme-dropdown-1";
    public static final String PROP_misc_comet_enzyme_dropdown_2 = "misc.comet.enzyme-dropdown-2";
    private static final Set<String> PROPS_MISC_NAMES;
    private static final Map<String, Function<String, String>> CONVERT_TO_FILE;
    private static final Map<String, Function<String, String>> CONVERT_TO_GUI;
    private static final String CALIBRATE_VALUE_OFF = "None";
    private static final String[] CALIBRATE_LABELS = {CALIBRATE_VALUE_OFF, "Mass calibration", "Mass calibration, parameter optimization"};
    private static final String[] MASS_DIFF_TO_VAR_MOD = {"No", "Yes, keep delta mass", "Yes, remove delta mass"};
    private static final String[] DEISOTOPE = {"No", "Yes", "Yes, use charge 1 and 2 for undeisotoped peaks"};
    private static final int[] MASS_DIFF_TO_VAR_MOD_MAP = {0, 2, 1};
    private static final String LOAD_CUSTOM_CONFIG_OPTION = "Custom Comet parameter file from disk";

    private static final java.util.List<MsfraggerEnzyme> ENZYMES = new CometEnzymeProvider().get();
    //public static FileNameExtensionFilter fnExtFilter = new FileNameExtensionFilter("LCMS files (mzML/mzXML/mgf/raw/d)", "mzml", "mzxml");
    private static String[] PROPS_MISC = {
            PROP_misc_adjust_precurosr_mass,
            PROP_misc_slice_db,
            PROP_misc_ram,
            PROP_misc_comet_digest_mass_lo,
            PROP_misc_comet_digest_mass_hi,
            PROP_MISC_comet_remove_precursor_range_lo,
            PROP_MISC_comet_remove_precursor_range_hi,
            PROP_MISC_comet_clear_mz_lo,
            PROP_MISC_comet_clear_mz_hi,
            PROP_MISC_comet_precursor_charge_lo,
            PROP_MISC_comet_precursor_charge_hi,
    };

    UiCombo uiComboMassDiffToVariableMod;
    UiCombo uiComboIsoErrors;
    private UiSpinnerDouble uiSpinnerFragBinOffset;
    private static final String PROP_MISC_digest_length_lo = "misc.digest_length_lo";
    private static final String PROP_MISC_digest_length_hi = "misc.digest_length_hi";
    private static final String PROP_MISC_search_enzyme_cut_1 = "no-use.search_enzyme_cut_1";
    private static final String PROP_MISC_search_enzyme_nocut_1 = "no-use.search_enzyme_nocut_1";
    private static final String PROP_MISC_search_enzyme_sense_1 = "no-use.search_enzyme_sense_1";
    private static final String PROP_MISC_search_enzyme_cut_2 = "no-use.search_enzyme_cut_2";
    private static final String PROP_MISC_search_enzyme_nocut_2 = "no-use.search_enzyme_nocut_2";
    private static final String PROP_MISC_search_enzyme_sense_2 = "no-use.search_enzyme_sense_2";
    private static final String PROP_MISC_ion_series = "misc.ion_series";

    private UiText uiTextPathParams;
    private boolean isRunBackupVar = true;


    private static String itos(int i) {
        return Integer.toString(i);
    }

    private static String stobtoitos(String s) {
        return itos(Boolean.parseBoolean(s) ? 1 : 0);
    }

    static {
        PROPS_MISC_NAMES = new HashSet<>(Arrays.asList(PROPS_MISC));
        CONVERT_TO_FILE = new HashMap<>();
        CONVERT_TO_GUI = new HashMap<>();

//        CONVERT_TO_FILE.put(CometParams.PROP_write_calibrated_mgf, s -> itos(Boolean.parseBoolean(s) ? 1 : 0));
//        CONVERT_TO_FILE.put(CometParams.PROP_mass_diff_to_variable_mod, s -> itos(
//                MASS_DIFF_TO_VAR_MOD_MAP[ArrayUtils.indexOf(MASS_DIFF_TO_VAR_MOD, s)]));
//        CONVERT_TO_FILE.put(CometParams.PROP_deisotope, s -> {
//            for (int i = 0; i < DEISOTOPE.length; ++i) {
//                if (s.equalsIgnoreCase(DEISOTOPE[i])) {
//                    return itos(i);
//                }
//            }
//            return "1";
//        });


        CONVERT_TO_FILE.put(CometParams.PROP_peptide_mass_tolerance, s -> itos(CometTolUnits.valueOf(s).valueInParamsFile()));
//        CONVERT_TO_FILE.put(CometParams.PROP_fragment_mass_units, s -> itos(CometTolUnits.valueOf(s).valueInParamsFile()));


//        CONVERT_TO_FILE.put(CometParams.PROP_precursor_true_units, s -> itos(
//                CometTolUnits.valueOf(s).valueInParamsFile()));
//        CONVERT_TO_FILE.put(CometParams.PROP_calibrate_mass, s -> itos(Arrays.asList(CALIBRATE_LABELS).indexOf(s)));
//        CONVERT_TO_FILE.put(CometParams.PROP_use_all_mods_in_first_search, s -> itos(Boolean.parseBoolean(s) ? 1 : 0));
        CONVERT_TO_FILE.put(CometParams.PROP_num_enzyme_termini, s -> itos(
                CleavageType.valueOf(s).valueInParamsFile()));
        CONVERT_TO_FILE.put(CometParams.PROP_remove_precursor_peak, s -> itos(
                RemovePrecursorPeak.get(s)));
//        CONVERT_TO_FILE.put(CometParams.PROP_intensity_transform, s -> itos(
//                IntensityTransform.get(s)));
//        CONVERT_TO_FILE.put(CometParams.PROP_localize_delta_mass, s -> itos(Boolean.parseBoolean(s) ? 1 : 0));
//        CONVERT_TO_FILE.put(CometParams.PROP_clip_nTerm_M, s -> itos(Boolean.parseBoolean(s) ? 1 : 0));
        CONVERT_TO_FILE.put(CometParams.PROP_override_charge, s -> itos(Boolean.parseBoolean(s) ? 1 : 0));
//        CONVERT_TO_FILE.put(CometParams.PROP_output_format, s -> FraggerOutputType.valueOf(s).valueInParamsFile());
//        CONVERT_TO_FILE.put(CometParams.PROP_report_alternative_proteins, s -> itos(Boolean.parseBoolean(s) ? 1 : 0));
//        CONVERT_TO_FILE.put(CometParams.PROP_fragment_ion_series, ionStr -> ionStr.trim().replaceAll("[\\s,;]+", ","));
//        CONVERT_TO_FILE.put(CometParams.PROP_ion_series_definitions, defStr -> defStr.trim().replaceAll("\\s*[,;]+\\s*", ", "));
//
//        CONVERT_TO_FILE.put(CometParams.PROP_labile_search_mode, s -> GLYCO_OPTIONS.get(GLYCO_OPTIONS_UI.indexOf(s)));
        CONVERT_TO_FILE.put(CometParams.PROP_mass_offsets, s -> s.replaceAll("[\\s]+", "/"));
//        CONVERT_TO_FILE.put(CometParams.PROP_Y_type_masses, s -> s.replaceAll("[\\s]+", "/"));
//        CONVERT_TO_FILE.put(CometParams.PROP_diagnostic_fragments, s -> s.replaceAll("[\\s]+", "/"));
//        CONVERT_TO_FILE.put(CometParams.PROP_remainder_masses, s -> s.replaceAll("[\\s]+", "/"));
//        CONVERT_TO_FILE.put(CometParams.PROP_deneutralloss, s -> s.toLowerCase().contentEquals("yes") ? "1" : "0");
//
//        CONVERT_TO_GUI.put(CometParams.PROP_write_calibrated_mgf, s -> Boolean.toString(Integer.parseInt(s) > 0));
//        CONVERT_TO_GUI.put(CometParams.PROP_mass_diff_to_variable_mod, s -> MASS_DIFF_TO_VAR_MOD[MASS_DIFF_TO_VAR_MOD_MAP[Integer.parseInt(s)]]);
//        CONVERT_TO_GUI.put(CometParams.PROP_deisotope, s -> DEISOTOPE[Integer.parseInt(s)]);
//        CONVERT_TO_GUI.put(CometParams.PROP_precursor_mass_units, s -> CometTolUnits.fromParamsFileToUi(s).name());
//        CONVERT_TO_GUI.put(CometParams.PROP_fragment_mass_units, s -> CometTolUnits.fromParamsFileToUi(s).name());
//        CONVERT_TO_GUI.put(CometParams.PROP_precursor_true_units, s -> CometTolUnits.fromParamsFileToUi(s).name());
//        CONVERT_TO_GUI.put(CometParams.PROP_calibrate_mass, s -> CALIBRATE_LABELS[Integer.parseInt(s)]);
//        CONVERT_TO_GUI.put(CometParams.PROP_use_all_mods_in_first_search, s -> Boolean.toString(Integer.parseInt(s) == 1));
        CONVERT_TO_GUI.put(CometParams.PROP_num_enzyme_termini, s -> CleavageType.fromValueInParamsFile(s).name());
        CONVERT_TO_GUI.put(CometParams.PROP_remove_precursor_peak, s -> RemovePrecursorPeak.get(Integer.parseInt(s)));
//        CONVERT_TO_GUI.put(CometParams.PROP_intensity_transform, s -> IntensityTransform.get(Integer.parseInt(s)));
//        CONVERT_TO_GUI.put(CometParams.PROP_localize_delta_mass, s -> Boolean.toString(Integer.parseInt(s) > 0));
//        CONVERT_TO_GUI.put(CometParams.PROP_clip_nTerm_M, s -> Boolean.toString(Integer.parseInt(s) > 0));
        CONVERT_TO_GUI.put(CometParams.PROP_override_charge, s -> Boolean.toString(Integer.parseInt(s) > 0));
//        CONVERT_TO_GUI.put(CometParams.PROP_output_format, s -> FraggerOutputType.fromValueInParamsFile(s).name());
//        CONVERT_TO_GUI.put(CometParams.PROP_report_alternative_proteins, s -> Boolean.toString(Integer.parseInt(s) > 0));

//        CONVERT_TO_GUI.put(CometParams.PROP_labile_search_mode, s -> GLYCO_OPTIONS_UI.get(GLYCO_OPTIONS.indexOf(s)));
        CONVERT_TO_GUI.put(CometParams.PROP_mass_offsets, text -> String.join(" ", text.split("/")));
//        CONVERT_TO_GUI.put(CometParams.PROP_Y_type_masses, text -> String.join(" ", text.split("/")));
//        CONVERT_TO_GUI.put(CometParams.PROP_diagnostic_fragments, text -> String.join(" ", text.split("/")));
//        CONVERT_TO_GUI.put(CometParams.PROP_remainder_masses, text -> String.join(" ", text.split("/")));
//        CONVERT_TO_GUI.put(CometParams.PROP_deneutralloss, s -> Integer.parseInt(s) == 1 ? "Yes" : "No");

        SEARCH_TYPE_NAME_MAPPING = new LinkedHashMap<>();
        SEARCH_TYPE_NAME_MAPPING.put("Closed Search default config", SearchTypeProp.closed);

        MAP_COMET_ISO_ERROR_CODES.put("0", "none");
        MAP_COMET_ISO_ERROR_CODES.put("1", "0/+1");
        MAP_COMET_ISO_ERROR_CODES.put("2", "0/+1/+2");
        MAP_COMET_ISO_ERROR_CODES.put("3", "0/+1/+2/+3");
        MAP_COMET_ISO_ERROR_CODES.put("4", "-8/-4/0/+4/+8");
        MAP_COMET_ISO_ERROR_CODES.put("5", "-1/0/+1/+2/+3");
    }

    private UiCheck checkRun;
    private JPanel pContent;
    private ModsTable tableVarMods;
    private ModsTable tableFixMods;
    public UiCombo uiComboOutputType;
    private Map<Component, Boolean> enablementMapping = new HashMap<>();

    private UiCombo uiComboEnzymes;
    private UiText uiTextCuts;
    private UiText uiTextNocuts;
    private UiText uiTextEnzymeName;
    private UiCombo uiComboSense;
    private UiCombo uiComboCleavage;

    private UiCombo uiComboEnzymes2;
    private UiText uiTextCuts2;
    private UiText uiTextNocuts2;
    private UiCombo uiComboSense2;
    private UiText uiTextEnzymeName2;

    private UiCombo uiComboLoadDefaultsNames;
//    private UiSpinnerDouble uiSpinnerPrecTolLo;
    private UiSpinnerDouble uiSpinnerPrecTolHi;
    private UiCombo uiComboPrecursorTolUnits;
    private final Map<String, String> cache = new HashMap<>();
    private JPanel pTop;
    private JPanel pBasic;
    private JPanel pMods;
    private JPanel pAdvanced;
    private UiCheck uiCheckWriteCalibratedMgf;


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
        return TAB_PREFIX;
    }

    public int getMassDiffToVariableMod() {
        return MASS_DIFF_TO_VAR_MOD_MAP[uiComboMassDiffToVariableMod.getSelectedIndex()];
    }

    private static void actionChangeMassMode(ItemEvent e) {
        if (e.getStateChange() == ItemEvent.SELECTED) {

            final Object item = e.getItem();
            if (!(item instanceof String)) {
                return;
            }
            try {
                FraggerPrecursorMassMode mode = FraggerPrecursorMassMode.valueOf((String) item);
                Bus.post(new MessagePrecursorSelectionMode(mode));

            } catch (IllegalArgumentException ex) {
                log.debug("Value [{}] not in FraggerPrecursorMassMode enum", item);
            }
        }
    }

    @Override
    protected void initMore() {
        postInitAddActionListeners();
        updateEnabledStatus(this, false); // will get enabled once we receive NoteConfigMsfragger

        super.initMore();

        if (!showSimplePanel) {
            // init fields with default values, called after initMore() which causes field renaming to happen
            log.debug("Calling TabComet loadDefaults(SearchTypeProp.closed, false) in initMore()");
            loadDefaults(SearchTypeProp.closed, false);
        }

        // try scrolling up
        addHierarchyListener(new HierarchyListener() {
            @Override
            public void hierarchyChanged(HierarchyEvent e) {

                final Component c = e.getComponent();
                final long f = e.getChangeFlags();
//        log.debug("tab msfragger hierarchy changed: {}", e.paramString());
//        if ((f & HierarchyEvent.DISPLAYABILITY_CHANGED) != 0) {
//          if (c.isDisplayable()) {
//            log.debug("tab fragger is now displayable={}, scrolling up", c.isDisplayable());
//            SwingUtils.scrollTo((JComponent) c, SwingUtils.LEFT, SwingUtils.TOP);
//          }
//        }
                if ((f & HierarchyEvent.SHOWING_CHANGED) != 0) {
                    if (!hasBeenShown.get() && c.isShowing()) {
                        hasBeenShown.set(true);
                        log.debug("tab fragger showing for the first time, scrolling");
                        SwingUtils.scrollTo((JComponent) c, SwingUtils.LEFT, SwingUtils.TOP);

                    }
                }
            }
        });
    }

    @Override
    protected void init() {
        this.setLayout(new MigLayout(new LC().fillX()));

        pTop = createPanelTop();
        pContent = createPanelContent();

        mu.add(this, pTop).growX().wrap();
        mu.add(this, pContent).growX().wrap();
        if (showSimplePanel) {
            pSimpleParams = createPanelSimpleParams();
            mu.add(pContent, pSimpleParams).growX().wrap();
        } else {
            pBasic = createPanelBasicOptions();
            pMods = createPanelMods();
            pAdvanced = createPanelAdvancedOptions();
            mu.add(pContent, pBasic).growX().wrap();
            mu.add(pContent, pMods).growX().wrap();
            mu.add(pContent, pAdvanced).growX().wrap();
        }
    }

    /**
     * Top panel with checkbox, buttons and RAM+Threads spinners.
     */
    private JPanel createPanelTop() {
        JPanel pTop = new JPanel(new MigLayout(new LC()));

        checkRun = new UiCheck("Run Comet", null, true);
        checkRun.setName("run-comet");
        checkRun.addActionListener(e -> {
            final boolean isSelected = checkRun.isSelected();
            updateEnabledStatus(pContent, isSelected);
        });

        JButton btnSave = new JButton("Save Config");
        btnSave.setToolTipText("<html>Save Comet compatible config file \n</br>"
                + "which can be used with Comet from command line");
        btnSave.addActionListener(this::actionBtnConfigSave);

        java.util.List<String> loadOptions = Seq.of(LOAD_CUSTOM_CONFIG_OPTION).append(Seq.seq(SEARCH_TYPE_NAME_MAPPING.keySet())).toList();
        uiComboLoadDefaultsNames = UiUtils.createUiCombo(loadOptions);
        JButton btnLoad = new JButton("Load");
        btnLoad.addActionListener(this::actionBtnConfigLoad);





        mu.add(pTop, checkRun).wrap();
//        mu.add(pTop, btnSave).split().spanX();
//        mu.add(pTop, btnLoad);
//        mu.add(pTop, new JLabel(":")).gapLeft("3px").gapRight("3px");
//        mu.add(pTop, uiComboLoadDefaultsNames).wrap();

        return pTop;
    }

    private JPanel createPanelContent() {
        pContent = new JPanel();
        pContent.setLayout(new MigLayout(new LC().fillX()));
        return pContent;
    }


    public String getParamsFilePath() {
        if (showSimplePanel && uiTextPathParams != null) {
            return uiTextPathParams.getNonGhostText();
        }
        throw new IllegalStateException("Params file needs to be created in graph config stage when not a simple UI is used");
    }

    private JPanel createPanelSimpleParams() {
        uiTextPathParams = UiUtils.uiTextBuilder().cols(30).create();
        FormEntry feWorkdir = mu.feb("workdir", uiTextPathParams)
                .label("Path to .params")
                .tooltip("<html>See examples at:\n" +
                        "https://uwpr.github.io/Comet/parameters/parameters_202102/\n" +
                        "Or start Comet executable with `-p` parameter to create a default one")
                .create();
        final String btnText = "Select .params";
        JButton btnBrowse = feWorkdir.browseButton(() -> FileChooserUtils.builder(btnText)
                .mode(FileChooserUtils.FcMode.FILES_ONLY)
                .multi(false)
                .paths(Stream.of(uiTextPathParams.getNonGhostText(), Fragpipe.propsVar().getProperty(LAST_WORK_DIR))).create(), btnText,
                selected -> uiTextPathParams.setText(selected.get(0).toString()));
        JButton btnOpenInFileManager = UiUtils.createButton("Open in File Manager", e -> {
            final String text = uiTextPathParams.getNonGhostText();
            if (StringUtils.isBlank(text)) {
                SwingUtils.showInfoDialog(CometPanel.this, "Empty path", "Does not exist");
                return;
            }
            Path existing = PathUtils.existing(text);
            if (existing == null) {
                SwingUtils.showInfoDialog(CometPanel.this, "Path:\n'" + text + "'\nDoes not exist", "Does not exist");
                return;
            }
            try {
                Desktop.getDesktop().open(existing.toFile());
            } catch (IOException ex) {
                SwingUtils.showErrorDialog(CometPanel.this, "Could not open path in system file browser.", "Error");
                return;
            }
        });


        JPanel p = mu.newPanel("Params file", true);
        mu.add(p, feWorkdir.label(), false).split().spanX();
        mu.add(p, feWorkdir.comp).growX();
        mu.add(p, btnBrowse);
        mu.add(p, btnOpenInFileManager).wrap();
        return p;
    }

    private JPanel createPanelBasicPeakMatch() {
        JPanel p = mu.newPanel("Peak Matching", true);

        // precursor mass tolerance
        java.util.List<String> units = Seq.of(CometTolUnits.values()).map(CometTolUnits::name).toList();
        uiComboPrecursorTolUnits = UiUtils.createUiCombo(units);
        FormEntry fePrecTolUnits = mu.feb(CometParams.PROP_peptide_mass_units, uiComboPrecursorTolUnits)
                .label("Precursor mass tolerance").create();
        uiSpinnerPrecTolHi = new UiSpinnerDouble(+20, -10000, 10000, 1,
                new DecimalFormat("0.###"));
        uiSpinnerPrecTolHi.setColumns(4);
        FormEntry fePrecTolHi = mu.feb(CometParams.PROP_peptide_mass_tolerance, uiSpinnerPrecTolHi).create();

        uiComboPrecursorTolUnits.addItemListener(e -> {
            Object selected = uiComboPrecursorTolUnits.getSelectedItem();
            if (selected == null || StringUtils.isNullOrWhitespace((String) selected))
                return;

            final CometTolUnits cometTolUnits = CometTolUnits.valueOf((String) selected);
            double fragBinOffset = 0;
            switch (cometTolUnits) {
                case PPM:
                    fragBinOffset = 0;
                    break;
                case amu:
                case mmu:
                    fragBinOffset = 0.4;
                    break;
            }
            uiSpinnerFragBinOffset.setValue(fragBinOffset);
        });

        // fragment mass tolerance
        UiSpinnerDouble uiSpinnerFragBinTol = new UiSpinnerDouble(0.0, 0, 10000, 1,
                new DecimalFormat("0.###"));
        uiSpinnerFragBinTol.setColumns(4);
        FormEntry feFragBinTol = mu.feb(CometParams.PROP_fragment_bin_tol, uiSpinnerFragBinTol)
                .label("Fragment bin tolerance")
                .tooltip("binning to use on fragment ions")
                .create();

        uiSpinnerFragBinOffset = new UiSpinnerDouble(0, 0, 1, 0.1,
                new DecimalFormat("0.###"));
        uiSpinnerFragBinOffset.setColumns(4);
        FormEntry feFragBinOffset = mu.feb(CometParams.PROP_fragment_bin_offset, uiSpinnerFragBinOffset)
                .label("offset")
                .tooltip("<html>This parameter controls how each fragment bin of size fragment_bin_tol is defined in terms of where each bin starts.\n" +
                        "For example, assume a fragment_bin_tol of 1.0. Most intuitively, the fragment bins would be 0.0 to 1.0, 1.0 to 2.0, 2.0 to 3.0, etc.\n" +
                        "This set of bins corresponds to a fragment_bin_offset of 0.0.\n" +
                        "However, consider if we set fragment_bin_offset to 0.5;\n" +
                        "this would cause the bins to be 0.5 to 1.5, 1.5 to 2.5, 2.5 to 3.5, etc.\n" +
                        "So this fragment_bin_offset gives one a mechanism to define where each bin starts and is centered.\n" +
                        "For ion trap data with a fragment_bin_tol of 1.0005, it is recommended to set fragment_bin_offset to 0.4.\n" +
                        "For high-res MS/MS data, one might use a fragment_bin_tol of 0.02 and a corresponding fragment_bin_offset of 0.0.\n" +
                        "Allowed values are between 0.0 and 1.0. The actual offset value is scaled by the fragment_bin_tol value.\n" +
                        "I know this is esoteric and any normal user should not give this parameter any thought beyond using the recommended settings.")
                .create();


        final List<String> isoCorrectionVals = MAP_COMET_ISO_ERROR_CODES.entrySet().stream()
                .sorted(Map.Entry.comparingByKey())
                .map(Map.Entry::getValue).collect(Collectors.toList());
        uiComboIsoErrors = UiUtils.createUiCombo(isoCorrectionVals);

        UiCombo uiComboPrecTolTypes = UiUtils.createUiCombo(new String[] {"0", "1"});
        FormEntry fePrecTolType = mu.feb(CometParams.PROP_precursor_tolerance_type, uiComboPrecTolTypes)
                .label("Precursor tolerance type")
                .tooltip("<html>This parameter controls how the “peptide_mass_tolerance” parameter is applied.\n" +
                        "That tolerance can be applied to the singly charged peptide mass or it can be applied to the precursor m/z.\n" +
                        "Note that this parameter is applied only when amu or mmu tolerances are specified.\n" +
                        "It is ignored when ppm tolerances are specified.\n" +
                        "Valid values are 0 or 1.\n" +
                        "Set this parameter to 0 to specify that the mass tolerance is applied to the singly charged peptide mass.\n" +
                        "Set this parameter to 1 to specify that the mass tolerance is applied to the precursor m/z.")
                .create();

        FormEntry feIsotopeError = mu.feb(CometParams.PROP_isotope_error, uiComboIsoErrors)
                .label("Isotope error")
                .tooltip("<html>String of the form 0/1/2/3 indicating which isotopic\n"
                        + "peak selection errors MSFragger will try to correct.")
                .create();

        mu.add(p, fePrecTolUnits.label(), mu.ccR());
        mu.add(p, fePrecTolUnits.comp).split(2);
        mu.add(p, fePrecTolHi.comp);
        mu.add(p, feFragBinTol.label(), mu.ccR()).gapLeft("10px");
        mu.add(p, feFragBinTol.comp).split(3);
        mu.add(p, feFragBinOffset.label());
        mu.add(p, feFragBinOffset.comp).pushX().wrap();

        mu.add(p, fePrecTolType.label(), mu.ccR());
        mu.add(p, fePrecTolType.comp).spanX().wrap();

        mu.add(p, feIsotopeError.label(), mu.ccR());
        mu.add(p, feIsotopeError.comp).spanX().wrap();

        return p;
    }

    private JPanel createPanelDigest() {
        // Digest panel
        JPanel p = mu.newPanel("Protein Digestion", true);

        uiComboEnzymes = UiUtils.createUiCombo(ENZYMES.stream().map(msfe -> msfe.name).collect(Collectors.toList()));
        Optional<MsfraggerEnzyme> trypsin = ENZYMES.stream().filter(e -> e.name.toLowerCase().startsWith("trypsin")).min(Comparator.comparing(o -> o.name));
        trypsin.ifPresent(msfraggerEnzyme -> uiComboEnzymes.setSelectedItem(msfraggerEnzyme.name));
        FormEntry feEnzymeList = mu.feb(PROP_misc_comet_enzyme_dropdown_1, uiComboEnzymes)
                .label("Load rules")
                .tooltip("<html>Load one of default definitions of enzyme cleavage rules.\n"
                        + "You can still edit the name and rules manually after loading.\n"
                        + "<b>Note about custom enzymes:</b> To use an enzyme not listed in\n"
                        + "the digestion rules drop-down menu, enter the custom cleavage rules\n"
                        + "but be sure to set the ‘Enzyme name’ field to ‘nonspecific’.\n"
                        + "Select ‘ENZYMATIC’ from the ‘Cleavage’ drop-down menu.").create();
        uiComboEnzymes.addItemListener(event -> {
            if (event.getStateChange() == ItemEvent.SELECTED) {
                Object item = event.getItem();
                log.debug("User selected enzyme: {}, of class {}", item, item.getClass());
                if (!(item instanceof String)) {
                    throw new IllegalStateException("Ui Combo Boxes should just contain strings");
                }
                String name = (String) item;
                MsfraggerEnzyme enzyme = ENZYMES.stream()
                        .filter(msfe -> msfe.name.equals(name)).findFirst()
                        .orElseThrow(() -> new IllegalStateException(
                                "Enzymes list should have contained the name from dropdown"));
                uiTextEnzymeName.setText(enzyme.name.equalsIgnoreCase("custom") ? "nonspecific" : enzyme.name);
                uiTextCuts.setText(enzyme.cut);
                uiTextNocuts.setText(enzyme.nocuts);
                uiComboSense.setSelectedItem(enzyme.sense);
                if ("nonspecific".equals(item)) {
                    uiComboCleavage.setSelectedItem(CleavageType.NONSPECIFIC.name());
                } else if (uiComboCleavage.getSelectedItem() != null) {
                    uiComboCleavage.setSelectedItem(uiComboCleavage.getSelectedItem().toString());
                }
            }
        });

        uiComboEnzymes2 = UiUtils.createUiCombo(ENZYMES.stream().map(msfe -> msfe.name).collect(Collectors.toList()));
        trypsin.ifPresent(msfraggerEnzyme -> uiComboEnzymes2.setSelectedItem(msfraggerEnzyme.name));
        FormEntry feEnzymeList2 = mu.feb(PROP_misc_comet_enzyme_dropdown_2, uiComboEnzymes2)
                .label("Load rules")
                .tooltip("<html>Load one of default definitions of enzyme cleavage rules.\n"
                        + "You can still edit the name and rules manually after loading.\n"
                        + "<b>Note about custom enzymes:</b> To use an enzyme not listed in\n"
                        + "the digestion rules drop-down menu, enter the custom cleavage rules\n"
                        + "but be sure to set the ‘Enzyme name’ field to ‘nonspecific’.\n"
                        + "Select ‘ENZYMATIC’ from the ‘Cleavage’ drop-down menu.").create();
        uiComboEnzymes2.addItemListener(event -> {
            if (event.getStateChange() == ItemEvent.SELECTED) {
                Object item = event.getItem();
                log.debug("User selected enzyme: {}, of class {}", item, item.getClass());
                if (!(item instanceof String)) {
                    throw new IllegalStateException("Ui Combo Boxes should just contain strings");
                }
                String name = (String) item;
                MsfraggerEnzyme enzyme = ENZYMES.stream()
                        .filter(msfe -> msfe.name.equals(name)).findFirst()
                        .orElseThrow(() -> new IllegalStateException(
                                "Enzymes list should have contained the name from dropdown"));
                uiTextEnzymeName2.setText(enzyme.name.equalsIgnoreCase("custom") ? "nonspecific" : enzyme.name);
                uiTextCuts2.setText(enzyme.cut);
                uiTextNocuts2.setText(enzyme.nocuts);
                uiComboSense2.setSelectedItem(enzyme.sense);
                if ("nonspecific".equals(item)) {
                    uiComboCleavage.setSelectedItem(CleavageType.NONSPECIFIC.name());
                } else if (uiComboCleavage.getSelectedItem() != null) {
                    uiComboCleavage.setSelectedItem(uiComboCleavage.getSelectedItem().toString());
                }
            }
        });


        FocusAdapter enzymeSpecFocusListener = new FocusAdapter() {
            @Override
            public void focusLost(FocusEvent evt) {
                super.focusLost(evt);
                final String cuts = StringUtils.sortedChars(uiTextCuts.getNonGhostText());
                final String nocuts = StringUtils.sortedChars(uiTextNocuts.getNonGhostText());
                java.util.List<MsfraggerEnzyme> enzymes = ENZYMES.stream()
                        .map(e -> new MsfraggerEnzyme(e.name, StringUtils.sortedChars(e.cut),
                                StringUtils.sortedChars(e.nocuts), e.sense))
                        .collect(Collectors.toList());
                java.util.List<String> matching = enzymes.stream()
                        .filter(e -> e.cut.equals(cuts) && e.nocuts.equals(nocuts))
                        .map(e -> e.name).collect(Collectors.toList());
                log.warn("Found matching enzymes: {}", matching);
                if (matching.contains("nonspecific")) {
                    trySelectEnzymeDropdown("nonspecific");
                } else if (!matching.isEmpty()) {
                    trySelectEnzymeDropdown(matching.get(0));
                } else {
                    trySelectEnzymeDropdown("custom");
                }
            }
        };

//        CleavageType[] cleavageTypes = new CleavageType[] {CleavageType.ENZYMATIC};
        final CleavageType[] cleavageTypes = CleavageType.values();
        java.util.List<String> cleavageTypeNames = Arrays.stream(cleavageTypes).map(Enum::name)
                .collect(Collectors.toList());
        uiComboCleavage = UiUtils.createUiCombo(cleavageTypeNames);
        FormEntry feCleavageType = mu.feb(CometParams.PROP_num_enzyme_termini, uiComboCleavage).label("Cleavage").create();
        FormEntry feClipM = mu.feb(CometParams.PROP_clip_nterm_methionine, new UiCheck("Clip N-term M", null))
                .tooltip("Trim protein N-terminal Methionine as a variable modification").create();


        uiTextEnzymeName = new UiText();
        FormEntry feEnzymeName = mu.feb(CometParams.PROP_search_enzyme_number_NAME, uiTextEnzymeName).label("Enzyme name 1").create();
        uiTextCuts = UiUtils.uiTextBuilder().cols(6).filter("[^A-Z-]").text("KR").create();
        uiTextCuts.addFocusListener(enzymeSpecFocusListener);
        FormEntry feCuts = mu.feb(PROP_MISC_search_enzyme_cut_1, uiTextCuts).label("Cuts 1")
                .tooltip("Capital letters for amino acids after which the enzyme cuts.").create();
        uiTextNocuts = UiUtils.uiTextBuilder().cols(3).filter("[^A-Z-@]").text("P").create();
        uiTextNocuts.addFocusListener(enzymeSpecFocusListener);
        FormEntry feNocuts = mu.feb(PROP_MISC_search_enzyme_nocut_1, uiTextNocuts).label("No cuts 1")
                .tooltip("Amino acids before which the enzyme won't cut.").create();
        uiComboSense = UiUtils.createUiCombo(new String[]{"N", "C"});
        FormEntry feSense = mu.feb(PROP_MISC_search_enzyme_sense_1, uiComboSense).label("Sense 1").create();


        uiTextEnzymeName2 = new UiText();
        FormEntry feEnzymeName2 = mu.feb(CometParams.PROP_search_enzyme2_number_NAME, uiTextEnzymeName2).label("Enzyme name 2").create();
        uiTextCuts2 = UiUtils.uiTextBuilder().cols(6).filter("[^A-Z-@]").text("KR").create();
        uiTextCuts2.addFocusListener(enzymeSpecFocusListener);
        FormEntry feCuts2 = mu.feb(PROP_MISC_search_enzyme_cut_2, uiTextCuts2).label("Cuts 2")
                .tooltip("Capital letters for amino acids after which the enzyme cuts.").create();
        uiTextNocuts2 = UiUtils.uiTextBuilder().cols(3).filter("[^A-Z-]").text("P").create();
        uiTextNocuts2.addFocusListener(enzymeSpecFocusListener);
        FormEntry feNocuts2 = mu.feb(PROP_MISC_search_enzyme_nocut_2, uiTextNocuts2).label("No cuts 2")
                .tooltip("Amino acids before which the enzyme won't cut.").create();
        uiComboSense2 = UiUtils.createUiCombo(new String[]{"N", "C"});
        FormEntry feSense2 = mu.feb(PROP_MISC_search_enzyme_sense_2, uiComboSense2).label("Sense 2").create();


        UiSpinnerInt uiSpinnerMissedCleavages = new UiSpinnerInt(2, 0, 1000, 1);
        uiSpinnerMissedCleavages.setColumns(2);
        FormEntry feMissedCleavages = mu.feb(CometParams.PROP_allowed_missed_cleavage, uiSpinnerMissedCleavages).label("Missed cleavages").create();


        FormEntry fePepLenMin = mu.feb(PROP_MISC_digest_length_lo, new UiSpinnerInt(7, 0, 1000, 1, 3))
                .label("Peptide length").create();
        FormEntry fePepLenMax = mu.feb(PROP_MISC_digest_length_hi, new UiSpinnerInt(50, 0, 1000, 1, 3))
                .create();
        UiSpinnerDouble uiSpinnerDigestMassLo = new UiSpinnerDouble(200, 0, 50000, 100,
                new DecimalFormat("0.#"));
        uiSpinnerDigestMassLo.setColumns(6);
        FormEntry fePepMassLo = mu.feb(PROP_misc_comet_digest_mass_lo, uiSpinnerDigestMassLo).label("Peptide mass range").create();
        UiSpinnerDouble uiSpinnerDigestMassHi = new UiSpinnerDouble(5000, 0, 50000, 100,
                new DecimalFormat("0.#"));
        uiSpinnerDigestMassHi.setColumns(6);
        FormEntry fePepMassHi = mu.feb(PROP_misc_comet_digest_mass_hi, uiSpinnerDigestMassHi).create();


        mu.add(p, feCleavageType.label(), mu.ccL()).span(2).split(13);
        mu.add(p, feCleavageType.comp).minWidth("100px");
        mu.add(p, feClipM.comp).wrap();
        mu.add(p, feClipM.comp).wrap();

        JPanel p2 = mu.newPanel(null, true);
        // BEGIN enzyme 1
        mu.add(p2, feEnzymeName.label(), mu.ccL()).span(2).split(2);
        mu.add(p2, feEnzymeName.comp, mu.ccL()).minWidth("100px");
        mu.add(p2, feEnzymeList.label(), mu.ccR());
        mu.add(p2, feEnzymeList.comp);
        mu.add(p2, feCuts.label()).gapLeft("5px");
        mu.add(p2, feCuts.comp);
        mu.add(p2, feNocuts.label());
        mu.add(p2, feNocuts.comp);
        mu.add(p2, feMissedCleavages.label(), mu.ccL());
        mu.add(p2, feMissedCleavages.comp).split(2);
        mu.add(p2, feSense.label(), mu.ccL());
        mu.add(p2, feSense.comp).split(2).spanX().wrap();

        // BEGIN enzyme 2
        mu.add(p2, feEnzymeName2.label(), mu.ccL()).span(2).split(2);
        mu.add(p2, feEnzymeName2.comp, mu.ccL()).minWidth("120px");
        mu.add(p2, feEnzymeList2.label(), mu.ccR());
        mu.add(p2, feEnzymeList2.comp);
        mu.add(p2, feCuts2.label()).gapLeft("5px");
        mu.add(p2, feCuts2.comp);
        mu.add(p2, feNocuts2.label());
        mu.add(p2, feNocuts2.comp);
        mu.add(p2, feSense2.label(), mu.ccL());
        mu.add(p2, feSense2.comp).split(2).spanX().wrap();
        // END enzyme 2
        mu.add(p, p2).wrap();

        mu.add(p, fePepLenMin.label(), mu.ccL()).split(12);
        mu.add(p, fePepLenMin.comp).split(3);
        mu.add(p, new JLabel("-"));
        mu.add(p, fePepLenMax.comp);
        mu.add(p, fePepMassLo.label(), mu.ccR());
        mu.add(p, fePepMassLo.comp).split(3);
        mu.add(p, new JLabel("-"));
        mu.add(p, fePepMassHi.comp);

        return p;
    }

    /**
     * Panel with all the basic options.
     */
    private JPanel createPanelBasicOptions() {
        JPanel pBase = mu.newPanel("Common Options (Advanced Options are at the end of the page)", true);
        mu.add(pBase, createPanelBasicPeakMatch()).growX().wrap();
        mu.add(pBase, createPanelDigest()).growX().wrap();

        return pBase;
    }

    private static Object[][] convertModsToVarModsData(List<Mod> mods) {
        Object[][] data = new Object[CometParams.VAR_MOD_COUNT_MAX][TABLE_VAR_MODS_COL_NAMES.length];
        for (int i = 0; i < data.length; i++) {
            data[i][0] = false;
            data[i][1] = null;
            data[i][2] = null;
            data[i][3] = null;
        }
        if (mods.size() > data.length) {
            throw new IllegalStateException("loaded mods list length is longer than msfragger supports");
        }
        for (int i = 0; i < mods.size(); i++) {
            Mod m = mods.get(i);
            data[i][0] = m.isEnabled;
            data[i][1] = m.sites;
            data[i][2] = m.massDelta;
            data[i][3] = m.maxOccurrences;
        }
        return data;
    }

    private ModsTable createTableVarMods() {
        Object[][] data = convertModsToVarModsData(Collections.emptyList());
        ModsTableModel m = new ModsTableModel(
                TABLE_VAR_MODS_COL_NAMES,
                new Class<?>[]{Boolean.class, String.class, Double.class, Integer.class},
                new boolean[]{true, true, true, true},
                new int[]{0, 1, 2, 3},
                data);
        final ModsTable t = new ModsTable(m, TABLE_VAR_MODS_COL_NAMES, CometPanel::convertModsToVarModsData);
        Fragpipe.rename(t, "table.var-mods", TAB_PREFIX);

        t.setToolTipText(
                "<html>Variable Modifications.<br/>\n" +
                        "Values:<br/>\n" +
                        "<ul>\n" +
                        "<li>A-Z amino acid codes</li>\n" +
                        "<li>* is used to represent any amino acid</li>\n" +
                        "<li>^ is used to represent a terminus</li>\n" +
                        "<li>[ is a modifier for protein N-terminal</li>\n" +
                        "<li>] is a modifier for protein C-terminal</li>\n" +
                        "<li>n is a modifier for peptide N-terminal</li>\n" +
                        "<li>c is a modifier for peptide C-terminal</li>\n" +
                        "</ul>\n" +
                        "Syntax Examples:\n" +
                        "<ul>\n" +
                        "<li>15.9949 M 3(for oxidation on methionine)</li>\n" +
                        "<li>79.66331 STY 1(for phosphorylation)</li>\n" +
                        "<li>-17.0265 nQnC 1(for pyro-Glu or loss of ammonia at peptide N-terminal)</li>\n" +
                        "</ul>\n" +
                        "Example (M oxidation and N-terminal acetylation):\n" +
                        "<ul>\n" +
                        "<li>variable_mod_01 = 15.9949 M 3</li>\n" +
                        "<li>variable_mod_02 = 42.0106 [^ 1</li>\n" +
                        "</ul>");
        t.setDefaultRenderer(Double.class, new TableCellDoubleRenderer());
        t.setDefaultRenderer(Integer.class, new TableCellIntRenderer());

        // set cell editor for max occurs for var mods
        DefaultCellEditor cellEditorMaxOccurs = new TableCellIntSpinnerEditor(1, 5, 1);
        t.setDefaultEditor(Integer.class, cellEditorMaxOccurs);
        t.setFillsViewportHeight(true);

        return t;
    }

    private static Object[][] convertModsToFixTableData(List<Mod> mods) {
        Object[][] data = new Object[CometParams.ADDONS_HUMAN_READABLE.length][TABLE_FIX_MODS_COL_NAMES.length];
        for (int i = 0; i < data.length; i++) {
            data[i][0] = false;
            data[i][1] = CometParams.ADDONS_HUMAN_READABLE[i];
            data[i][2] = 0.0;
        }
        if (mods.size() > data.length) {
            throw new IllegalStateException("mod list length larger than fix mods used by fragger");
        }
        for (int i = 0; i < mods.size(); i++) {
            Mod m = mods.get(i);
            data[i][0] = m.isEnabled;
            data[i][1] = m.sites;
            data[i][2] = m.massDelta;
        }
        return data;
    }

    private ModsTable createTableFixMods() {
        Object[][] data = convertModsToFixTableData(Collections.emptyList());

        ModsTableModel m = new ModsTableModel(
                TABLE_FIX_MODS_COL_NAMES,
                new Class<?>[]{Boolean.class, String.class, Double.class},
                new boolean[]{true, false, true},
                new int[]{0, 1, 2},
                data);

        ModsTable t = new ModsTable(m, TABLE_FIX_MODS_COL_NAMES, CometPanel::convertModsToFixTableData);
        Fragpipe.rename(t, "table.fix-mods", TAB_PREFIX);

        t.setToolTipText(
                "<html>Fixed Modifications.<br/>Act as if the mass of aminoacids/termini was permanently changed.");
        t.setDefaultRenderer(Double.class, new TableCellDoubleRenderer());
        t.setFillsViewportHeight(true);

        return t;
    }

    private JPanel createPanelMods() {
        // Panel with modifications
        JPanel pMods = new JPanel(new MigLayout(new LC().fillX()));
        pMods.setBorder(new TitledBorder("Modifications"));

        JPanel pVarmods = new JPanel(new MigLayout(new LC()));
        pVarmods.setBorder(new TitledBorder("Variable modifications"));

        FormEntry feMaxVarmodsPerMod = mu.feb(CometParams.PROP_max_variable_mods_in_peptide, new UiSpinnerInt(3, 0, 5, 1, 4))
                .label("Max variable mods on a peptide")
                .tooltip("<html>The maximum number of variable modifications allowed per\n" +
                        "peptide sequence. This number does not include fixed modifications.").create();

        FormEntry feRequireVarMods = mu.feb(CometParams.PROP_require_variable_mod, new UiCheck("Require var mod presence", null)).create();

        tableVarMods = createTableVarMods();
        SwingUtilities.invokeLater(() -> {
            setJTableColSize(tableVarMods, 0, 20, 150, 50);
        });
        JScrollPane varModsScroll = new JScrollPane(tableVarMods,
                JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        //tableScrollVarMods.setPreferredSize(new Dimension(tableScrollVarMods.getPreferredSize().width, 140));

        pVarmods.add(feMaxVarmodsPerMod.label(), new CC().alignX("right"));
        pVarmods.add(feMaxVarmodsPerMod.comp);
        pVarmods.add(feRequireVarMods.comp, new CC().wrap());
        pVarmods
                .add(varModsScroll, new CC().minHeight("100px").maxHeight("150px").spanX().wrap());

        JPanel pFixmods = new JPanel(new MigLayout(new LC()));
        pFixmods.setBorder(new TitledBorder("Fixed modifications"));

        tableFixMods = createTableFixMods();
        SwingUtilities.invokeLater(() -> {
            setJTableColSize(tableFixMods, 0, 20, 150, 50);
        });
        JScrollPane tableScrollFixMods = new JScrollPane(tableFixMods,
                JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        pFixmods.add(tableScrollFixMods,
                new CC().minHeight("100px").maxHeight("200px").growX().spanX().wrap());

        pMods.add(pVarmods, new CC().wrap().growX());
        pMods.add(pFixmods, new CC().wrap().growX());

        pContent.add(pMods, new CC().wrap().growX());

        return pMods;
    }

    /**
     * Panel with all the advanced options.
     */
    private JPanel createPanelAdvancedOptions() {
        JPanel p = mu.newPanel("Advanced Options", new LC());

        mu.add(p, createPanelAdvancedSpectral()).growX().wrap();
        mu.add(p, createPanelAdvancedOutput()).growX().wrap();
        mu.add(p, createPanelAdvancedPeakMatch()).growX().wrap();

        return p;
    }

    private JPanel createPanelAdvancedSpectral() {
        JPanel p = mu.newPanel("Spectral Processing", true);

        FormEntry feMinPeaks = mu.feb(CometParams.PROP_minimum_peaks, new UiSpinnerInt(15, 0, 1000, 1, 4))
                .label("Min peaks").create();
//        FormEntry feUseTopN = mu.feb(CometParams.PROP_use_topN_peaks, new UiSpinnerInt(100, 0, 1000000, 10, 4)).label("Use top N peaks").create();
        UiSpinnerDouble spinnerMinIntensity = new UiSpinnerDouble(0.01, 0, Double.MAX_VALUE, 0.01, 2, new DecimalFormat("0.00"));
        spinnerMinIntensity.setColumns(4);
        FormEntry feMinRatio = mu.feb(CometParams.PROP_minimum_intensity, spinnerMinIntensity).label("Min intensity").create();
        UiSpinnerDouble uiSpinnerClearRangeMzLo = UiUtils.spinnerDouble(0, 0, 100000, 10)
                .setCols(5).setFormat("#.###").create();
        FormEntry feClearRangeMzLo = mu.feb(PROP_MISC_comet_clear_mz_lo, uiSpinnerClearRangeMzLo)
                .label("Clear m/z range").create();
        UiSpinnerDouble uiSpinnerClearRangeMzHi = UiUtils.spinnerDouble(0, 0, 100000, 10)
                .setCols(5).setFormat("#.###").create();
        FormEntry feClearRangeMzHi = mu.feb(PROP_MISC_comet_clear_mz_hi, uiSpinnerClearRangeMzHi)
                .create();

        FormEntry feRemovePrecPeak = mu.feb(CometParams.PROP_remove_precursor_peak, UiUtils.createUiCombo(CometRemovePrecursorPeak.getNames())).
                label("Remove precursor peak")
                .tooltip("Remove with all charge states is for ETD/ECD data and removes M+e peaks in addition to precursor peak")
                .create();
        DecimalFormat df1 = new DecimalFormat("0.#");
        FormEntry fePrecRemoveRangeLo = mu.feb(CometParams.PROP_remove_precursor_tolerance,
                        UiSpinnerDouble.builder(1.5, 0, 10, 0.1).setCols(5).setFormat(df1).create())
                .label("removal tol (Da)").create();

        mu.add(p, feMinPeaks.label(), mu.ccR());
        mu.add(p, feMinPeaks.comp).split(3).spanX();
        mu.add(p, feMinRatio.label()).gapBefore("20px");
        mu.add(p, feMinRatio.comp).wrap();

        mu.add(p, feClearRangeMzLo.label(), mu.ccR());
        mu.add(p, feClearRangeMzLo.comp).split(3).spanX();
        mu.add(p, new JLabel("-"));
        mu.add(p, feClearRangeMzHi.comp).wrap();

        mu.add(p, feRemovePrecPeak.label(), mu.ccR());
        mu.add(p, feRemovePrecPeak.comp).split(3).spanX();
        mu.add(p, fePrecRemoveRangeLo.label());
        mu.add(p, fePrecRemoveRangeLo.comp).pushX().wrap();;

        return p;
    }


    /**
     * Advanced peak matching panel
     */
    private JPanel createPanelAdvancedPeakMatch() {
        JPanel p = mu.newPanel("Advanced Peak Matching Options", true);

//        FormEntry feMinFragsModeling = mu.feb(MsfraggerParams.PROP_min_fragments_modelling, new UiSpinnerInt(2, 0, 1000, 1, 4)).label("Min frags modeling").create();
//        FormEntry feMinMatchedFrags = mu.feb(MsfraggerParams.PROP_min_matched_fragments, new UiSpinnerInt(4, 0, 1000, 1, 4)).label("Min matched frags").create();

        FormEntry feIonSeries = mu.feb(PROP_MISC_ion_series, new UiText(10))
                .label("Fragment ion series").tooltip(
                        "Which peptide ion series to check against.\n"
                                + "<b>Use spaces, commas or semicolons as delimiters</b>, e.g. \"b,y\"\n"
                                + "This mostly depends on fragmentation method.\n"
                                + "Typically \"b,y\" are used for CID and \"c,z\" for ECD.\n"
                                + "MSFragger can generate \"a,b,c,x,y,z\" ion series by default,\n"
                                + "Y refers to capital Y ions for glyco mode\n"
                                + "<b>you can define your own in 'Define custom ion series' field</b>.\n"
                                + "If you define custom series, you will need to include the name you\n"
                                + "gave it here.").create();

        String tooltipPrecursorCHarge =
                "Assume range of potential precursor charge states.\n" +
                        "Only relevant when override_charge is set to 1 or \n" +
                        "there is no charge information in scans.";
        FormEntry fePrecursorChargeLo = mu.feb(PROP_MISC_comet_precursor_charge_lo, new UiSpinnerInt(1, 0, 30, 1, 2))
                .tooltip(tooltipPrecursorCHarge).create();
        FormEntry fePrecursorChargeHi = mu.feb(PROP_MISC_comet_precursor_charge_hi, new UiSpinnerInt(4, 0, 30, 1, 2))
                .tooltip(tooltipPrecursorCHarge).create();
        FormEntry feOverrideCharge = mu.feb(CometParams.PROP_override_charge, UiUtils.createUiCombo(new String[]{"0", "1", "2", "3"}))
                .tooltip("<html>\n" +
                        "    0 = keep any known precursor charge state values in the input files\n" +
                        "    1 = ignore known precursor charge state values in the input files and instead\n" +
                        "        use the charge state range specified by the “precursor_charge” parameter.\n" +
                        "    2 = only search precursor charge state values that are within the range specified\n" +
                        "        by the “precursor_charge” parameter.\n" +
                        "    3 = keep any known precursor charge state values. For unknown charge states,\n" +
                        "        search as singly charged if there is no signal above the precursor m/z or use the “precursor_charge” range otherwise\n")
                .create();


        UiSpinnerInt uiSpinnerMaxFragCharge = new UiSpinnerInt(2, 1, 20, 1, 2);
        FormEntry feMaxFragCharge = mu.feb(CometParams.PROP_max_fragment_charge, uiSpinnerMaxFragCharge)
                .label("Max fragment charge").tooltip("Enabled only when Deisotope = 0.").create();



        mu.add(p, feMaxFragCharge.label(), mu.ccR());
        mu.add(p, feMaxFragCharge.comp).pushX().wrap();
        mu.add(p, feIonSeries.label(), mu.ccR());
        mu.add(p, feIonSeries.comp).growX();
        mu.add(p, feOverrideCharge.comp).split(4).spanX();
        mu.add(p, fePrecursorChargeLo.comp);
        mu.add(p, new JLabel("-"));
        mu.add(p, fePrecursorChargeHi.comp).wrap();

        return p;
    }

    private JPanel createPanelAdvancedOutput() {
        JPanel p = mu.newPanel("Advanced Output Options", true);


//        FormEntry feReportTopN = mu.feb(MsfraggerParams.PROP_output_report_topN,
//                        new UiSpinnerInt(1, 1, 10000, 1, 4)).label("Report top N")
//                .tooltip("Report top N PSMs per input spectrum.").create();
//        UiSpinnerDouble uiSpinnerOutputMaxExpect = new UiSpinnerDouble(50, 0, Double.MAX_VALUE, 1,
//                new DecimalFormat("0.#"));
//        uiSpinnerOutputMaxExpect.setColumns(4);
//        FormEntry feOutputMaxExpect = mu.feb(MsfraggerParams.PROP_output_max_expect, uiSpinnerOutputMaxExpect).label("Output max expect")
//                .tooltip("Suppresses reporting of PSM if top hit has<br> expectation greater than this threshold").create();


//        uiComboOutputType = UiUtils.createUiCombo(FraggerOutputType.values());
//        FormEntry feOutputType = mu.feb(MsfraggerParams.PROP_output_format, uiComboOutputType).label("Output format")
//                .tooltip("How the search results are to be reported.\n" +
//                        "Downstream tools only support pepXML format.\n\n" +
//                        "Use PIN if you want to process the result results with Percolator by yourself.\n" +
//                        "Use TSV (tab delimited file) if you want to process \n" +
//                        "search results yourself for easier import into other software.").create();

//        FormEntry feReportAltProts = mu.feb(MsfraggerParams.PROP_report_alternative_proteins, new UiCheck("Report alternative proteins", null, false)).create();

//        uiCheckWriteCalibratedMgf = UiUtils.createUiCheck("Write calibrated MGF", false);
//        FormEntry feCheckWriteCalibratedMgf = mu.feb(MsfraggerParams.PROP_write_calibrated_mgf, uiCheckWriteCalibratedMgf).create();

//        mu.add(p, feReportTopN.label(), mu.ccR());
//        mu.add(p, feReportTopN.comp).growX();
//        mu.add(p, feReportAltProts.comp);
//        mu.add(p, feOutputMaxExpect.label()).split().spanX().gapLeft("10px");
//        mu.add(p, feOutputMaxExpect.comp).pushX().wrap();
//        mu.add(p, feOutputType.label(), mu.ccR());
//        mu.add(p, feOutputType.comp);
//        mu.add(p, feCheckWriteCalibratedMgf.comp).wrap();

        return p;
    }

    private void postInitAddActionListeners() {
        TabWorkflow tabWorkflow = Fragpipe.getStickyStrict(TabWorkflow.class);
    }

    private void setJTableColSize(JTable table, int colIndex, int minW, int maxW, int prefW) {
        table.getColumnModel().getColumn(colIndex).setMinWidth(minW);
        table.getColumnModel().getColumn(colIndex).setMaxWidth(maxW);
        table.getColumnModel().getColumn(colIndex).setPreferredWidth(prefW);
    }

    private void updateRowHeights(JTable table) {
        for (int row = 0; row < table.getRowCount(); row++) {
            int rowHeight = table.getRowHeight();

            for (int column = 0; column < table.getColumnCount(); column++) {
                Component comp = table.prepareRenderer(table.getCellRenderer(row, column), row, column);
                rowHeight = Math.max(rowHeight, comp.getPreferredSize().height);
            }

            table.setRowHeight(row, rowHeight);
        }
    }

    private void formFrom(CometParams params) {
        Map<String, String> map = confToUiMap(params);
        Map<String, String> prependedMap = MapUtils.remapKeys(map, k -> StringUtils.prependOnce(k, TAB_PREFIX));
        Map<String, String> translatedMap = FragpipeCacheUtils.translateValuesToUi(prependedMap);
        formFrom(translatedMap);
        tableVarMods.setData(params.getVariableMods());
        tableFixMods.setData(params.getAdditionalMods());
        updateRowHeights(tableVarMods);
        setJTableColSize(tableVarMods, 0, 20, 150, 50);
        updateRowHeights(tableFixMods);
        setJTableColSize(tableFixMods, 0, 20, 150, 50);
    }

    private CometParams formCollect() {
        Map<String, String> map = formToMap();

        // ram and threads parameters have been moved to Workflow tab, need to re-inject them when saving fragger params
        TabWorkflow tabWorkflow = Fragpipe.getStickyStrict(TabWorkflow.class);
        map.put(CometParams.PROP_num_threads, itos(tabWorkflow.getThreads()));

//        CometParams ps = paramsFromMap(map);
        CometParams params = paramsFromMap(map);

        // before collecting mods, make sure that no table cell editor is open
        stopJTableEditing(tableFixMods);
        stopJTableEditing(tableVarMods);

        java.util.List<Mod> modsVar = formToMap(tableVarMods.model);
        params.setVariableMods(modsVar);
        java.util.List<Mod> modsFix = formToMap(tableFixMods.model);
        params.setAdditionalMods(modsFix);

        return params;
    }

    private boolean stopJTableEditing(JTable t) {
        TableCellEditor editor = t.getCellEditor();
        if (editor == null) {
            log.debug("cell editor was null");
            return true;
        }
        log.debug("cell editor existed, trying to close");
        return editor.stopCellEditing();
    }

    private java.util.List<Mod> formToMap(ModsTableModel model) {
        return model.getModifications();
    }

    private void formFrom(Map<String, String> map) {
        log.debug("SwingUtils.valuesSet(this, map) is being called from formFrom(Map<String, String> map)");
        SwingUtils.valuesSet(this, map);
    }

    private Map<String, String> formToMap() {
        Map<String, String> map = SwingUtils.valuesGet(this, null);
        HashMap<String, String> m = new HashMap<>();
        map.forEach((k, v) -> m.put(StringUtils.stripLeading(k, TAB_PREFIX), v));
        return m;
    }

    public CometParams getParams() {
        return formCollect();
    }

    /**
     * Converts textual representations of all fields in the form to stadard {@link CometParams}.
     */
    private CometParams paramsFromMap(Map<String, String> map) {
        CometParams p = new CometParams();
        final double[] precursorRemoveRange = new double[2];
        final double[] clearMzRange = new double[2];
        final double[] digestMassRange = new double[2];
        final int[] precursorChargeRange = new int[2];

        final Set<String> propNames = Arrays.stream(CometParams.PROP_NAMES).collect(Collectors.toSet());

        for (Map.Entry<String, String> e : map.entrySet()) {
            final String k = e.getKey();
            final String v = e.getValue();
            if (propNames.contains(k)) {
                // known property
                Function<String, String> converter = CONVERT_TO_FILE.getOrDefault(k, s -> s);
                String converted = converter.apply(v);

                if (PROP_MISC_ion_series.equals(k) && StringUtils.isNullOrWhitespace(converted)) {
                    // don't set ion series to be used in the fragger config file if the string is emtpty
                    continue;
                }
                p.getProps().setProp(k, converted);

            } else {
                // unknown prop, it better should be from the "misc" category we added in this panel
                if (PROPS_MISC_NAMES.contains(k) || k.startsWith("misc.")) {
                    log.trace("Found misc option: {}={}", k, v);

                    switch (k) {
                        case PROP_MISC_comet_remove_precursor_range_lo:
                            precursorRemoveRange[0] = Double.parseDouble(v);
                            break;
                        case PROP_MISC_comet_remove_precursor_range_hi:
                            precursorRemoveRange[1] = Double.parseDouble(v);
                            break;
                        case PROP_MISC_comet_clear_mz_lo:
                            clearMzRange[0] = Double.parseDouble(v);
                            break;
                        case PROP_MISC_comet_clear_mz_hi:
                            clearMzRange[1] = Double.parseDouble(v);
                            break;
                        case PROP_misc_comet_digest_mass_lo:
                            digestMassRange[0] = Double.parseDouble(v);
                            break;
                        case PROP_misc_comet_digest_mass_hi:
                            digestMassRange[1] = Double.parseDouble(v);
                            break;
                        case PROP_MISC_comet_precursor_charge_lo:
                            precursorChargeRange[0] = Integer.parseInt(v);
                            break;
                        case PROP_MISC_comet_precursor_charge_hi:
                            precursorChargeRange[1] = Integer.parseInt(v);
                            break;
                    }

                } else {
                    // we don't know what this option is, someone probably forgot to add it to the list of
                    // known ones
                    log.debug("Unknown prop name in fragger panel: [{}] with value [{}]", k, v);
                }
            }
        }

//        FraggerOutputType outType = p.getOutputFormat();
//        if (outType == null) {
//            throw new IllegalStateException("FraggerOutputType was not set by the point where we needed to provide the output extension.");
//        }

        return p;
    }

    private Map<String, String> confToUiMap(CometParams params) {
        HashMap<String, String> map = new HashMap<>();
        for (Map.Entry<String, Props.Prop> e : params.getProps().getMap().entrySet()) {
            if (e.getValue().isEnabled) {
                final Function<String, String> converter = CONVERT_TO_GUI.getOrDefault(e.getKey(), Function.identity());
                try {
                    String converted = converter.apply(e.getValue().value);
                    map.put(e.getKey(), converted);
                } catch (Exception ex) {
                    //throw new IllegalStateException(ex);
                    log.error("Error converting parameter [{}={}]", e.getKey(), e.getValue().value);
                }
            }
        }

        // special treatment of some fields
        double[] clearMzRange = params.getClearMzRange();
        double[] digestMassRange = params.getDigestMassRange();
        int[] precursorCharge = params.getPrecursorCharge();
        DecimalFormat fmt = new DecimalFormat("0.#");
        DecimalFormat fmt2 = new DecimalFormat("0.##");
        map.put(PROP_MISC_comet_clear_mz_lo, fmt.format(clearMzRange[0]));
        map.put(PROP_MISC_comet_clear_mz_hi, fmt.format(clearMzRange[1]));
        map.put(PROP_misc_comet_digest_mass_lo, fmt.format(digestMassRange[0]));
        map.put(PROP_misc_comet_digest_mass_hi, fmt.format(digestMassRange[1]));
        map.put(PROP_MISC_comet_precursor_charge_lo, fmt.format(precursorCharge[0]));
        map.put(PROP_MISC_comet_precursor_charge_hi, fmt.format(precursorCharge[1]));

        return map;
    }

    private boolean modListContainsIllegalSites(List<Mod> mods) {
        return mods.stream().anyMatch(m -> m.sites != null && m.sites.contains("[*"));
    }

    @Subscribe(sticky = true, threadMode = ThreadMode.MAIN_ORDERED)
    public void on(NoteConfigSearchEngine m) {
        final boolean isRun = m.type == NoteConfigSearchEngine.Type.Comet;
        log.debug("Setting Comet isRun: {}", isRun);
        checkRun.setSelected(isRun);
        isRunBackupVar = isRun;
        updateEnabledStatus(this, isRun && m.isValid());
    }

    @Subscribe(sticky = true, threadMode = ThreadMode.MAIN_ORDERED)
    public void on(NoteConfigComet m) {
        updateEnabledStatus(this, m.isValid());
    }

    @Subscribe
    public void on(MessagePrecursorSelectionMode m) {
        log.debug("Received MessagePrecursorSelectionMode [{}]. Doing nothing.", m.mode.name());
    }

    @Subscribe
    public void on(MessageCometParamsUpdate m) {
        log.debug("Got MessageCometParamsUpdate, updating TabComet via formFrom(m.params)");
        formFrom(m.params);
    }

    @Subscribe
    public void on(MessageSearchType m) {
        loadDefaults(m.type);
    }

    public boolean isRun() {
        return SwingUtils.isEnabledAndChecked(checkRun);
    }

    public String getOutputFileExt() {
        return getOutputType().getExtension();
    }

    public FraggerOutputType getOutputType() {
        String val = uiComboOutputType.getItemAt(uiComboOutputType.getSelectedIndex());
        return FraggerOutputType.valueOf(val);
    }

    private void actionBtnConfigSave(ActionEvent e) {
        // now save the actual user's choice
        JFileChooser fc = FileChooserUtils.builder("Choose where params file should be saved")
                .approveButton("Save")
                .multi(false).acceptAll(true).mode(FileChooserUtils.FcMode.FILES_ONLY)
                .paths(Seq.of(
                        Fragpipe.propsVarGet(ThisAppProps.PROP_FRAGGER_PARAMS_FILE_IN),
                        Fragpipe.propsVarGet(PROP_FILECHOOSER_LAST_PATH))).create();

        fc.setSelectedFile(new File(CometParams.CACHE_FILE));
        final Component parent = SwingUtils.findParentFrameForDialog(this);
        if (JFileChooser.APPROVE_OPTION == fc.showSaveDialog(parent)) {
            File selectedFile = fc.getSelectedFile();
            Path path = Paths.get(selectedFile.getAbsolutePath());
            Fragpipe.propsVarSet(ThisAppProps.PROP_FRAGGER_PARAMS_FILE_IN, path.toString());
            Fragpipe.propsVarSet(PROP_FILECHOOSER_LAST_PATH, path.toString());

            // if exists, overwrite
            if (Files.exists(path)) {
                int overwrite = JOptionPane.showConfirmDialog(parent, "<html>File exists, overwrtie?<br/><br/>" + path, "Overwrite", JOptionPane.OK_CANCEL_OPTION);
                if (JOptionPane.OK_OPTION != overwrite) {
                    return;
                }
                try {
                    Files.delete(path);
                } catch (IOException ex) {
                    JOptionPane.showMessageDialog(parent, "Could not overwrite", "Overwrite", JOptionPane.ERROR_MESSAGE);
                    return;
                }
            }
            try {
                CometParams params = formCollect();
                params.save(new FileOutputStream(path.toFile()));
                params.save();

            } catch (IOException ex) {
                JOptionPane.showMessageDialog(parent, "<html>Could not save file: <br/>" + path + "<br/>" + ex.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
            }
        }
    }

    private void actionConfigLoadUserSelected(ActionEvent e) {
        FileNameExtensionFilter filter = new FileNameExtensionFilter("Properties/Params",
                "properties", "params", "para", "conf", "txt");
        JFileChooser fc = FileChooserUtils.create("Select saved file", "Load", false, FileChooserUtils.FcMode.FILES_ONLY, true, filter);
        fc.setFileFilter(filter);
        FileChooserUtils.setPath(fc, Stream.of(ThisAppProps.load(ThisAppProps.PROP_FRAGGER_PARAMS_FILE_IN)));

        Component parent = SwingUtils.findParentFrameForDialog(this);
        int saveResult = fc.showOpenDialog(parent);
        if (JFileChooser.APPROVE_OPTION == saveResult) {
            File f = fc.getSelectedFile();
            Path p = Paths.get(f.getAbsolutePath());
            ThisAppProps.save(ThisAppProps.PROP_FRAGGER_PARAMS_FILE_IN, p.toString());

            if (Files.exists(p)) {
                try {
                    CometParams params = formCollect();
                    params.load(new FileInputStream(f), true);
                    Bus.post(new MessageCometParamsUpdate(params));
                    params.save();

                } catch (Exception ex) {
                    JOptionPane.showMessageDialog(parent, "<html>Could not load the saved file: <br/>" + ex.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
                }
            } else {
                JOptionPane.showMessageDialog(parent, "<html>This is strange,<br/> "
                        + "but the file you chose to load doesn't exist anymore.", "Strange", JOptionPane.ERROR_MESSAGE);
            }
        }
    }

    private void actionBtnConfigLoad(ActionEvent actionEvent) {
        String option = (String) uiComboLoadDefaultsNames.getSelectedItem();

        if (LOAD_CUSTOM_CONFIG_OPTION.equals(option)) {
            actionConfigLoadUserSelected(actionEvent);
            return;
        }

        SearchTypeProp type = SEARCH_TYPE_NAME_MAPPING.get(option);
        if (type == null) {
            throw new IllegalStateException(String.format("No mapping for search type string '%s'", option));
        }
        if (loadDefaults(type, true)) {
            postSearchTypeUpdate(type, true);
        }
    }

    private void loadDefaults(SearchTypeProp type) {
        log.debug("TabComet loadDefaults() called for SearchTypeProp type={}", type.name());
        CometParams params = new CometParams();
        params.loadDefault();
        formFrom(params);

        // reset some fields that are not part of Fragger config file
    }

    /**
     * @return False if user's confirmation was required, but they cancelled the operation. True
     * otherwise.
     */
    private boolean loadDefaults(final SearchTypeProp type, boolean askConfirmation) {
        if (askConfirmation) {
            int confirmation = JOptionPane.showConfirmDialog(SwingUtils.findParentFrameForDialog(this), "Load " + type + " search default configuration?");
            if (JOptionPane.YES_OPTION != confirmation) {
                return false;
            }
        }
        loadDefaults(type);
        return true;
    }

    private boolean trySelectEnzymeDropdown(String name) {
        try {
            String saveCuts = uiTextCuts.getNonGhostText();
            String saveNouts = uiTextNocuts.getNonGhostText();
            uiComboEnzymes.setSelectedItem(name);
            if (true || "custom".equals(name)) { // remove 'true' to reset specificities to their definitions in our file
                uiTextCuts.setText(saveCuts);
                uiTextNocuts.setText(saveNouts);
            }
            return true;
        } catch (Exception ignored) {
        }
        return false;
    }

    /**
     * @return False if user's confirmation was required, but they cancelled the operation. True
     * otherwise.
     */
    private boolean postSearchTypeUpdate(SearchTypeProp type, boolean askConfirmation) {
        if (askConfirmation) {
            int confirmation = JOptionPane.showConfirmDialog(SwingUtils.findParentFrameForDialog(this),
                    "<html>Would you like to update options for other tools as well?<br/>"
                            + "<b>Highly recommended</b>, unless you're sure what you're doing)");
            if (JOptionPane.OK_OPTION != confirmation) {
                return false;
            }
        }
        Bus.post(new MessageSearchType(type));
        return true;
    }

    public String getEnzymeName() {
        return uiTextEnzymeName.getNonGhostText();
    }

    public String getEnzymeCut2() {
        return uiTextCuts2.getNonGhostText();
    }


}
