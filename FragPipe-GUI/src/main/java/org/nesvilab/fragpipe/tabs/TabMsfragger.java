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

package org.nesvilab.fragpipe.tabs;

import static org.nesvilab.fragpipe.tools.fragger.MsfraggerParams.ACTIVATION_TYPES;
import static org.nesvilab.fragpipe.tools.fragger.MsfraggerParams.ACTIVATION_TYPE_ALL;
import static org.nesvilab.fragpipe.tools.fragger.MsfraggerParams.ACTIVATION_TYPE_CID;
import static org.nesvilab.fragpipe.tools.fragger.MsfraggerParams.ACTIVATION_TYPE_ECD;
import static org.nesvilab.fragpipe.tools.fragger.MsfraggerParams.ACTIVATION_TYPE_ETD;
import static org.nesvilab.fragpipe.tools.fragger.MsfraggerParams.ACTIVATION_TYPE_HCD;
import static org.nesvilab.fragpipe.tools.fragger.MsfraggerParams.ANALYZER_TYPES;
import static org.nesvilab.fragpipe.tools.fragger.MsfraggerParams.ANALYZER_TYPE_ALL;
import static org.nesvilab.fragpipe.tools.fragger.MsfraggerParams.ANALYZER_TYPE_ASTMS;
import static org.nesvilab.fragpipe.tools.fragger.MsfraggerParams.ANALYZER_TYPE_FTMS;
import static org.nesvilab.fragpipe.tools.fragger.MsfraggerParams.ANALYZER_TYPE_ITMS;
import static org.nesvilab.fragpipe.tools.fragger.MsfraggerParams.GLYCO_OPTIONS;
import static org.nesvilab.fragpipe.tools.fragger.MsfraggerParams.GLYCO_OPTION_labile;
import static org.nesvilab.fragpipe.tools.fragger.MsfraggerParams.GLYCO_OPTION_nglycan;
import static org.nesvilab.fragpipe.tools.fragger.MsfraggerParams.GLYCO_OPTION_off;
import static org.nesvilab.fragpipe.tools.fragger.MsfraggerParams.PROP_group_variable;
import static org.nesvilab.fragpipe.tools.fragger.MsfraggerParams.PROP_mass_offsets_detailed;
import static org.nesvilab.fragpipe.util.MassOffsetUtils.floatArrToString;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.ItemSelectable;
import java.awt.event.ActionEvent;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.awt.event.HierarchyEvent;
import java.awt.event.HierarchyListener;
import java.awt.event.ItemEvent;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.io.PrintWriter;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.imageio.ImageIO;
import javax.swing.DefaultCellEditor;
import javax.swing.DefaultComboBoxModel;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.SwingUtilities;
import javax.swing.border.LineBorder;
import javax.swing.border.TitledBorder;
import javax.swing.filechooser.FileFilter;
import javax.swing.filechooser.FileNameExtensionFilter;
import javax.swing.table.TableCellEditor;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.apache.commons.lang3.ArrayUtils;
import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.ThreadMode;
import org.jooq.lambda.Seq;
import org.nesvilab.fragpipe.Fragpipe;
import org.nesvilab.fragpipe.api.Bus;
import org.nesvilab.fragpipe.api.FragpipeCacheUtils;
import org.nesvilab.fragpipe.api.ModsTable;
import org.nesvilab.fragpipe.api.ModsTableModel;
import org.nesvilab.fragpipe.api.SearchTypeProp;
import org.nesvilab.fragpipe.dialogs.DetailedOffsetEditDialog;
import org.nesvilab.fragpipe.messages.MessageMsfraggerParamsUpdate;
import org.nesvilab.fragpipe.messages.MessagePrecursorSelectionMode;
import org.nesvilab.fragpipe.messages.MessageSearchType;
import org.nesvilab.fragpipe.messages.MessageValidityMassCalibration;
import org.nesvilab.fragpipe.messages.NoteConfigDbsplit;
import org.nesvilab.fragpipe.messages.NoteConfigMsfragger;
import org.nesvilab.fragpipe.params.Props.Prop;
import org.nesvilab.fragpipe.params.ThisAppProps;
import org.nesvilab.fragpipe.tools.enums.CleavageType;
import org.nesvilab.fragpipe.tools.enums.FraggerOutputType;
import org.nesvilab.fragpipe.tools.enums.FraggerPrecursorMassMode;
import org.nesvilab.fragpipe.tools.enums.IntensityTransform;
import org.nesvilab.fragpipe.tools.enums.MassTolUnits;
import org.nesvilab.fragpipe.tools.enums.PrecursorMassTolUnits;
import org.nesvilab.fragpipe.tools.enums.RemovePrecursorPeak;
import org.nesvilab.fragpipe.tools.fragger.EnzymeProvider;
import org.nesvilab.fragpipe.tools.fragger.Mod;
import org.nesvilab.fragpipe.tools.fragger.MsfraggerEnzyme;
import org.nesvilab.fragpipe.tools.fragger.MsfraggerParams;
import org.nesvilab.fragpipe.util.MassOffsetUtils;
import org.nesvilab.fragpipe.util.SDRFtable;
import org.nesvilab.utils.MapUtils;
import org.nesvilab.utils.StringUtils;
import org.nesvilab.utils.SwingUtils;
import org.nesvilab.utils.swing.DocumentFilters;
import org.nesvilab.utils.swing.FileChooserUtils;
import org.nesvilab.utils.swing.FileNameEndingFilter;
import org.nesvilab.utils.swing.FormEntry;
import org.nesvilab.utils.swing.JPanelBase;
import org.nesvilab.utils.swing.MigUtils;
import org.nesvilab.utils.swing.UiCheck;
import org.nesvilab.utils.swing.UiCombo;
import org.nesvilab.utils.swing.UiSpinnerDouble;
import org.nesvilab.utils.swing.UiSpinnerInt;
import org.nesvilab.utils.swing.UiText;
import org.nesvilab.utils.swing.UiUtils;
import org.nesvilab.utils.swing.renderers.TableCellDoubleRenderer;
import org.nesvilab.utils.swing.renderers.TableCellIntRenderer;
import org.nesvilab.utils.swing.renderers.TableCellIntSpinnerEditor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class TabMsfragger extends JPanelBase {

  private static final Logger log = LoggerFactory.getLogger(TabMsfragger.class);
  private static final Pattern pattern = Pattern.compile("(.+): Scans .+ Instrument = ([^;\\n\\r]+)");
  private final static MigUtils mu = MigUtils.get();
  private AtomicBoolean hasBeenShown = new AtomicBoolean(false);
  public static final String PROP_FILECHOOSER_LAST_PATH = "msfragger.filechooser.path";
  public static final String CACHE_FORM = "msfragger-form" + ThisAppProps.TEMP_FILE_EXT;
  public static final String CACHE_PROPS = "msfragger-props" + ThisAppProps.TEMP_FILE_EXT;
  public static final LinkedHashMap<String, SearchTypeProp> SEARCH_TYPE_NAME_MAPPING;
  private static final String[] TABLE_VAR_MODS_COL_NAMES = {"Enabled", "Site (editable)",
      "Mass delta (editable)", "Max occurrences (editable)"};
  private static final String[] TABLE_FIX_MODS_COL_NAMES = {"Enabled", "Site",
      "Mass delta (editable)"};
  public static final String[] TABLE_OFFSET_COL_NAMES = {"Mass", "Sites", "Diagnostic ions", "Peptide remainders", "Fragment remainders"};
  private static final String PROP_misc_adjust_precurosr_mass = "misc.adjust-precursor-mass";
  private static final String PROP_misc_slice_db = "misc.slice-db";
  private static final String PROP_misc_ram = "misc.ram";
  private static final String PROP_misc_fragger_remove_precursor_range_lo = "misc.fragger.remove-precursor-range-lo";
  private static final String PROP_misc_fragger_remove_precursor_range_hi = "misc.fragger.remove-precursor-range-hi";
  private static final String PROP_misc_fragger_digest_mass_lo = "misc.fragger.digest-mass-lo";
  private static final String PROP_misc_fragger_digest_mass_hi = "misc.fragger.digest-mass-hi";
  private static final String PROP_misc_fragger_clear_mz_lo = "misc.fragger.clear-mz-lo";
  private static final String PROP_misc_fragger_clear_mz_hi = "misc.fragger.clear-mz-hi";
  private static final String PROP_misc_fragger_precursor_charge_lo = "misc.fragger.precursor-charge-lo";
  private static final String PROP_misc_fragger_precursor_charge_hi = "misc.fragger.precursor-charge-hi";
  public static final String PROP_misc_fragger_enzyme_dropdown_1 = "misc.fragger.enzyme-dropdown-1";
  public static final String PROP_misc_fragger_enzyme_dropdown_2 = "misc.fragger.enzyme-dropdown-2";
  public static final String TAB_PREFIX = "msfragger.";
  private static final Set<String> PROPS_MISC_NAMES;
  private static final Map<String, Function<String, String>> CONVERT_TO_FILE;
  private static final Map<String, Function<String, String>> CONVERT_TO_GUI;
  private static final String[] CALIBRATE_LABELS = {"None", "Mass calibration", "Mass calibration, parameter optimization"};
  private static final String[] MASS_DIFF_TO_VAR_MOD = {"No", "Yes, keep delta mass", "Yes, remove delta mass"};
  private static final String[] GROUP_VARIABLE = {"None", "Number of enzymatic termini", "Protein evidence from FASTA file"};
  private static final String[] DEISOTOPE = {"No", "Yes", "Yes, use charge 1 and 2 for undeisotoped peaks"};
  private static final int[] MASS_DIFF_TO_VAR_MOD_MAP = {0, 2, 1};
  private static final List<String> GLYCO_OPTIONS_UI = Arrays
      .asList(GLYCO_OPTION_off, GLYCO_OPTION_nglycan, GLYCO_OPTION_labile);
  private static final List<String> ACTIVATION_TYPES_UI = Arrays
          .asList(ACTIVATION_TYPE_ALL, ACTIVATION_TYPE_HCD, ACTIVATION_TYPE_ETD, ACTIVATION_TYPE_CID, ACTIVATION_TYPE_ECD);
  private static final List<String> ANALYZER_TYPES_UI = Arrays.asList(ANALYZER_TYPE_ALL, ANALYZER_TYPE_FTMS, ANALYZER_TYPE_ITMS, ANALYZER_TYPE_ASTMS);
  private static final HashMap<String, String> ACTIVATION_MAP;  // to handle input of "" or "all" for activation param
  private static final Map<String, String> ANALYZER_MAP = new HashMap<>();

  static {
    ACTIVATION_MAP = new HashMap<>();
    ACTIVATION_MAP.put("ALL", ACTIVATION_TYPE_ALL);
    ACTIVATION_MAP.put("", ACTIVATION_TYPE_ALL);
    ACTIVATION_MAP.put("HCD", ACTIVATION_TYPE_HCD);
    ACTIVATION_MAP.put("ETD", ACTIVATION_TYPE_ETD);
    ACTIVATION_MAP.put("CID", ACTIVATION_TYPE_CID);
    ACTIVATION_MAP.put("ECD", ACTIVATION_TYPE_ECD);

    ANALYZER_MAP.put("", ANALYZER_TYPE_ALL);
    ANALYZER_MAP.put("ALL", ANALYZER_TYPE_ALL);
    ANALYZER_MAP.put("FTMS", ANALYZER_TYPE_FTMS);
    ANALYZER_MAP.put("ITMS", ANALYZER_TYPE_ITMS);
    ANALYZER_MAP.put("ASTMS", ANALYZER_TYPE_ASTMS);
  }

  public static final List<MsfraggerEnzyme> ENZYMES = new EnzymeProvider().get();
  private static String[] PROPS_MISC = {
      PROP_misc_adjust_precurosr_mass,
      PROP_misc_slice_db,
      PROP_misc_ram,
      PROP_misc_fragger_digest_mass_lo,
      PROP_misc_fragger_digest_mass_hi,
      PROP_misc_fragger_remove_precursor_range_lo,
      PROP_misc_fragger_remove_precursor_range_hi,
      PROP_misc_fragger_clear_mz_lo,
      PROP_misc_fragger_clear_mz_hi,
      PROP_misc_fragger_precursor_charge_lo,
      PROP_misc_fragger_precursor_charge_hi,
  };

  UiCombo uiComboMassDiffToVariableMod;

  private static String itos(int i) {
    return Integer.toString(i);
  }

  static {
    PROPS_MISC_NAMES = new HashSet<>(Arrays.asList(PROPS_MISC));
    CONVERT_TO_FILE = new HashMap<>();
    CONVERT_TO_GUI = new HashMap<>();

    CONVERT_TO_FILE.put(MsfraggerParams.PROP_write_calibrated_mzml, s -> itos(Boolean.parseBoolean(s) ? 1 : 0));
    CONVERT_TO_FILE.put(MsfraggerParams.PROP_write_uncalibrated_mzml, s -> itos(1));
    CONVERT_TO_FILE.put(MsfraggerParams.PROP_write_mzbin_all, s -> itos(Boolean.parseBoolean(s) ? 1 : 0));
    CONVERT_TO_FILE.put(MsfraggerParams.PROP_mass_diff_to_variable_mod, s -> itos(
        MASS_DIFF_TO_VAR_MOD_MAP[ArrayUtils.indexOf(MASS_DIFF_TO_VAR_MOD, s)]));
    CONVERT_TO_FILE.put(MsfraggerParams.PROP_group_variable, s -> {
      for (int i = 0; i < GROUP_VARIABLE.length; ++i) {
        if (s.equalsIgnoreCase(GROUP_VARIABLE[i])) {
          return itos(i);
        }
      }
      return "0";
    });
    CONVERT_TO_FILE.put(MsfraggerParams.PROP_deisotope, s -> {
      for (int i = 0; i < DEISOTOPE.length; ++i) {
        if (s.equalsIgnoreCase(DEISOTOPE[i])) {
          return itos(i);
        }
      }
      return "1";
    });
    CONVERT_TO_FILE.put(MsfraggerParams.PROP_precursor_mass_units, s -> itos(PrecursorMassTolUnits.valueOf(s).valueInParamsFile()));
    CONVERT_TO_FILE.put(MsfraggerParams.PROP_fragment_mass_units, s -> itos(MassTolUnits.valueOf(s).valueInParamsFile()));
    CONVERT_TO_FILE.put(MsfraggerParams.PROP_precursor_true_units, s -> itos(
        MassTolUnits.valueOf(s).valueInParamsFile()));
    CONVERT_TO_FILE.put(MsfraggerParams.PROP_calibrate_mass, s -> {
      if (s.contentEquals(CALIBRATE_LABELS[0])) {
        return "0";
      } else if (s.contentEquals(CALIBRATE_LABELS[1])) {
        return "1";
      } else if (s.contentEquals(CALIBRATE_LABELS[2])) {
        return "2";
      } else {
        return "0";
      }
    });
    CONVERT_TO_FILE.put(MsfraggerParams.PROP_use_all_mods_in_first_search, s -> itos(Boolean.parseBoolean(s) ? 1 : 0));
    CONVERT_TO_FILE.put(MsfraggerParams.PROP_num_enzyme_termini, s -> itos(
        CleavageType.valueOf(s).valueInParamsFile()));
    CONVERT_TO_FILE.put(MsfraggerParams.PROP_remove_precursor_peak, s -> itos(
        RemovePrecursorPeak.get(s)));
    CONVERT_TO_FILE.put(MsfraggerParams.PROP_intensity_transform, s -> itos(
        IntensityTransform.get(s)));
    CONVERT_TO_FILE.put(MsfraggerParams.PROP_check_spectral_files, s -> itos(Boolean.parseBoolean(s) ? 1 : 0));
    CONVERT_TO_FILE.put(MsfraggerParams.PROP_reuse_dia_fragment_peaks, s -> itos(Boolean.parseBoolean(s) ? 1 : 0));
    CONVERT_TO_FILE.put(MsfraggerParams.PROP_require_precursor, s -> itos(Boolean.parseBoolean(s) ? 1 : 0));
    CONVERT_TO_FILE.put(MsfraggerParams.PROP_activation_filter, s -> ACTIVATION_TYPES.get(ACTIVATION_TYPES_UI.indexOf(ACTIVATION_MAP.get(s.toUpperCase()))));
    CONVERT_TO_FILE.put(MsfraggerParams.PROP_analyzer_types, s -> ANALYZER_TYPES.get(ANALYZER_TYPES_UI.indexOf(ANALYZER_MAP.get(s.toUpperCase()))));
    CONVERT_TO_FILE.put(MsfraggerParams.PROP_localize_delta_mass, s -> itos(Boolean.parseBoolean(s) ? 1 : 0));
    CONVERT_TO_FILE.put(MsfraggerParams.PROP_clip_nTerm_M, s -> itos(Boolean.parseBoolean(s) ? 1 : 0));
    CONVERT_TO_FILE.put(MsfraggerParams.PROP_override_charge, s -> itos(Boolean.parseBoolean(s) ? 1 : 0));
    CONVERT_TO_FILE.put(MsfraggerParams.PROP_output_format, s -> FraggerOutputType.valueOf(s).valueInParamsFile());
    CONVERT_TO_FILE.put(MsfraggerParams.PROP_report_alternative_proteins, s -> itos(Boolean.parseBoolean(s) ? 1 : 0));
    CONVERT_TO_FILE.put(MsfraggerParams.PROP_fragment_ion_series, ionStr -> ionStr.trim().replaceAll("[\\s,;]+",","));
    CONVERT_TO_FILE.put(MsfraggerParams.PROP_labile_fragment_ion_series, ionStr -> ionStr.trim().replaceAll("[\\s,;]+",","));

    CONVERT_TO_FILE.put(MsfraggerParams.PROP_labile_search_mode, s -> GLYCO_OPTIONS.get(GLYCO_OPTIONS_UI.indexOf(s)));
    CONVERT_TO_FILE.put(MsfraggerParams.PROP_mass_offsets, s -> {
      Set<String> tt = new TreeSet<>();
      if (s == null || s.trim().isEmpty()) {
        return "0";
      }
      for (String part : s.trim().split("[\\s/]+")) {
        tt.add(String.valueOf(Float.parseFloat(part))); // Parsing the values to float and converting back to String to make sure that MSFragger will get the value with the same string representation.
      }
      return String.join("/", tt);
    });
    CONVERT_TO_FILE.put(MsfraggerParams.PROP_mass_offsets_detailed, s -> {
      if (s == null || s.trim().isEmpty()) {
        return "";
      } else {
        return s;
      }
    });
    CONVERT_TO_FILE.put(MsfraggerParams.PROP_use_detailed_offsets, s -> itos(Boolean.parseBoolean(s) ? 1 : 0));
    CONVERT_TO_FILE.put(MsfraggerParams.PROP_Y_type_masses, s -> s.replaceAll("[\\s]+", "/"));
    CONVERT_TO_FILE.put(MsfraggerParams.PROP_diagnostic_fragments, s -> s.replaceAll("[\\s]+", "/"));
    CONVERT_TO_FILE.put(MsfraggerParams.PROP_remainder_masses, s -> s.replaceAll("[\\s]+", "/"));
    CONVERT_TO_FILE.put(MsfraggerParams.PROP_deneutralloss, s -> s.toLowerCase().contentEquals("yes") ? "1" : "0");

    CONVERT_TO_GUI.put(MsfraggerParams.PROP_write_calibrated_mzml, s -> Boolean.toString(Integer.parseInt(s) > 0));
    CONVERT_TO_GUI.put(MsfraggerParams.PROP_write_uncalibrated_mzml, s -> Boolean.toString(true));
    CONVERT_TO_GUI.put(MsfraggerParams.PROP_write_mzbin_all, s -> Boolean.toString(Integer.parseInt(s) > 0));
    CONVERT_TO_GUI.put(MsfraggerParams.PROP_mass_diff_to_variable_mod, s-> MASS_DIFF_TO_VAR_MOD[MASS_DIFF_TO_VAR_MOD_MAP[Integer.parseInt(s)]]);
    CONVERT_TO_GUI.put(MsfraggerParams.PROP_group_variable, s-> GROUP_VARIABLE[Integer.parseInt(s)]);
    CONVERT_TO_GUI.put(MsfraggerParams.PROP_deisotope, s-> DEISOTOPE[Integer.parseInt(s)]);
    CONVERT_TO_GUI.put(MsfraggerParams.PROP_precursor_mass_units, s -> PrecursorMassTolUnits.fromParamsFileRepresentation(s).name());
    CONVERT_TO_GUI.put(MsfraggerParams.PROP_fragment_mass_units, s -> MassTolUnits.fromFileToUi(s).name());
    CONVERT_TO_GUI.put(MsfraggerParams.PROP_precursor_true_units, s -> MassTolUnits.fromFileToUi(s).name());
    CONVERT_TO_GUI.put(MsfraggerParams.PROP_calibrate_mass, s -> {
      if (s.contentEquals("0")) {
        return CALIBRATE_LABELS[0];
      } else if (s.contentEquals("1")) {
        return CALIBRATE_LABELS[1];
      } else if (s.contentEquals("2")) {
        return CALIBRATE_LABELS[2];
      } else {
        return CALIBRATE_LABELS[0];
      }
    });
    CONVERT_TO_GUI.put(MsfraggerParams.PROP_use_all_mods_in_first_search, s -> Boolean.toString(Integer.parseInt(s) == 1));
    CONVERT_TO_GUI.put(MsfraggerParams.PROP_num_enzyme_termini, s -> CleavageType.fromValueInParamsFile(s).name());
    CONVERT_TO_GUI.put(MsfraggerParams.PROP_remove_precursor_peak, s -> RemovePrecursorPeak.get(Integer.parseInt(s)));
    CONVERT_TO_GUI.put(MsfraggerParams.PROP_intensity_transform, s -> IntensityTransform.get(Integer.parseInt(s)));
    CONVERT_TO_GUI.put(MsfraggerParams.PROP_check_spectral_files, s -> Boolean.toString(Integer.parseInt(s) > 0));
    CONVERT_TO_GUI.put(MsfraggerParams.PROP_reuse_dia_fragment_peaks, s -> Boolean.toString(Integer.parseInt(s) > 0));
    CONVERT_TO_GUI.put(MsfraggerParams.PROP_require_precursor, s -> Boolean.toString(Integer.parseInt(s) > 0));
    CONVERT_TO_GUI.put(MsfraggerParams.PROP_activation_filter, s -> ACTIVATION_TYPES_UI.get(ACTIVATION_TYPES.indexOf(ACTIVATION_MAP.get(s.toUpperCase()))));
    CONVERT_TO_GUI.put(MsfraggerParams.PROP_analyzer_types, s -> ANALYZER_TYPES_UI.get(ANALYZER_TYPES.indexOf(ANALYZER_MAP.get(s.toUpperCase()))));
    CONVERT_TO_GUI.put(MsfraggerParams.PROP_localize_delta_mass, s -> Boolean.toString(Integer.parseInt(s) > 0));
    CONVERT_TO_GUI.put(MsfraggerParams.PROP_clip_nTerm_M, s -> Boolean.toString(Integer.parseInt(s) > 0));
    CONVERT_TO_GUI.put(MsfraggerParams.PROP_override_charge, s -> Boolean.toString(Integer.parseInt(s) > 0));
    CONVERT_TO_GUI.put(MsfraggerParams.PROP_output_format, s -> FraggerOutputType.fromValueInParamsFile(s).name());
    CONVERT_TO_GUI.put(MsfraggerParams.PROP_report_alternative_proteins, s -> Boolean.toString(Integer.parseInt(s) > 0));

    CONVERT_TO_GUI.put(MsfraggerParams.PROP_labile_search_mode, s -> GLYCO_OPTIONS_UI.get(GLYCO_OPTIONS.indexOf(s)));
    CONVERT_TO_GUI.put(MsfraggerParams.PROP_mass_offsets, text -> {
      if (text == null || text.trim().isEmpty()) {
        return "0";
      }
      return String.join(" ", text.trim().split("/"));
    });
    CONVERT_TO_GUI.put(MsfraggerParams.PROP_mass_offsets_detailed, text -> {
      if (text == null || text.trim().isEmpty()) {
        return "";
      } else {
        return text;
      }
    });
    CONVERT_TO_GUI.put(MsfraggerParams.PROP_use_detailed_offsets, s -> Boolean.toString(Integer.parseInt(s) > 0));
    CONVERT_TO_GUI.put(MsfraggerParams.PROP_Y_type_masses, text -> String.join(" ", text.split("/")));
    CONVERT_TO_GUI.put(MsfraggerParams.PROP_diagnostic_fragments, text -> String.join(" ", text.split("/")));
    CONVERT_TO_GUI.put(MsfraggerParams.PROP_remainder_masses, text -> String.join(" ", text.split("/")));
    CONVERT_TO_GUI.put(MsfraggerParams.PROP_deneutralloss, s -> Integer.parseInt(s) == 1 ? "Yes" : "No");

    SEARCH_TYPE_NAME_MAPPING = new LinkedHashMap<>();
    SEARCH_TYPE_NAME_MAPPING.put("Closed search default config", SearchTypeProp.closed);
    SEARCH_TYPE_NAME_MAPPING.put("Open search default config", SearchTypeProp.open);
    SEARCH_TYPE_NAME_MAPPING.put("Non-specific search default config", SearchTypeProp.nonspecific);
    SEARCH_TYPE_NAME_MAPPING.put("Mass offset search default config", SearchTypeProp.offset);
  }

  private UiCheck checkRun;
  private UiCheck checkDigestOnly;
  private JPanel pContent;
  private ModsTable tableVarMods;
  private ModsTable tableFixMods;
  public UiCombo uiComboMassCalibrate;
  public UiCombo uiComboOutputType;
  private UiCombo uiComboMassMode;
  private UiCombo uiComboFragTolUnits;
  private UiSpinnerInt uiSpinnerDbsplit;
  private UiCheck uiCheckLocalizeDeltaMass;
  private UiText uiTextLabileIonSeries;
  private UiText uiTextCustomIonSeries;
  private JLabel labelCustomIonSeries;
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

  private UiText uiTextMassOffsets;
  private UiText uiTextRemainderMasses;
  public UiCheck uiCheckMassOffsetFile;
  public UiCombo uiComboGlyco;
  private UiSpinnerDouble uiSpinnerPrecTolLo;
  private UiSpinnerDouble uiSpinnerPrecTolHi;
  private UiSpinnerDouble uiSpinnerFragTol;
  private UiCombo uiComboPrecursorTolUnits;
  private final Map<String, String> cache = new HashMap<>();
  private UiText uiTextIsoErr;
  private UiText epMassOffsets;
  private UiText epDetailedMassOffsets;
  private UiText uiTextRestrictDeltamassTo;
  private JPanel pOffsetRegular;
  private JPanel pOffsetDetailed;
  private JPanel pTop;
  private JPanel pBasic;
  private JPanel pMods;
  private JPanel pAdvanced;
  private UiCheck uiCheckWriteCalibratedMzml;
  private UiCombo uiComboGroupVariable;
  private UiSpinnerInt uiSpinnerOutputReportTopNDda;
  private UiSpinnerInt uiSpinnerOutputReportTopNDia1;
  private UiSpinnerInt uiSpinnerOutputReportTopNDdaPlus;

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

  public UiText getUiTextIsoErr() {
    return uiTextIsoErr;
  }

  public boolean isMassOffsetSearch() {
    if (SwingUtils.isEnabledAndChecked(uiCheckMassOffsetFile)) {
      return true;
    }

    Object selected = uiComboPrecursorTolUnits.getSelectedItem();
    if (selected == null || StringUtils.isNullOrWhitespace((String) selected)) {
      return false;
    }

    String[] massOffsetStrings = (epMassOffsets).getNonGhostText().trim().split("[/\\s]+");
    return massOffsetStrings.length > 1 || Math.abs(Double.parseDouble(massOffsetStrings[0])) > 0.01;
  }

  public boolean isOpenSearch() {
    Object selected = uiComboPrecursorTolUnits.getSelectedItem();
    if (selected == null || StringUtils.isNullOrWhitespace((String) selected)) {
      return false;
    }

    return PrecursorMassTolUnits.valueOf((String) selected).valueInParamsFile() == 0 && uiSpinnerPrecTolLo.getActualValue() < -3 && uiSpinnerPrecTolHi.getActualValue() > 3;
  }

  public int getMassDiffToVariableMod() {
    return MASS_DIFF_TO_VAR_MOD_MAP[uiComboMassDiffToVariableMod.getSelectedIndex()];
  }

  public boolean isLocalizeDeltaMass() {
    return SwingUtils.isEnabledAndChecked(uiCheckLocalizeDeltaMass);
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
    updateEnabledStatus(this, false); // will get enabled once we receive NoteConfigMsfragger
    updateEnabledStatus(uiSpinnerDbsplit, false); // only gets enabled when DbSlice2 is initialized

    updateEnabledStatus(uiTextCustomIonSeries, true);
    updateEnabledStatus(labelCustomIonSeries, true);

    super.initMore();

    // init fields with default values, called after initMore() which causes field renaming to happen
    log.debug("Calling TabMsfragger loadDefaults(SearchTypeProp.closed, false) in initMore()");
    loadDefaults(SearchTypeProp.closed, false);

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

    // enable/disable the mass offset file UI
    SwingUtils.setDisablementUpdater(this, pOffsetRegular, uiCheckMassOffsetFile);
    SwingUtils.setEnablementUpdater(this, pOffsetDetailed, uiCheckMassOffsetFile);
  }

  @Override
  protected void init() {
    this.setLayout(new MigLayout(new LC().fillX()));

    pTop = createPanelTop();
    pContent = createPanelContent();
    pBasic = createPanelBasicOptions();
    pMods = createPanelMods();
    pAdvanced = createPanelAdvancedOptions();

    mu.add(this, pTop).growX().wrap();
    mu.add(this, pContent).growX().wrap();
    mu.add(pContent, pBasic).growX().wrap();
    mu.add(pContent, pMods).growX().wrap();
    mu.add(pContent, pAdvanced).growX().wrap();
  }

  /** Top panel with checkbox, buttons and RAM+Threads spinners. */
  private JPanel createPanelTop() {
    JPanel pTop = new JPanel(new MigLayout(new LC().fillX()));

    checkRun = new UiCheck("Run MSFragger", null, true);
    checkRun.setName("run-msfragger");
    checkRun.addActionListener(e -> {
      final boolean isSelected = checkRun.isSelected();
      updateEnabledStatus(pContent, isSelected);
      updateEnabledStatus(checkDigestOnly, isSelected);
    });

    checkDigestOnly = new UiCheck("Digestion only", null, false);
    checkDigestOnly.setName("digest-only");
    checkDigestOnly.setToolTipText("Digest the proteins into peptides without running database search. Mostly for spectral library prediction.");

    JLabel imageLabel = new JLabel();
    try {
      BufferedImage image = ImageIO.read(Objects.requireNonNull(getClass().getResource("/org/nesvilab/fragpipe/icons/msfragger-128.png")));
      imageLabel = new JLabel(new ImageIcon(image));
    } catch (Exception ex) {
      ex.printStackTrace();
    }

    mu.add(pTop, checkRun);
    mu.add(pTop, imageLabel, mu.ccR()).gapRight("50").wrap();
    mu.add(pTop, checkDigestOnly).wrap();

    return pTop;
  }

  private JPanel createPanelContent() {
    pContent = new JPanel();
    pContent.setLayout(new MigLayout(new LC().fillX()));
    return pContent;
  }

  private JPanel createPanelBasicPeakMatch() {
    JPanel p = mu.newPanel("Peak matching", true);

    // precursor mass tolerance
    List<String> units = Seq.of(PrecursorMassTolUnits.values()).map(PrecursorMassTolUnits::name).toList();
    uiComboPrecursorTolUnits = UiUtils.createUiCombo(units);
    FormEntry fePrecTolUnits = mu.feb(MsfraggerParams.PROP_precursor_mass_units, uiComboPrecursorTolUnits).label("Precursor mass tolerance").create();
    uiSpinnerPrecTolLo = new UiSpinnerDouble(-20, -10000, 10000, 1,
        new DecimalFormat("0.###"));
    uiSpinnerPrecTolLo.setColumns(4);
    FormEntry fePrecTolLo = mu.feb(MsfraggerParams.PROP_precursor_mass_lower, uiSpinnerPrecTolLo).create();
    uiSpinnerPrecTolHi = new UiSpinnerDouble(+20, -10000, 10000, 1,
        new DecimalFormat("0.###"));
    uiSpinnerPrecTolHi.setColumns(4);
    FormEntry fePrecTolHi = mu.feb(MsfraggerParams.PROP_precursor_mass_upper, uiSpinnerPrecTolHi).create();

    uiComboPrecursorTolUnits.addItemListener(e -> {
      Object selected = uiComboPrecursorTolUnits.getSelectedItem();
      if (selected == null || StringUtils.isNullOrWhitespace((String)selected))
        return;
      final boolean isDisabled = PrecursorMassTolUnits.valueOf((String)selected).valueInParamsFile() > 1;
      uiSpinnerPrecTolLo.setEnabled(!isDisabled);
      uiSpinnerPrecTolHi.setEnabled(!isDisabled);

      // treat calibrate masses dropdown
      boolean wasEnabled = uiComboMassCalibrate.isEnabled();
      if (wasEnabled && isDisabled) { //  switching from enabled to disabled
        String oldVal = (String)uiComboMassCalibrate.getSelectedItem();
        if (oldVal != null) {
          cache.put(MsfraggerParams.PROP_calibrate_mass, oldVal);
        }
        uiComboMassCalibrate.setSelectedItem("None");
        uiComboMassCalibrate.setEnabled(false);
      } else if (!wasEnabled && !isDisabled) { // switching from disabled to enabled
        String cachedVal = cache.get(MsfraggerParams.PROP_calibrate_mass);
        if (cachedVal != null) {
          uiComboMassCalibrate.setSelectedItem(cachedVal);
        }
        uiComboMassCalibrate.setEnabled(true);
      }
    });

    // fragment mass tolerance
    uiComboFragTolUnits = UiUtils.createUiCombo(MassTolUnits.values());
    FormEntry feFragTolUnits = mu.feb(MsfraggerParams.PROP_fragment_mass_units, uiComboFragTolUnits)
        .label("Fragment mass tolerance").create();
    uiSpinnerFragTol = new UiSpinnerDouble(10, 0, 10000, 1,
        new DecimalFormat("0.###"));
    uiSpinnerFragTol.setColumns(4);
    FormEntry feFragTol = mu.feb(MsfraggerParams.PROP_fragment_mass_tolerance, uiSpinnerFragTol).create();


    // mass calibrate
    uiComboMassCalibrate = UiUtils.createUiCombo(CALIBRATE_LABELS);
    FormEntry feCalibrate = mu.feb(MsfraggerParams.PROP_calibrate_mass, uiComboMassCalibrate)
        .label("<html>Calibration and optimization").create();

    uiTextIsoErr = UiUtils.uiTextBuilder().cols(10).filter("[^\\d/-]+").text("-1/0/1/2").create();
    FormEntry feIsotopeError = mu.feb(MsfraggerParams.PROP_isotope_error, uiTextIsoErr)
        .label("Isotope error")
        .tooltip("<html>String of the form 0/1/2/3 indicating which isotopic\n"
            + "peak selection errors MSFragger will try to correct.")
        .create();

    mu.add(p, fePrecTolUnits.label(), mu.ccR());
    mu.add(p, fePrecTolUnits.comp).split(4);
    mu.add(p, fePrecTolLo.comp);
    mu.add(p, new JLabel("-"));
    mu.add(p, fePrecTolHi.comp);
    mu.add(p, feFragTolUnits.label(), mu.ccR()).gapLeft("10px");
    mu.add(p, feFragTolUnits.comp).split(2);
    mu.add(p, feFragTol.comp).pushX().wrap();

    mu.add(p, feCalibrate.label(), mu.ccR());
    mu.add(p, feCalibrate.comp);
    mu.add(p, feIsotopeError.label(), mu.ccR());
    mu.add(p, feIsotopeError.comp).spanX().wrap();

    return p;
  }

  private JPanel createPanelDigest() {
    // Digest panel
    JPanel p = mu.newPanel("Protein digestion", true);

    uiComboEnzymes = UiUtils.createUiCombo(ENZYMES.stream().map(msfe -> msfe.name).collect(Collectors.toList()));
    Optional<MsfraggerEnzyme> trypsin = ENZYMES.stream().filter(e -> e.name.toLowerCase().startsWith("trypsin")).min(Comparator.comparing(o -> o.name));
    trypsin.ifPresent(msfraggerEnzyme -> uiComboEnzymes.setSelectedItem(msfraggerEnzyme.name));
    FormEntry feEnzymeList = mu.feb(PROP_misc_fragger_enzyme_dropdown_1, uiComboEnzymes)
        .label("Load rules")
        .tooltip("<html>Load one of default definitions of enzyme cleavage rules.\n"
            + "You can still edit the name and rules manually after loading.\n"
            + "<b>Note about custom enzymes:</b> To use an enzyme not listed in\n"
            + "the digestion rules drop-down menu, enter the custom cleavage rules\n"
            + "but be sure to set the 'Enzyme name' field to 'nonspecific'.\n"
            + "Select 'ENZYMATIC' from the 'Cleavage' drop-down menu.").create();
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
    FormEntry feEnzymeList2 = mu.feb(PROP_misc_fragger_enzyme_dropdown_2, uiComboEnzymes2)
        .label("Load rules")
        .tooltip("<html>Load one of default definitions of enzyme cleavage rules.\n"
            + "You can still edit the name and rules manually after loading.\n"
            + "<b>Note about custom enzymes:</b> To use an enzyme not listed in\n"
            + "the digestion rules drop-down menu, enter the custom cleavage rules\n"
            + "but be sure to set the 'Enzyme name' field to 'nonspecific'.\n"
            + "Select 'ENZYMATIC' from the 'Cleavage' drop-down menu.").create();
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
        if (StringUtils.isBlank(uiTextCuts.getNonGhostText())) {
          uiTextCuts.setText("@");
        }
        final String cuts = StringUtils.sortedChars(uiTextCuts.getNonGhostText());
        final String nocuts = StringUtils.sortedChars(uiTextNocuts.getNonGhostText());
        final String sense = StringUtils.sortedChars(uiComboSense.asString());
        List<MsfraggerEnzyme> enzymes = ENZYMES.stream()
            .map(e -> new MsfraggerEnzyme(e.name, StringUtils.sortedChars(e.cut),
                StringUtils.sortedChars(e.nocuts), e.sense))
            .collect(Collectors.toList());
        List<String> matching = enzymes.stream()
            .filter(e -> e.cut.equals(cuts) && e.nocuts.equals(nocuts) && e.sense.equals(sense))
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
    // listener for second enzyme properties
    FocusAdapter enzymeSpecFocusListener2 = new FocusAdapter() {
      @Override
      public void focusLost(FocusEvent evt) {
        super.focusLost(evt);
        final String cuts = StringUtils.sortedChars(uiTextCuts2.getNonGhostText());
        final String nocuts = StringUtils.sortedChars(uiTextNocuts2.getNonGhostText());
        final String sense = StringUtils.sortedChars(uiComboSense2.asString());
        List<MsfraggerEnzyme> enzymes = ENZYMES.stream()
                .map(e -> new MsfraggerEnzyme(e.name, StringUtils.sortedChars(e.cut),
                        StringUtils.sortedChars(e.nocuts), e.sense))
                .collect(Collectors.toList());
        List<String> matching = enzymes.stream()
                .filter(e -> e.cut.equals(cuts) && e.nocuts.equals(nocuts) && e.sense.equals(sense))
                .map(e -> e.name).collect(Collectors.toList());
        log.warn("Found matching enzymes: {}", matching);
        if (matching.contains("nonspecific")) {
          trySelectEnzymeDropdown2("nonspecific");
        } else if (!matching.isEmpty()) {
          trySelectEnzymeDropdown2(matching.get(0));
        } else {
          trySelectEnzymeDropdown2("custom");
        }
      }
    };

    uiTextEnzymeName = new UiText();
    FormEntry feEnzymeName = mu.feb(MsfraggerParams.PROP_search_enzyme_name_1, uiTextEnzymeName).label("Enzyme name 1").create();
    uiTextCuts = UiUtils.uiTextBuilder().cols(6).filter("[^A-Z@]", "@").text("KR").create();
    uiTextCuts.addFocusListener(enzymeSpecFocusListener);
    FormEntry feCuts = mu.feb(MsfraggerParams.PROP_search_enzyme_cut_1, uiTextCuts).label("Cuts 1")
        .tooltip("Enter capital letters for amino acids where the enzyme cuts, or ‘@’ if none.").create();
    uiTextNocuts = UiUtils.uiTextBuilder().cols(3).filter("[^A-Z@]", "@").text("P").create();
    uiTextNocuts.addFocusListener(enzymeSpecFocusListener);
    FormEntry feNocuts = mu.feb(MsfraggerParams.PROP_search_enzyme_nocut_1, uiTextNocuts).label("No cuts 1")
        .tooltip("Amino acids before which the enzyme won’t cut, or ‘@’ if none.").create();

    List<String> cleavageTypeNames = Arrays.stream(CleavageType.values()).map(Enum::name)
        .collect(Collectors.toList());
    uiComboCleavage = UiUtils.createUiCombo(cleavageTypeNames);
    FormEntry feCleavageType = mu.feb(MsfraggerParams.PROP_num_enzyme_termini, uiComboCleavage).label("Cleavage").create();
    UiSpinnerInt uiSpinnerMissedCleavages = new UiSpinnerInt(2, 0, 1000, 1);
    uiSpinnerMissedCleavages.setColumns(2);
    FormEntry feMissedCleavages = mu.feb(MsfraggerParams.PROP_allowed_missed_cleavage_1, uiSpinnerMissedCleavages).label("Missed cleavages 1").create();
    uiComboSense = UiUtils.createUiCombo(new String[]{"N","C"});
    FormEntry feSense = mu.feb(MsfraggerParams.PROP_search_enzyme_sense_1, uiComboSense).label("Sense 1").create();

    uiTextEnzymeName2 = new UiText();
    FormEntry feEnzymeName2 = mu.feb(MsfraggerParams.PROP_search_enzyme_name_2, uiTextEnzymeName2).label("Enzyme name 2").create();
    uiTextCuts2 = UiUtils.uiTextBuilder().cols(6).filter("[^A-Z@]", "@").text("KR").create();
    uiTextCuts2.addFocusListener(enzymeSpecFocusListener2);
    FormEntry feCuts2 = mu.feb(MsfraggerParams.PROP_search_enzyme_cut_2, uiTextCuts2).label("Cuts 2")
        .tooltip("Enter capital letters for amino acids where the enzyme cuts, or ‘@’ if none. Leave empty to not use this enzyme.").create();
    uiTextNocuts2 = UiUtils.uiTextBuilder().cols(3).filter("[^A-Z@]", "@").text("P").create();
    uiTextNocuts2.addFocusListener(enzymeSpecFocusListener2);
    FormEntry feNocuts2 = mu.feb(MsfraggerParams.PROP_search_enzyme_nocut_2, uiTextNocuts2).label("No cuts 2")
        .tooltip("Amino acids before which the enzyme won’t cut, or ‘@’ if none. Leave empty to not use this enzyme.").create();


    UiSpinnerInt uiSpinnerMissedCleavages2 = new UiSpinnerInt(2, 0, 1000, 1);
    uiSpinnerMissedCleavages2.setColumns(2);
    FormEntry feMissedCleavages2 = mu.feb(MsfraggerParams.PROP_allowed_missed_cleavage_2, uiSpinnerMissedCleavages2).label("Missed cleavages 2").create();
    uiComboSense2 = UiUtils.createUiCombo(new String[]{"N","C"});
    FormEntry feSense2 = mu.feb(MsfraggerParams.PROP_search_enzyme_sense_2, uiComboSense2).label("Sense 2").create();

    FormEntry feClipM = mu.feb(MsfraggerParams.PROP_clip_nTerm_M, new UiCheck("Clip N-term M", null))
        .tooltip("Trim protein N-terminal Methionine as a variable modification").create();



    FormEntry fePepLenMin = mu.feb(MsfraggerParams.PROP_digest_min_length, new UiSpinnerInt(7, 0, 1000, 1, 3))
        .label("Peptide length").create();
    FormEntry fePepLenMax = mu.feb(MsfraggerParams.PROP_digest_max_length, new UiSpinnerInt(50, 0, 1000, 1, 3))
        .create();
    UiSpinnerDouble uiSpinnerDigestMassLo = new UiSpinnerDouble(200, 0, 50000, 100,
        new DecimalFormat("0.#"));
    uiSpinnerDigestMassLo.setColumns(6);
    FormEntry fePepMassLo = mu.feb(PROP_misc_fragger_digest_mass_lo, uiSpinnerDigestMassLo).label("Peptide mass range (Da)").create();
    UiSpinnerDouble uiSpinnerDigestMassHi = new UiSpinnerDouble(5000, 0, 50000, 100,
        new DecimalFormat("0.#"));
    uiSpinnerDigestMassHi.setColumns(6);
    FormEntry fePepMassHi = mu.feb(PROP_misc_fragger_digest_mass_hi, uiSpinnerDigestMassHi).create();

    uiSpinnerDbsplit = new UiSpinnerInt(1, 1, 9999, 1, 2);
    FormEntry feDbsplit = mu.feb(PROP_misc_slice_db, uiSpinnerDbsplit).label("<html>Split database")
        .tooltip("<html>Split database into smaller chunks.\n"
            + "Only use for very large databases (200MB+) or<br/>non-specific digestion.").create();


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
    mu.add(p2, feMissedCleavages2.label(), mu.ccL());
    mu.add(p2, feMissedCleavages2.comp).split(2);
    mu.add(p2, feSense2.label(), mu.ccL());
    mu.add(p2, feSense2.comp).split(2).spanX().wrap();
// END enzyme 2
    mu.add(p,p2).wrap();

    mu.add(p, fePepLenMin.label(), mu.ccL()).split(12);
    mu.add(p, fePepLenMin.comp).split(3);
    mu.add(p, new JLabel("-"));
    mu.add(p, fePepLenMax.comp);
    mu.add(p, fePepMassLo.label(), mu.ccR());
    mu.add(p, fePepMassLo.comp).split(3);
    mu.add(p, new JLabel("-"));
    mu.add(p, fePepMassHi.comp);

    mu.add(p, feDbsplit.label()).gapLeft("10px").split(2);
    mu.add(p, feDbsplit.comp).pushX().wrap();

    return p;
  }

  /** Panel with all the basic options. */
  private JPanel createPanelBasicOptions() {
      JPanel pBase = mu.newPanel("Common options (advanced options are at the end of the page)", true);
      mu.add(pBase, createPanelBasicPeakMatch()).growX().wrap();
      mu.add(pBase, createPanelDigest()).growX().wrap();

      return pBase;
  }

  private JPanel createPanelGlyco() {
    JPanel p = mu.newPanel("Glyco/Labile mods", mu.lcFillXNoInsetsTopBottom());

    uiComboGlyco = UiUtils.createUiCombo(GLYCO_OPTIONS_UI);
    FormEntry feGlycoSearchMode = mu.feb(uiComboGlyco)
        .name(MsfraggerParams.PROP_labile_search_mode)
        .label("Labile modification search mode")
            .tooltip("labile mode assumes delta mass will dissociate from peptide\n" +
                    "during MS2. Uses restrict delta mass parameter to determine allowed sites.\n" +
                    "Allows diagnostic and Y ions\n" +
                    "nglycan mode overrides allowed sites to be the N-X-S/T sequon and\n" +
                    "enables all other labile mode settings.")
            .create();

    final UiSpinnerDouble uiSpinnerMinInt = UiSpinnerDouble.builder(0, 0, 1, 0.1).setFormat("#.##")
        .setCols(5).create();
    FormEntry feOxoniumIonMinimumIntensity = mu.feb(uiSpinnerMinInt)
        .name(MsfraggerParams.PROP_diagnostic_intensity_filter)
        .label("Diagnostic ion minimum intensity")
            .tooltip("Minimum diagnostic ion intensity to search for mass offsets/open search.\n" +
                    "Summed intensity of all diagnostic fragment masses (below) from a spectrum.\n" +
                    "Set to 0 to disable (all spectra will be searched for mass offsets/open search)")
            .create();

    final UiSpinnerInt uiSpinnerMinSeqMatches = new UiSpinnerInt(2, 0, 1000, 1, 4);
    FormEntry feMinSeqMatches = mu.feb(MsfraggerParams.PROP_min_sequence_matches, uiSpinnerMinSeqMatches)
            .label("Min sequence-specific ions")
            .tooltip("<html>The minimum number of peptide sequence-specific ions (i.e., not Y ions)\n" +
                    "required to record a match. Only used if peptide remainder ions are searched. Default: 2").create();

    UiText ep1 = new UiText();
    FormEntry feYIonMasses = mu.feb(ep1).name(MsfraggerParams.PROP_Y_type_masses)
        .label("Peptide remainder masses")
            .tooltip("List of possible peptide remainder ion (intact peptide + partially fragmented modification) masses\n" +
                    "Provide as neutral masses. Should include 0 in most cases. Space or / separated\n" +
                    "example: '0 203.0794 365.1322'")
            .create();

    UiText ep2 = new UiText();
    FormEntry feOxoniumIons = mu.feb(ep2).name(MsfraggerParams.PROP_diagnostic_fragments)
        .label("Diagnostic ion m/zs")
            .tooltip("List of possible diagnostic fragment ions to consider. NOTE: provide as [M+H]+, NOT neutral masses.\n" +
                    "Not used if Diagnostic Ion Minimum Intensity is 0\n" +
                    "Space or / separated")
            .create();

    uiTextRemainderMasses = new UiText();
    FormEntry feRemainderIons = mu.feb(MsfraggerParams.PROP_remainder_masses, uiTextRemainderMasses)
            .label("Fragment remainder masses")
            .tooltip("List of possible remainder fragment ions to consider. Provide as neutral masses.\n" +
                    "Remainder masses are partial modification masses left on b/y ions\n" +
                    "after fragmentation.\n ")
            .create();

      uiTextLabileIonSeries = new UiText(10);
      uiTextLabileIonSeries.setText("b,y");
      FormEntry feLabileIonSeries = mu.feb(MsfraggerParams.PROP_labile_fragment_ion_series, uiTextLabileIonSeries)
              .label("Labile fragment ion series").tooltip(
                      "Which peptide ion series to search for labile ions. REPLACES intact mods with labile remainder(s) as specified.\n"
                              + "<b>Use spaces, commas or semicolons as delimiters</b>, e.g. \"b,y\"\n"
                              + "This mostly depends on fragmentation method.\n"
                              + "Typically \"b,y\" are used for CID and none for for ETD/ECD.\n"
                              + "c,z can be added for very high supplemental activation in EThcD or similar.\n"
                              + "others can be added for other fragmentation types (e.g., UVPD, EID, etc.).\n"
                              + "MSFragger supports \"a,b,c,x,y,z\".").create();


      uiComboGlyco.addItemListener(e -> {
      // needs to be done after components to be turned on/off have been created
      final String selected = (String)uiComboGlyco.getSelectedItem();
      final boolean enabled = !MsfraggerParams.GLYCO_OPTION_off.equalsIgnoreCase(selected);
      updateEnabledStatus(uiSpinnerMinInt, enabled);
      updateEnabledStatus(ep1, enabled);
      updateEnabledStatus(ep2, enabled);
      updateEnabledStatus(uiTextRemainderMasses, enabled);
      updateEnabledStatus(uiSpinnerMinSeqMatches, enabled);
      updateEnabledStatus(uiTextLabileIonSeries, enabled);
    });
    // trigger the item listener on startup
    // (done with indexes so that it breaks if the list and OFF option are changed)
    int indexGlycoOff = MsfraggerParams.GLYCO_OPTIONS.indexOf(MsfraggerParams.GLYCO_OPTION_off);
    uiComboGlyco.setSelectedItem(null);
    uiComboGlyco.setSelectedItem(MsfraggerParams.GLYCO_OPTIONS.get(indexGlycoOff));


    mu.add(p, feGlycoSearchMode.label(), mu.ccR());
    mu.add(p, feGlycoSearchMode.comp);
    mu.add(p, feOxoniumIonMinimumIntensity.label(), mu.ccR());
    mu.add(p, feOxoniumIonMinimumIntensity.comp);
    mu.add(p, feMinSeqMatches.label(), mu.ccR());
    mu.add(p, feMinSeqMatches.comp);
    mu.add(p, feLabileIonSeries.label(), mu.ccR());
    mu.add(p, feLabileIonSeries.comp).wrap();
    mu.add(p, feOxoniumIons.label(), mu.ccR());
    mu.add(p, feOxoniumIons.comp).spanX().growX().wrap();
    mu.add(p, feYIonMasses.label(), mu.ccR());
    mu.add(p, feYIonMasses.comp).spanX().growX().wrap();
    mu.add(p, feRemainderIons.label(), mu.ccR());
    mu.add(p, feRemainderIons.comp).spanX().growX().wrap();

    return p;
  }

  public static Object[][] convertOffsetsToTableData(List<MassOffsetUtils.MassOffset> offsets) {
    Object[][] data = new Object[offsets.size()][TABLE_OFFSET_COL_NAMES.length];
    for (int i = 0; i < offsets.size(); i++) {
      MassOffsetUtils.MassOffset m = offsets.get(i);
      data[i][0] = m.mass;
      data[i][1] = m.getSiteStr();
      data[i][2] = floatArrToString(m.diagnosticIons);
      data[i][3] = floatArrToString(m.peptideRemainderIons);
      data[i][4] = floatArrToString(m.fragmentRemainderIons);
    }
    return data;
  }

  private static Object[][] convertModsToVarModsData(List<Mod> mods) {
    Object[][] data = new Object[MsfraggerParams.VAR_MOD_COUNT_MAX][TABLE_VAR_MODS_COL_NAMES.length];
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
        new Class<?>[]{Boolean.class, String.class, Float.class, Integer.class},
        new boolean[]{true, true, true, true},
        new int[]{0, 1, 2, 3},
        data);
    final ModsTable t = new ModsTable(m, TABLE_VAR_MODS_COL_NAMES, TabMsfragger::convertModsToVarModsData);
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
            "<li>79.96633 STY 1(for phosphorylation)</li>\n" +
            "<li>-17.0265 nQnC 1(for pyro-Glu or loss of ammonia at peptide N-terminal)</li>\n" +
            "<li>42.0106 [^ 1 (acetylation at any protein N-terminal)</li>\n" +
            "<li>229.16293 n^ 1 (TMT10/11 at any peptide N-terminal)</li>\n" +
            "<li>100 cK 1 (100 Da modification at peptide C-terminal K)</li>\n" +
            "<li>50 ]K 1 (50 Da modification at protein C-terminal K)</li>\n" +
            "<li>60 ]^ 1 (60 Da modification at any protein C-terminal)</li>\n" +
            "<li>70 c^ 1 (70 Da modification at any peptide C-terminal)</li>\n" +
            "</ul>\n" +
            "Example (M oxidation and N-terminal acetylation):\n" +
            "<ul>\n" +
            "<li>variable_mod_01 = 15.9949 M 3</li>\n" +
            "<li>variable_mod_02 = 42.0106 [^ 1</li>\n" +
            "</ul>");
    t.setDefaultRenderer(Float.class, new TableCellDoubleRenderer());
    t.setDefaultRenderer(Integer.class, new TableCellIntRenderer());

    // set cell editor for max occurs for var mods
    DefaultCellEditor cellEditorMaxOccurs = new TableCellIntSpinnerEditor(1, 5, 1);
    t.setDefaultEditor(Integer.class, cellEditorMaxOccurs);
    t.setFillsViewportHeight(true);

    return t;
  }

  private static Object[][] convertModsToFixTableData(List<Mod> mods) {
    Object[][] data = new Object[MsfraggerParams.ADDONS_HUMAN_READABLE.length][TABLE_FIX_MODS_COL_NAMES.length];
    for (int i = 0; i < data.length; i++) {
      data[i][0] = false;
      data[i][1] = MsfraggerParams.ADDONS_HUMAN_READABLE[i];
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

    ModsTable t = new ModsTable(m, TABLE_FIX_MODS_COL_NAMES, TabMsfragger::convertModsToFixTableData);
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

    FormEntry feMaxVarmodsPerMod = mu.feb(MsfraggerParams.PROP_max_variable_mods_per_peptide, new UiSpinnerInt(3, 0, 5, 1, 4))
        .label("Max variable mods on a peptide")
        .tooltip("<html>The maximum number of variable modifications allowed per\n" +
            "peptide sequence. This number does not include fixed modifications.").create();
    FormEntry feMaxCombos = mu.feb(MsfraggerParams.PROP_max_variable_mods_combinations, new UiSpinnerInt(5000, 0, 10000000, 500, 4))
        .label("Max combinations").create();

    FormEntry feUseAllModsInFirstSearch = mu.feb(MsfraggerParams.PROP_use_all_mods_in_first_search, new UiCheck("Use all mods in first search", null)).create();

    tableVarMods = createTableVarMods();
    SwingUtilities.invokeLater(() -> {
      setJTableColSize(tableVarMods, 0, 20, 150, 50);
    });
    JScrollPane varModsScroll = new JScrollPane(tableVarMods,
        JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
    //tableScrollVarMods.setPreferredSize(new Dimension(tableScrollVarMods.getPreferredSize().width, 140));

    pVarmods.add(feMaxVarmodsPerMod.label(), new CC().alignX("right"));
    pVarmods.add(feMaxVarmodsPerMod.comp);
    pVarmods.add(feMaxCombos.label(), new CC().alignX("right"));
    pVarmods.add(feMaxCombos.comp, new CC().alignX("right"));
    pVarmods.add(feUseAllModsInFirstSearch.comp, new CC().wrap());
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

  /** Panel with all the advanced options. */
  private JPanel createPanelAdvancedOptions() {
    JPanel p = mu.newPanel("Advanced options", new LC());

    mu.add(p, createPanelMassOffsets()).growX().wrap();
    mu.add(p, createPanelGlyco()).growX().wrap();
    mu.add(p, createPanelAdvancedSpectral()).growX().wrap();
    mu.add(p, createPanelAdvancedOpenSearch()).growX().wrap();
    mu.add(p, createPanelAdvancedOutput()).growX().wrap();
    mu.add(p, createPanelAdvancedPeakMatch()).growX().wrap();

    return p;
  }

  private JPanel createPanelMassOffsets() {
    JPanel p = mu.newPanel("Mass offsets", true);

    // create separate panels for the regular vs detailed offset mode to allow enabling only one at a time
    pOffsetRegular = mu.newPanel(null, mu.lcFillXNoInsetsTopBottom());
    pOffsetDetailed = mu.newPanel(null, mu.lcFillXNoInsetsTopBottom());

    uiTextRestrictDeltamassTo = UiUtils.uiTextBuilder().filter("[^A-Zal\\-nc\\[\\]^*]")
        .cols(20).create();
    FormEntry feRestrictDeltamassTo = mu.feb(uiTextRestrictDeltamassTo).name(MsfraggerParams.PROP_restrict_deltamass_to)
        .tooltip("Allowed mod sites in Open Search / Mass Offset Search /Glyco mode").label("Restrict delta mass to")
        .create();

    // mass offsets text field separately
    String tooltipMassOffsets = "<html>Creates multiple precursor tolerance windows with<br>\n"
        + "specified mass offsets. These values are multiplexed<br>\n"
        + "with the isotope error option.<br><br>\n\n"
        + "For example, value \"0/79.966\" can be used<br>\n"
        + "as a restricted open search that looks for unmodified<br>\n"
        + "and phosphorylated peptides (on any residue).<br><br>\n\n"
        + "Setting isotope_error to 0/1/2 in combination<br>\n"
        + "with this example will create search windows around<br>\n"
        + "(0,1,2,79.966, 80.966, 81.966). Masses can be separated\n"
        + " with \"/\" or space.";

    epMassOffsets = new UiText();
    epMassOffsets.setPreferredSize(new Dimension(100, 25));
    epMassOffsets.setBorder(new LineBorder(Color.LIGHT_GRAY, 1));

    uiTextMassOffsets = UiUtils.uiTextBuilder().filter("[^-\\(\\)\\./,\\d ]").text("0").create();

    FormEntry feMassOffsets = mu.feb(MsfraggerParams.PROP_mass_offsets, epMassOffsets)
        .label("Mass offsets")
        .tooltip(tooltipMassOffsets).create();

    uiCheckMassOffsetFile = UiUtils.createUiCheck("Use detailed mass offsets", false);
    String offsetCheckTip = "If checked, uses the provided detailed mass offset list to perform a mass offset search with specific allowed amino acids and/or fragment ions for each offset.";
    FormEntry feCheckMassOffsetFile = mu.feb(MsfraggerParams.PROP_use_detailed_offsets, uiCheckMassOffsetFile).tooltip(offsetCheckTip).create();

    epDetailedMassOffsets = new UiText();
    epDetailedMassOffsets.setPreferredSize(new Dimension(100, 25));
    epDetailedMassOffsets.setBorder(new LineBorder(Color.LIGHT_GRAY, 1));

    String tooltipOffsetDetailed = "(Optional) Detailed mass offset list. Overrides mass offset information above if provided. Can be loaded from & saved to template files using the buttons.";
    FormEntry feMassOffsetsDetailed = mu.feb(MsfraggerParams.PROP_mass_offsets_detailed, epDetailedMassOffsets)
            .label("Detailed mass offsets")
            .tooltip(tooltipOffsetDetailed).create();

    JButton btnLoadOffsetsFile = new JButton("Load offsets");
    btnLoadOffsetsFile.addActionListener(this::actionBtnLoadDetailedOffsets);
    JButton btnSaveOffsetsFile = new JButton("Save offsets");
    btnSaveOffsetsFile.addActionListener(this::actionBtnSaveDetailedOffsets);
    JButton btnEditOffsetsTable = new JButton("Edit detailed mass offsets");
    btnEditOffsetsTable.addActionListener(this::actionBtnEditDetailedOffsets);

    mu.add(pOffsetRegular, feMassOffsets.comp).spanX().growX().pushX().wrap();
    mu.add(pOffsetRegular, feRestrictDeltamassTo.label(), mu.ccR()).spanX().split(2);
    mu.add(pOffsetRegular, feRestrictDeltamassTo.comp).growX().pushX().wrap();

    mu.add(pOffsetDetailed, btnEditOffsetsTable).split();
    mu.add(pOffsetDetailed, feMassOffsetsDetailed.comp).growX().pushX().wrap();
    mu.add(pOffsetDetailed, btnLoadOffsetsFile).split();
    mu.add(pOffsetDetailed, btnSaveOffsetsFile).split().wrap();

    mu.add(p, pOffsetRegular).growX().wrap();
    mu.add(p, feCheckMassOffsetFile.comp).split();
    mu.add(p, pOffsetDetailed).growX().wrap();

    return p;
  }

  private JPanel createPanelAdvancedSpectral() {
    JPanel p = mu.newPanel("Spectral processing", true);

    FormEntry feMinPeaks = mu.feb(MsfraggerParams.PROP_minimum_peaks, new UiSpinnerInt(15, 0, 1000, 1, 4))
        .label("Min peaks").create();
    FormEntry feUseTopN = mu.feb(MsfraggerParams.PROP_use_topN_peaks, new UiSpinnerInt(150, 0, 1000000, 10, 4)).label("Use top N peaks").create();
    UiSpinnerDouble spinnerMinRatio = new UiSpinnerDouble(0.01, 0, 1, 0.01, 2, new DecimalFormat("0.00"));
    spinnerMinRatio.setColumns(4);
    FormEntry feMinRatio = mu.feb(MsfraggerParams.PROP_minimum_ratio, spinnerMinRatio).label("Min ratio").create();
    UiSpinnerDouble uiSpinnerClearRangeMzLo = UiUtils.spinnerDouble(0, 0, 100000, 10)
        .setCols(5).setFormat("#.###").create();
    FormEntry feClearRangeMzLo = mu.feb(PROP_misc_fragger_clear_mz_lo, uiSpinnerClearRangeMzLo)
        .label("Clear m/z range").create();
    UiSpinnerDouble uiSpinnerClearRangeMzHi = UiUtils.spinnerDouble(0, 0, 100000, 10)
        .setCols(5).setFormat("#.###").create();
    FormEntry feClearRangeMzHi = mu.feb(PROP_misc_fragger_clear_mz_hi, uiSpinnerClearRangeMzHi)
        .create();

    uiComboMassMode = new UiCombo(); // UiUtils.createUiCombo(FraggerPrecursorMassMode.values());
    uiComboMassMode.setModel(new DefaultComboBoxModel<>(new String[] {
        FraggerPrecursorMassMode.selected.name(),
        FraggerPrecursorMassMode.isolated.name(),
        FraggerPrecursorMassMode.corrected.name(),
    }));
    uiComboMassMode.addItemListener(TabMsfragger::actionChangeMassMode);
    FormEntry fePrecursorMassMode = mu.feb(MsfraggerParams.PROP_precursor_mass_mode, uiComboMassMode).label("Precursor mass mode")
        .tooltip("<html>Determines which entry from mzML files will be\n"
            + "used as the precursor's mass - 'Selected' or 'Isolated' ion.\n"
            + "'Corrected' performs mono-isotopic mass correction").create();

    FormEntry feRemovePrecPeak = mu.feb(MsfraggerParams.PROP_remove_precursor_peak, UiUtils.createUiCombo(RemovePrecursorPeak.getNames())).
            label("Remove precursor peak")
            .tooltip("Remove with all charge states is for ETD/ECD data and removes M+e peaks in addition to precursor peak")
            .create();
    DecimalFormat df1 = new DecimalFormat("0.#");
    FormEntry fePrecRemoveRangeLo = mu.feb(PROP_misc_fragger_remove_precursor_range_lo,
        UiSpinnerDouble.builder(-1.5, -1000.0, 1000.0, 0.1).setCols(5).setFormat(df1).create())
        .label("removal m/z range").create();
    FormEntry fePrecRemoveRangeHi = mu.feb(PROP_misc_fragger_remove_precursor_range_hi,
        UiSpinnerDouble.builder(+1.5, -1000.0, 1000.0, 0.1).setCols(5).setFormat(df1).create())
        .create();
    FormEntry feIntensityTransform = mu.feb(MsfraggerParams.PROP_intensity_transform, UiUtils.createUiCombo(IntensityTransform.getNames())).label("Intensity transform").create();
    FormEntry feCheckReuseDiaFragmentPeaks = mu.feb(MsfraggerParams.PROP_reuse_dia_fragment_peaks, UiUtils.createUiCheck("Reuse DIA fragment peaks", false)).tooltip("Allow the same peak matches to multiple peptides. For DIA data type only.").create();
    FormEntry feCheckSpectralFiles = mu.feb(MsfraggerParams.PROP_check_spectral_files, UiUtils.createUiCheck("Check spectral files", true)).tooltip("Checking spectral files before searching.").create();
    FormEntry feActivationFilter = mu.feb(MsfraggerParams.PROP_activation_filter, UiUtils.createUiCombo(ACTIVATION_TYPES_UI)).label("Activation type filter")
            .tooltip("Filter to include only scans matching the corresponding activation type.\n" +
                    "NOTE: not all instruments and activation types are supported. Use ETD for EThcd\n" +
                    "or other hybrid data.").create();
    FormEntry feAnalyzerTypes = mu.feb(MsfraggerParams.PROP_analyzer_types, UiUtils.createUiCombo(ANALYZER_TYPES_UI)).label("Analyzer filter")
        .tooltip("Filter to include only scans matching the corresponding analyzer type.\nNOTE: only support mzML and raw format files.").create();
    FormEntry feCheckRequirePrecursor = mu.feb(MsfraggerParams.PROP_require_precursor, UiUtils.createUiCheck("Require precursor", true))
        .tooltip("If required, PSMs with no precursor peaks will be discarded. For DIA data type only.").create();

    mu.add(p, feActivationFilter.label()).split(2);
    mu.add(p, feActivationFilter.comp);
    mu.add(p, fePrecursorMassMode.label()).split(2);
    mu.add(p, fePrecursorMassMode.comp);
    mu.add(p, feCheckSpectralFiles.comp);
    mu.add(p, feCheckRequirePrecursor.comp).pushX().wrap();

    mu.add(p, feAnalyzerTypes.label()).split(2);
    mu.add(p, feAnalyzerTypes.comp);
    mu.add(p, feMinPeaks.label()).split(2);
    mu.add(p, feMinPeaks.comp);
    mu.add(p, feUseTopN.label()).split(2);
    mu.add(p, feUseTopN.comp);
    mu.add(p, feMinRatio.label()).split(2);
    mu.add(p, feMinRatio.comp).wrap();

    mu.add(p, feClearRangeMzLo.label()).split(4);
    mu.add(p, feClearRangeMzLo.comp);
    mu.add(p, new JLabel("-"));
    mu.add(p, feClearRangeMzHi.comp);
    mu.add(p, feIntensityTransform.label()).split(2);
    mu.add(p, feIntensityTransform.comp);
    mu.add(p, feCheckReuseDiaFragmentPeaks.comp).wrap();

    mu.add(p, feRemovePrecPeak.label()).split(2);
    mu.add(p, feRemovePrecPeak.comp);
    mu.add(p, fePrecRemoveRangeLo.label()).split(4);
    mu.add(p, fePrecRemoveRangeLo.comp);
    mu.add(p, new JLabel("-"));
    mu.add(p, fePrecRemoveRangeHi.comp).wrap();
    return p;
  }


  /** Advanced peak matching panel */
  private JPanel createPanelAdvancedPeakMatch() {
    JPanel p = mu.newPanel("Advanced peak matching options", true);

    FormEntry feMinFragsModeling = mu.feb(MsfraggerParams.PROP_min_fragments_modelling, new UiSpinnerInt(2, 0, 1000, 1, 4)).label("Min frags modeling").create();
    FormEntry feMinMatchedFrags = mu.feb(MsfraggerParams.PROP_min_matched_fragments, new UiSpinnerInt(4, 0, 1000, 1, 4)).label("Min matched frags").create();

    FormEntry feIonSeries = mu.feb(MsfraggerParams.PROP_fragment_ion_series, new UiText(10))
        .label("Fragment ion series").tooltip(
            "Which peptide ion series to check against.\n"
                + "<b>Use spaces, commas or semicolons as delimiters</b>, e.g. \"b,y\"\n"
                + "This mostly depends on fragmentation method.\n"
                + "Typically \"b,y\" are used for CID and \"c,z\" for ECD.\n"
                + "MSFragger supports \"a,b,c,x,y,z,b-18,y-18,Y,b~,y~\" ion series.\n"
                + "Y refers to capital Y ions for glyco mode.\n"
                + "b~ and y~ refer to b and y ions with 203 modification for glyco mode.\n"
                + "<b>you can define your own in 'Define custom ion series' field</b>.\n"
                + "If you define custom series, you do not need to include the name here.").create();
    uiTextCustomIonSeries = new UiText(10);
    String tooltipCustomIonSeriesOriginal = "Custom ion series allow specification of arbitrary mass gains/losses\n"
        + "for N- and C-terminal ions. Separate multiple definitions by commas or semicolons.\n"
        + "<b>Format:</b> name terminus mass-delta\n"
        + "Example definition string:\n"
        + "b* N -17.026548;b0 N -18.010565\n"
        + "This would define two new ion types named <i>b*</i> and <i>b0</i>,\n"
        + "you can name them whatever you fancy. <i>b*</i> is the equivalent of an\n"
        + "N terminal b-ion with ammonia loss, <i>b0</i> is the same with water loss.\n"
        + "After defining the new type, you do not need to put them to the 'Fragment ion series' box.\n";
    FormEntry feCustomSeries = mu.feb(MsfraggerParams.PROP_ion_series_definitions, uiTextCustomIonSeries)
        .label("Add custom ion series").tooltip(tooltipCustomIonSeriesOriginal).create();
    labelCustomIonSeries = feCustomSeries.label();

    FormEntry feTrueTolUnits = mu.feb(MsfraggerParams.PROP_precursor_true_units, UiUtils.createUiCombo(
        MassTolUnits.values())).label("Precursor true tolerance").create();
    UiSpinnerDouble uiSpinnerTrueTol = new UiSpinnerDouble(10, 0, 100000, 5,
        new DecimalFormat("0.###"));
    uiSpinnerTrueTol.setColumns(4);
    FormEntry feTrueTol = mu.feb(MsfraggerParams.PROP_precursor_true_tolerance, uiSpinnerTrueTol)
        .tooltip("True precursor mass tolerance should be set to your instrument's\n"
            + "precursor mass accuracy(window is +/- this value).  This value is used\n"
            + "for tie breaking of results and boosting of unmodified peptides in open\n"
            + "search.").create();

    String tooltipPrecursorCHarge =
        "Assume range of potential precursor charge states.\n" +
            "Only relevant when override_charge is set to 1 or \n" +
            "there is no charge information in scans.";
    FormEntry fePrecursorChargeLo = mu.feb(PROP_misc_fragger_precursor_charge_lo, new UiSpinnerInt(1, 0, 30, 1, 2))
        .tooltip(tooltipPrecursorCHarge).create();
    FormEntry fePrecursorChargeHi = mu.feb(PROP_misc_fragger_precursor_charge_hi, new UiSpinnerInt(4, 0, 30, 1, 2))
        .tooltip(tooltipPrecursorCHarge).create();
    FormEntry feOverrideCharge = mu.feb(MsfraggerParams.PROP_override_charge, new UiCheck("Override charge with precursor charge", null))
        .tooltip("Ignores precursor charge and uses charge state\n" +
            "specified in precursor_charge range.").create();

    UiCombo uiComboDeisotope = UiUtils.createUiCombo(DEISOTOPE);
    FormEntry feDeisotope = mu.feb(MsfraggerParams.PROP_deisotope, uiComboDeisotope)
        .label("Deisotope").tooltip("Deisotope the MS/MS spectrum before matching. Won't affect the MS spectrum.").create();

    UiCombo uiComboDeneutralloss = UiUtils.createUiCombo(new String[]{"Yes", "No"});
    FormEntry feComboDeneutralloss = mu.feb(MsfraggerParams.PROP_deneutralloss, uiComboDeneutralloss)
        .label("Deneutralloss").tooltip("Deneutralloss the MS/MS spectrum before matching. Won't affect the MS spectrum.").create();

    UiSpinnerInt uiSpinnerMaxFragCharge = new UiSpinnerInt(2, 1, 20, 1, 2);
    FormEntry feMaxFragCharge = mu.feb(MsfraggerParams.PROP_max_fragment_charge, uiSpinnerMaxFragCharge)
        .label("Max fragment charge").tooltip("Enabled only when Deisotope = 0.").create();

    uiComboDeisotope.addItemListener(e -> {
      if (uiComboDeisotope.getSelectedIndex() > 0) {
        uiComboDeneutralloss.setSelectedIndex(0);
        updateEnabledStatus(uiComboDeneutralloss, true);
      } else {
        uiComboDeneutralloss.setSelectedIndex(1);
        updateEnabledStatus(uiComboDeneutralloss, false);
      }
    });

    mu.add(p, feMinFragsModeling.label(), mu.ccR());
    mu.add(p, feMinFragsModeling.comp);
    mu.add(p, feMinMatchedFrags.label(), mu.ccR());
    mu.add(p, feMinMatchedFrags.comp);
    mu.add(p, feMaxFragCharge.label(), mu.ccR());
    mu.add(p, feMaxFragCharge.comp).pushX().wrap();
    mu.add(p, feDeisotope.label(), mu.ccR());
    mu.add(p, feDeisotope.comp);
    mu.add(p, feIonSeries.label(), mu.ccR());
    mu.add(p, feIonSeries.comp).growX();
    mu.add(p, labelCustomIonSeries, mu.ccR());
    mu.add(p, feCustomSeries.comp).spanX().growX().wrap();
    mu.add(p, feComboDeneutralloss.label(), mu.ccR());
    mu.add(p, feComboDeneutralloss.comp);
    mu.add(p, feTrueTolUnits.label(), mu.ccR());
    mu.add(p, feTrueTolUnits.comp).split(2);
    mu.add(p, feTrueTol.comp).growX();
    mu.add(p, feOverrideCharge.comp).split(4).spanX();
    mu.add(p, fePrecursorChargeLo.comp);
    mu.add(p, new JLabel("-"));
    mu.add(p, fePrecursorChargeHi.comp).wrap();

    return p;
  }

  private JPanel createPanelAdvancedOutput() {
    JPanel p = mu.newPanel("Advanced output options", true);

    uiSpinnerOutputReportTopNDda = new UiSpinnerInt(1, 1, 10000, 1, 4);
    uiSpinnerOutputReportTopNDia1 = new UiSpinnerInt(5, 1, 10000, 1, 4);
    uiSpinnerOutputReportTopNDdaPlus = new UiSpinnerInt(5, 1, 7, 1, 4);

    FormEntry feReportTopNDda = mu.feb(MsfraggerParams.PROP_output_report_topN, uiSpinnerOutputReportTopNDda)
        .label("Report top N for DDA")
        .tooltip("Report top N PSMs per input spectrum for DDA data.").create();

    FormEntry feReportTopNDia1 = mu.feb(MsfraggerParams.PROP_output_report_topN_dia1, uiSpinnerOutputReportTopNDia1)
        .label("Report top N for DIA")
        .tooltip("Report top N PSMs per input spectrum for DIA, DIA-Lib, and GPF-DIA data type.").create();

    FormEntry feReportTopNDdaPlus = mu.feb(MsfraggerParams.PROP_output_report_topN_dda_plus, uiSpinnerOutputReportTopNDdaPlus)
        .label("Report top N for DDA+")
        .tooltip("Report top N PSMs per input spectrum for DDA+ data type.").create();

    UiSpinnerDouble uiSpinnerOutputMaxExpect = new UiSpinnerDouble(50, 0, Double.MAX_VALUE, 1,
        new DecimalFormat("0.#"));
    uiSpinnerOutputMaxExpect.setColumns(4);
    FormEntry feOutputMaxExpect = mu.feb(MsfraggerParams.PROP_output_max_expect, uiSpinnerOutputMaxExpect).label("Output max expect")
        .tooltip("Suppresses reporting of PSM if top hit has<br> expectation greater than this threshold").create();


    uiComboOutputType = UiUtils.createUiCombo(FraggerOutputType.values());
    FormEntry feOutputType = mu.feb(MsfraggerParams.PROP_output_format, uiComboOutputType).label("Output format")
        .tooltip("How the search results are to be reported.\n" +
            "Downstream tools only support pepXML format.\n\n" +
            "Use PIN if you want to process the result results with Percolator by yourself.\n" +
            "Use TSV (tab delimited file) if you want to process \n" +
            "search results yourself for easier import into other software.").create();

    FormEntry feReportAltProts = mu.feb(MsfraggerParams.PROP_report_alternative_proteins, new UiCheck("Report alternative proteins", null, false)).create();

    uiCheckWriteCalibratedMzml = UiUtils.createUiCheck("Write calibrated mzML", false);
    FormEntry feCheckWriteCalibratedMzml = mu.feb(MsfraggerParams.PROP_write_calibrated_mzml, uiCheckWriteCalibratedMzml).create();

    uiComboGroupVariable = UiUtils.createUiCombo(Arrays.asList(GROUP_VARIABLE));
    FormEntry feGroupVariable= mu.feb(uiComboGroupVariable)
        .name(PROP_group_variable)
        .label("Group variable")
        .tooltip("Specify the variable to group PSMs for the FDR estimation.")
        .create();

    mu.add(p, feReportTopNDda.label(), mu.ccR());
    mu.add(p, feReportTopNDda.comp);
    mu.add(p, feReportAltProts.comp).gapLeft("20px");
    mu.add(p, feOutputType.label(), mu.ccR()).gapLeft("10px");
    mu.add(p, feOutputType.comp).pushX().wrap();

    mu.add(p, feReportTopNDdaPlus.label(), mu.ccR());
    mu.add(p, feReportTopNDdaPlus.comp);
    mu.add(p, feCheckWriteCalibratedMzml.comp).gapLeft("20px");
    mu.add(p, feGroupVariable.label(), mu.ccR()).gapLeft("10px");
    mu.add(p, feGroupVariable.comp).wrap();

    mu.add(p, feReportTopNDia1.label(), mu.ccR());
    mu.add(p, feReportTopNDia1.comp);
    mu.add(p, feOutputMaxExpect.label()).gapLeft("20px").split(2);
    mu.add(p, feOutputMaxExpect.comp).wrap();

    return p;
  }

  private JPanel createPanelAdvancedOpenSearch() {
    JPanel p = new JPanel(new MigLayout(new LC()));
    p.setBorder(new TitledBorder("Open search options"));
    FormEntry feTrackZeroTopN = mu.feb(MsfraggerParams.PROP_track_zero_topN,
        new UiSpinnerInt(0, 0, 1000, 5, 5)).label("Track zero top N").create();
    UiSpinnerDouble spinnerZeroBinAcceptExpect = new UiSpinnerDouble(0, 0, Double.MAX_VALUE, 0.1, 5,
        new DecimalFormat("0.##########"));
    spinnerZeroBinAcceptExpect.setColumns(5);
    FormEntry feZeroBinAcceptExpect = mu.feb(MsfraggerParams.PROP_zero_bin_accept_expect, spinnerZeroBinAcceptExpect)
        .label("Zero bin accept expect").create();
    UiSpinnerDouble spinnerZeroBinMultExpect = new UiSpinnerDouble(1, 0, 1, 0.05, 5,
        new DecimalFormat("0.##########"));
    spinnerZeroBinMultExpect.setColumns(5);
    FormEntry feZeroBinMultExpect = mu.feb(MsfraggerParams.PROP_zero_bin_mult_expect, spinnerZeroBinMultExpect)
        .label("Zero bin multiply expect").create();
    uiComboMassDiffToVariableMod = UiUtils.createUiCombo(MASS_DIFF_TO_VAR_MOD);
    FormEntry feComboMassDiffToVariableMod = mu.feb(MsfraggerParams.PROP_mass_diff_to_variable_mod, uiComboMassDiffToVariableMod)
        .label("Report mass shift as a variable mod")
            .tooltip("Places/replaces delta mass with an assigned (variable) mod.\n" +
                    "*No*: off (default)\n" +
                    "*Yes, keep delta mass*: Adds a variable modification, but does NOT change the delta mass\n" +
                    "Allows differential modeling of delta masses in PeptideProphet (used for glyco quant)\n" +
                    "Does NOT work with PTM-Shepherd\n" +
                    "MSFragger param value 2\n" +
                    "*Yes, remove delta mass*: Adds variable mod and changes delta mass to near 0\n" +
                    "PeptideProphet accmass modeling only (no modeling by delta mass)\n" +
                    "Does NOT work with PTM-Shepherd\n" +
                    "MSFragger param value 1")
            .create();

    UiText uiTextShiftedIonsExclusion = new UiText();
    uiTextShiftedIonsExclusion.setDocument(DocumentFilters.getFilter("[A-Za-z]"));
    uiTextShiftedIonsExclusion.setText("(-1.5,3.5)");
    FormEntry feShiftedIonsExclusion = mu.feb(MsfraggerParams.PROP_delta_mass_exclude_ranges,
        uiTextShiftedIonsExclusion).label("Delta mass exclude range")
        .tooltip("<html>Range expressed like: (-1.5,3.5)").create();

    uiCheckLocalizeDeltaMass = new UiCheck("<html>Localize mass shift (LOS)", null, false);
    FormEntry feLocalizeDeltaMass = mu.feb(MsfraggerParams.PROP_localize_delta_mass,
        uiCheckLocalizeDeltaMass)
        .tooltip("<html>Use additional shifted ion series when matching fragments.\n"
            + "Shifted ion series are the same as regular b/y ions,\n"
            + "but with the addition of the mass shift of the precursor.\n"
            + "Regular ion series will still be used.\n"
            + "This option is <b>incompatible</b> with database splitting.").create();

    mu.add(p, feComboMassDiffToVariableMod.label(), mu.ccR());
    mu.add(p, feComboMassDiffToVariableMod.comp).split().spanX().wrap();

    mu.add(p, feTrackZeroTopN.label(), mu.ccR());
    mu.add(p, feTrackZeroTopN.comp).wrap();

    mu.add(p, feZeroBinAcceptExpect.label(), mu.ccR());
    mu.add(p, feZeroBinAcceptExpect.comp);
    mu.add(p, feZeroBinMultExpect.label(), mu.ccR());
    mu.add(p, feZeroBinMultExpect.comp).wrap();

    mu.add(p, feShiftedIonsExclusion.label(), mu.ccR());
    mu.add(p, feShiftedIonsExclusion.comp).growX().spanX().wrap();

    mu.add(p, feLocalizeDeltaMass.comp).skip(1).wrap();

    return p;
  }

  public static void setJTableColSize(JTable table, int colIndex, int minW, int maxW, int prefW) {
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

  private void formFrom(MsfraggerParams params) {
    Map<String, String> map = confToUiMap(params);
    Map<String, String> prependedMap = MapUtils.remapKeys(map, k -> StringUtils.prependOnce(k, TAB_PREFIX));
    Map<String, String> translatedMap = FragpipeCacheUtils.translateValuesToUi(prependedMap);
    formFrom(translatedMap);
    tableVarMods.setData(params.getVariableMods());
    tableFixMods.setData(params.getFixedMods());
    updateRowHeights(tableVarMods);
    setJTableColSize(tableVarMods, 0, 20, 150, 50);
    updateRowHeights(tableFixMods);
    setJTableColSize(tableFixMods, 0, 20, 150, 50);
  }

  private MsfraggerParams formCollect() {
    Map<String, String> map = formToMap();

    // ram and threads parameters have been moved to Workflow tab, need to re-inject them when saving fragger params
    TabWorkflow tabWorkflow = Fragpipe.getStickyStrict(TabWorkflow.class);
    map.put(MsfraggerParams.PROP_num_threads, itos(tabWorkflow.getThreads()));

    map.put(MsfraggerParams.PROP_group_variable, getGroupVariable());

    MsfraggerParams params = paramsFromMap(map);

    // before collecting mods, make sure that no table cell editor is open
    stopJTableEditing(tableFixMods);
    stopJTableEditing(tableVarMods);

    List<Mod> modsVar = formToMap(tableVarMods.model);
    params.setVariableMods(modsVar);
    List<Mod> modsFix = formToMap(tableFixMods.model);
    params.setFixedMods(modsFix);

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

  private List<Mod> formToMap(ModsTableModel model) {
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

  public MsfraggerParams getParams() {
    return formCollect();
  }

  public Set<Float> getVarModMassSet() {
    List<Mod> modsVar = formToMap(tableVarMods.model);
    Set<Float> modMassSet = new TreeSet<>();
    for (Mod mod : modsVar) {
      if (mod != null && mod.isEnabled && mod.sites != null && !mod.sites.trim().isEmpty() && Math.abs(mod.massDelta) > 0.01) {
        modMassSet.add(mod.massDelta);
      }
    }
    return modMassSet;
  }

  public Set<Float> getFixedModMassSet() {
    List<Mod> modsVar = formToMap(tableFixMods.model);
    Set<Float> modMassSet = new TreeSet<>();
    for (Mod mod : modsVar) {
      if (mod != null && mod.isEnabled && mod.sites != null && !mod.sites.trim().isEmpty() && Math.abs(mod.massDelta) > 0.01) {
        modMassSet.add(mod.massDelta);
      }
    }
    return modMassSet;
  }

  public Set<Float> getMassOffsetSet() {
    Set<Float> outputSet = new TreeSet<>();
    if (uiCheckMassOffsetFile.isSelected()) {
      // detailed/fancy offset mode
      ArrayList<MassOffsetUtils.MassOffset> offsets = getDetailedOffsets();
      for (MassOffsetUtils.MassOffset offset: offsets) {
        if (Math.abs(offset.mass) > 0.01f) {
          outputSet.add(offset.mass);
        }
      }
    } else {
      // regular offset mode
      String[] ss = epMassOffsets.getNonGhostText().trim().split("[/\\s]+");
      for (String s : ss) {
        if (s.isEmpty()) {
          continue;
        }
        float v = Float.parseFloat(s);
        if (Math.abs(v) > 0.01f) {
          outputSet.add(v);
        }
      }
    }
    return outputSet;
  }

  public ArrayList<MassOffsetUtils.MassOffset> getDetailedOffsets() {
    String offsetStr = epDetailedMassOffsets.getNonGhostText();
    ArrayList<MassOffsetUtils.MassOffset> offsets = new ArrayList<>();
    if (!offsetStr.isEmpty()) {
      String[] splits = offsetStr.split(";");
      for (String split: splits) {
        offsets.add(new MassOffsetUtils.MassOffset(split));
      }
    }
    return offsets;
  }

  public void setDetailedOffsets(String offsetsText) {
    epDetailedMassOffsets.setText(offsetsText);
  }

  public String getRegularOffsetStringFromDetailedOffsets() {
    ArrayList<MassOffsetUtils.MassOffset> offsets = getDetailedOffsets();
    String[] offsetStrs = Arrays.stream(offsets.toArray(new MassOffsetUtils.MassOffset[0])).map(offset -> Double.toString(offset.mass)).toArray(String[]::new);
    return String.join("/", offsetStrs);
  }

  public void setMassOffsets(String offsetsText) {
    epMassOffsets.setText(offsetsText);
  }

  public String getGroupVariable() {
    return uiComboGroupVariable.getSelectedItem().toString();
  }


  /**
   * Converts textual representations of all fields in the form to stadard {@link MsfraggerParams}.
   */
  private MsfraggerParams paramsFromMap(Map<String, String> map) {
    MsfraggerParams p = new MsfraggerParams();
    final double[] precursorRemoveRange = new double[2];
    final double[] clearMzRange = new double[2];
    final double[] digestMassRange = new double[2];
    final int[] precursorChargeRange = new int[2];

    for (Entry<String, String> e : map.entrySet()) {
      final String k = e.getKey();
      final String v = e.getValue();
      if (MsfraggerParams.PROP_NAMES_SET.contains(k)) {
        // known property
        Function<String, String> converter = CONVERT_TO_FILE.getOrDefault(k, s -> s);
        String converted = converter.apply(v);

        if (MsfraggerParams.PROP_fragment_ion_series.equals(k) && StringUtils.isNullOrWhitespace(converted)) {
          // don't set ion series to be used in the fragger config file if the string is emtpty
          continue;
        }
        if (MsfraggerParams.PROP_labile_fragment_ion_series.equals(k) && StringUtils.isNullOrWhitespace(converted)) {
            // don't set ion series to be used in the fragger config file if the string is emtpty
            continue;
        }
        p.getProps().setProp(k, converted);

      } else {
        // unknown prop, it better should be from the "misc" category we added in this panel
        if (PROPS_MISC_NAMES.contains(k) || k.startsWith("misc.")) {
          log.trace("Found misc option: {}={}", k, v);

          switch (k) {
            case PROP_misc_fragger_remove_precursor_range_lo:
              precursorRemoveRange[0] = Double.parseDouble(v);
              break;
            case PROP_misc_fragger_remove_precursor_range_hi:
              precursorRemoveRange[1] = Double.parseDouble(v);
              break;
            case PROP_misc_fragger_clear_mz_lo:
              clearMzRange[0] = Double.parseDouble(v);
              break;
            case PROP_misc_fragger_clear_mz_hi:
              clearMzRange[1] = Double.parseDouble(v);
              break;
            case PROP_misc_fragger_digest_mass_lo:
              digestMassRange[0] = Double.parseDouble(v);
              break;
            case PROP_misc_fragger_digest_mass_hi:
              digestMassRange[1] = Double.parseDouble(v);
              break;
            case PROP_misc_fragger_precursor_charge_lo:
              precursorChargeRange[0] = Integer.parseInt(v);
              break;
            case PROP_misc_fragger_precursor_charge_hi:
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
    p.setClearMzRange(clearMzRange);
    p.setDigestMassRange(digestMassRange);
    p.setPrecursorCharge(precursorChargeRange);
    p.setRemovePrecursorRange(precursorRemoveRange);

    FraggerOutputType outType = p.getOutputFormat();
    if (outType == null) {
      throw new IllegalStateException("FraggerOutputType was not set by the point where we needed to provide the output extension.");
    }

    return p;
  }

  private Map<String, String> confToUiMap(MsfraggerParams params) {
    HashMap<String, String> map = new HashMap<>();
    for (Entry<String, Prop> e : params.getProps().getMap().entrySet()) {
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
    double[] precursorRemoveRange = params.getRemovePrecursorRange();
    double[] clearMzRange = params.getClearMzRange();
    double[] digestMassRange = params.getDigestMassRange();
    int[] precursorCharge = params.getPrecursorCharge();
    DecimalFormat fmt = new DecimalFormat("0.#");
    DecimalFormat fmt2 = new DecimalFormat("0.##");
    map.put(PROP_misc_fragger_clear_mz_lo, fmt.format(clearMzRange[0]));
    map.put(PROP_misc_fragger_clear_mz_hi, fmt.format(clearMzRange[1]));
    map.put(PROP_misc_fragger_digest_mass_lo, fmt.format(digestMassRange[0]));
    map.put(PROP_misc_fragger_digest_mass_hi, fmt.format(digestMassRange[1]));
    map.put(PROP_misc_fragger_precursor_charge_lo, fmt.format(precursorCharge[0]));
    map.put(PROP_misc_fragger_precursor_charge_hi, fmt.format(precursorCharge[1]));
    map.put(PROP_misc_fragger_remove_precursor_range_lo, fmt2.format(precursorRemoveRange[0]));
    map.put(PROP_misc_fragger_remove_precursor_range_hi, fmt2.format(precursorRemoveRange[1]));

    return map;
  }

  @Subscribe(sticky = true, threadMode = ThreadMode.MAIN_ORDERED)
  public void on(NoteConfigMsfragger m) {
    updateEnabledStatus(this, m.isValid());
  }

  @Subscribe(sticky = true, threadMode = ThreadMode.MAIN_ORDERED)
  public void on(NoteConfigDbsplit m) {
    log.debug("Got NoteConfigDbsplit. Setting MSFragger tab DB Split option to enabled={}", m.isValid());
    updateEnabledStatus(uiSpinnerDbsplit, m.isValid());
  }

  @Subscribe
  public void on(MessageValidityMassCalibration msg) {
    log.debug("Got message 'MessageValidityMassCalibration' reading isValid = {} ", msg.isValid);
    enablementMapping.put(uiComboMassCalibrate, msg.isValid);
    updateEnabledStatus(uiComboMassCalibrate, msg.isValid);
  }

  @Subscribe
  public void on(MessagePrecursorSelectionMode m) {
    log.debug("Received MessagePrecursorSelectionMode [{}]. Doing nothing.", m.mode.name());
  }

  @Subscribe
  public void on(MessageMsfraggerParamsUpdate m) {
    log.debug("Got MessageMsfraggerParamsUpdate, updating TabMsfragger via formFrom(m.params)");
    formFrom(m.params);
  }

  @Subscribe
  public void on(MessageSearchType m) {
    loadDefaults(m.type);
  }

  public boolean isRun() {
    return SwingUtils.isEnabledAndChecked(checkRun) && !isDigestOnly();
  }

  public boolean isChecked() {
    return checkRun.isSelected();
  }

  public boolean isDigestOnly() {
    return checkDigestOnly.isSelected();
  }

  public int getMassCalibration() {
    if (uiComboMassCalibrate.getSelectedIndex() == 0) {
      return 0;
    } else if (uiComboMassCalibrate.getSelectedIndex() == 1) {
      return 1;
    } else if (uiComboMassCalibrate.getSelectedIndex() == 2) {
      return 2;
    } else {
      return 0;
    }
  }

  public boolean isWriteCalMzml() {
    return uiCheckWriteCalibratedMzml.isSelected();
  }

  public void setWriteCalMzml(boolean selected) {
    uiCheckWriteCalibratedMzml.setSelected(selected);
  }

  public int getNumDbSlices() {
    return uiSpinnerDbsplit.getActualValue();
  }

  public void setNumDbSlices(int v) {
    uiSpinnerDbsplit.setValue(v);
  }

  public String getMassOffsets() {
    if (uiCheckMassOffsetFile.isSelected()) {
      return getRegularOffsetStringFromDetailedOffsets();
    } else {
      return uiTextMassOffsets.getNonGhostText();
    }
  }

  public String getOutputFileExt() {
    return getOutputType().getExtension();
  }

  public FraggerOutputType getOutputType() {
    String val = uiComboOutputType.getItemAt(uiComboOutputType.getSelectedIndex());
    return FraggerOutputType.valueOf(val);
  }

  public int getOutputReportTopNDda() {
    return uiSpinnerOutputReportTopNDda.getActualValue();
  }

  public int getOutputReportTopNDia1() {
    return uiSpinnerOutputReportTopNDia1.getActualValue();
  }


  public int getOutputReportTopNDdaPlus() {
    return uiSpinnerOutputReportTopNDdaPlus.getActualValue();
  }

  public List<Mod> getVarModsTable() { return formToMap(tableVarMods.model); }

  private void actionBtnLoadDetailedOffsets(ActionEvent event) {
    List<FileFilter> tsvFilters = new ArrayList<>();
    FileFilter filter = new FileNameExtensionFilter("Mass offset file (.tsv or .txt)", "tsv", "txt");
    tsvFilters.add(filter);

    String loc = Fragpipe.propsVarGet(PROP_FILECHOOSER_LAST_PATH);
    JFileChooser fc = FileChooserUtils.builder("Select the Detailed Mass Offsets file to load")
            .approveButton("Select").mode(FileChooserUtils.FcMode.FILES_ONLY)
            .acceptAll(false).multi(false).filters(tsvFilters)
            .paths(Stream.of(loc)).create();


    String selectedPath;
    int userSelection = fc.showOpenDialog(SwingUtils.findParentFrameForDialog(this));
    if (JFileChooser.APPROVE_OPTION == userSelection) {
      selectedPath = fc.getSelectedFile().toString();
      try {
        Fragpipe.propsVarSet(PROP_FILECHOOSER_LAST_PATH, selectedPath);
        String offsetStr = MassOffsetUtils.parseOffsetsFile(selectedPath, this);
        Fragpipe.propsVarSet(PROP_mass_offsets_detailed, offsetStr);
        epDetailedMassOffsets.setText(offsetStr);
      } catch (IOException ex) {
        log.error(String.format("Could not load mass offsets from file %s", selectedPath));
        SwingUtils.showErrorDialogWithStacktrace(ex, this);
      }
    }
  }

  private void actionBtnSaveDetailedOffsets(ActionEvent event) {
    // get file path to save
    FileNameEndingFilter filter = new FileNameEndingFilter("Mass offset file (.tsv)", "tsv");
    Path savePath = TabWorkflow.getSaveFilePath(null, PROP_FILECHOOSER_LAST_PATH, filter, ".tsv", false, this);

    if (savePath == null) {
      // user cancelled action
      return;
    }

    // parse detailed mass offset string
    String offsetStr = epDetailedMassOffsets.getNonGhostText();
    ArrayList<MassOffsetUtils.MassOffset> offsets = new ArrayList<>();
    if (!offsetStr.isEmpty()) {
      String[] splits = offsetStr.split(";");
      for (String split: splits) {
        offsets.add(new MassOffsetUtils.MassOffset(split));
      }
    }

    // save to file
    try {
      PrintWriter out = new PrintWriter(savePath.toFile());
      out.print("# Mass\tAllowed sites\tDiagnostic ions\tPeptide remainder ions\tFragment remainder ions\n");
      for (MassOffsetUtils.MassOffset offset : offsets) {
        out.print(offset.toFileString());
      }
      out.flush();
      out.close();
    } catch (IOException ex) {
      log.error(String.format("Could not save mass offsets to file %s", savePath));
      SwingUtils.showErrorDialogWithStacktrace(ex, this);
    }
  }

  /**
   * Open a table dialog to edit detailed mass offsets.
   */
  private void actionBtnEditDetailedOffsets(ActionEvent actionEvent) {
    DetailedOffsetEditDialog tableDialog = new DetailedOffsetEditDialog(SwingUtils.findParentFrame(this), getDetailedOffsets());

    tableDialog.setVisible(true);
    if (tableDialog.getDialogResult() != JOptionPane.OK_OPTION) {
      return;
    }

    // get new data
    stopJTableEditing(tableDialog.table);
    List<MassOffsetUtils.MassOffset> updatedOffsets = tableDialog.getModel().getOffsets();

    // save new offsets to parameters and GUI text field
    String offsetStr = updatedOffsets.stream().map(MassOffsetUtils.MassOffset::toString).collect(Collectors.joining(MassOffsetUtils.DELIMITER));
    Fragpipe.propsVarSet(PROP_mass_offsets_detailed, offsetStr);
    epDetailedMassOffsets.setText(offsetStr);
  }

    private void loadDefaults(SearchTypeProp type) {
    log.debug("TabMsfragger loadDefaults() called for SearchTypeProp type={}", type.name());
    MsfraggerParams params = new MsfraggerParams();
    params.loadDefaults(type);
    formFrom(params);

    // reset some fields that are not part of Fragger config file
    uiSpinnerDbsplit.setValue(1);
  }

  /**
   * @return False if user's confirmation was required, but they cancelled the operation. True
   *         otherwise.
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

  private boolean trySelectEnzymeDropdown2(String name) {
    try {
      String saveCuts = uiTextCuts2.getNonGhostText();
      String saveNouts = uiTextNocuts2.getNonGhostText();
      uiComboEnzymes2.setSelectedItem(name);
      if (true || "custom".equals(name)) { // remove 'true' to reset specificities to their definitions in our file
        uiTextCuts2.setText(saveCuts);
        uiTextNocuts2.setText(saveNouts);
      }
      return true;
    } catch (Exception ignored) {
    }
    return false;
  }

  public String getEnzymeName() {
    return uiTextEnzymeName.getNonGhostText();
  }
  public String getEnzymeCut2() {
    return uiTextCuts2.getNonGhostText();
  }

  /**
   * Format for each enzyme is "NT=[name];CS=[regex]", where regex is the regular expression of the enzyme
   * cleavage rules. Format for regex: (?<=[cut])(?![no cut])
   * @return
   */
  public ArrayList<String> getSDRFenzymes() {
    ArrayList<String> enzymes = new ArrayList<>();
    if (!uiTextCuts.getNonGhostText().isEmpty()) {
      String regex = String.format("(?%s[%s])", uiComboSense.getSelectedItem() == "C" ? "<=" : "=", uiTextCuts.getNonGhostText());
      if (!uiTextNocuts.getNonGhostText().isEmpty()){
        regex += String.format("(?![%s])", uiTextNocuts.getNonGhostText());
      }
      enzymes.add(String.format("NT=%s;CS=%s", SDRFtable.mapEnzymeToSDRF(uiTextEnzymeName.getNonGhostText()), regex));
    }
    if (!uiTextCuts2.getNonGhostText().isEmpty()) {
      String regex = String.format("(?%s[%s])", uiComboSense2.getSelectedItem() == "C" ? "<=" : "=", uiTextCuts2.getNonGhostText());
      if (!uiTextNocuts2.getNonGhostText().isEmpty()){
        regex += String.format("(?![%s])", uiTextNocuts2.getNonGhostText());
      }
      enzymes.add(String.format("NT=%s;CS=%s", SDRFtable.mapEnzymeToSDRF(uiTextEnzymeName2.getNonGhostText()), regex));
    }
    return enzymes;
  }

  /**
   * Get SDRF formatted strings for each modification searched.
   * Format for mods is "NT=[name];MT=[Fixed/Variable];PP=[term];TA=[S,T,Y];MM=[mass]",
   * where name can be anything and term is one of "Anywhere, Protein N-term, Protein C-term, Any N-term, Any C-term"
   * @return
   */
  public ArrayList<String> getSDRFmods() {
    ArrayList<String> mods = new ArrayList<>();
    List<Mod> modsVariable = formToMap(tableVarMods.model);
    List<Mod> modsFixed = formToMap(tableFixMods.model);
    List<Mod> modsOffset = getOffsetsAsMods();

    Pattern aaPattern = Pattern.compile("([A-Z])");
    for (Mod mod : modsVariable) {
      if (mod.isEnabled) {
        // find all amino acid sites
        ArrayList<String> sites = new ArrayList<>();
        Matcher aaMatcher = aaPattern.matcher(mod.sites);
        while (aaMatcher.find()) {
          sites.add(aaMatcher.group());
        }
        String terminal = getSDRFvarmodTerm(mod.sites);
        ArrayList<String> sitesWithTermini = updateSitesTerm(terminal, sites);
        String sitesTemp = String.join("", sites);

        String unimodName = SDRFtable.matchUnimod(mod.massDelta, sitesWithTermini);
        String name = getModName(mod.massDelta, mod.sites, unimodName);
        String siteStr = sitesTemp.isEmpty() ? "" : String.format(";TA=%s", sitesTemp);
        String modStr = String.format("NT=%s;MT=Variable;PP=%s%s;MM=%.5f", name, terminal, siteStr, mod.massDelta);
        mods.add(modStr);
      }
    }
    for (Mod mod : modsFixed) {
      if (mod.isEnabled && mod.massDelta != 0.0) {
        // get terminal
        String terminal = getSDRFfixedmodTerm(mod.sites);
        // get amino acid if not terminal
        String sitesStr;
        ArrayList<String> sites = new ArrayList<>();
        if (terminal.matches("Anywhere")) {
          Matcher aaMatcher = aaPattern.matcher(mod.sites);
          while (aaMatcher.find()) {
            sites.add(aaMatcher.group());
          }
          sitesStr = String.format(";TA=%s", String.join("", sites));
        } else {
          sitesStr = "";
        }
        ArrayList<String> sitesWithTermini = updateSitesTerm(terminal, sites);
        String unimodName = SDRFtable.matchUnimod(mod.massDelta, sitesWithTermini);
        String name = getModName(mod.massDelta, mod.sites, unimodName);
        String modStr = String.format("NT=%s;MT=Fixed;PP=%s%s;MM=%.5f", name, terminal, sitesStr, mod.massDelta);
        mods.add(modStr);
      }
    }
    for (Mod offset : modsOffset) {
      if (!(offset.massDelta == 0.0)) {
        ArrayList<String> sites = Arrays.stream(offset.sites.split("")).collect(Collectors.toCollection(ArrayList::new));
        String unimodName = SDRFtable.matchUnimod(offset.massDelta, sites);
        String name = unimodName != null ? unimodName : String.format("offset:%.5f", offset.massDelta);
        String glycoSite = "";
        String offsetResidues = offset.sites;
        if (Objects.equals(uiComboGlyco.getSelectedItem(), GLYCO_OPTION_nglycan)) {
          glycoSite = "TS=N[^P][ST];";
          offsetResidues = "N";
        }
        String modStr = String.format("NT=%s;MT=Variable;PP=%s;TA=%s;%sMM=%.5f", name, "Anywhere", offsetResidues, glycoSite, offset.massDelta);
        mods.add(modStr);
      }
    }
    return mods;
  }

  /**
   * Return a list of Mod objects from the mass offsets, including all site restrictions for regular and fancy
   * offset modes.
   * @return
   */
  public List<Mod> getOffsetsAsMods(){
    ArrayList<Mod> offsetMods = new ArrayList<>();
    if (uiCheckMassOffsetFile.isSelected()) {
      // fancy/detailed offset mode
      ArrayList<MassOffsetUtils.MassOffset> offsets = getDetailedOffsets();
      for (MassOffsetUtils.MassOffset offset: offsets) {
        offsetMods.add(new Mod(offset.mass, String.join("", offset.allowedResidues), true, 1));
      }
    } else {
      // regular offset mode
      Set<Float> offsetSet = getMassOffsetSet();
      String offsetResidues = uiTextRestrictDeltamassTo.getNonGhostText().replace("-", "");
      if (offsetResidues.matches("all")) {
        offsetResidues = "ACDEFGHIKLMNPQRSTVWY";
      }
      for (float mass : offsetSet) {
        offsetMods.add(new Mod(mass, offsetResidues, true, 1));
      }
    }
    return offsetMods;
  }

  private String getModName(double mass, String sites, String unimodName) {
    if (unimodName == null) {
      // Unimod either not matched or ambiguous: use our generic format
      return String.format("%s:%.0f", sites, mass);
    } else {
      return unimodName;
    }
  }

  private String getSDRFvarmodTerm(String modSites) {
    if (modSites.contains("[")) {
      return "Protein N-term";
    } else if (modSites.contains("]")) {
      return "Protein C-term";
    } else if (modSites.contains("n")) {
      return "Any N-term";
    } else if (modSites.contains("c")) {
      return "Any C-term";
    } else {
      return "Anywhere";
    }
  }

  /**
   * Add terminal site indicators to the sites list for Unimod matching
   * @return
   */
  private ArrayList<String> updateSitesTerm(String terminal, ArrayList<String> sites) {
    ArrayList<String> sitesWithTerm = new ArrayList<>(sites);
    switch (terminal) {
      case "Anywhere":
        return sitesWithTerm;
      case "Protein N-term":
      case "Any N-term":
        sitesWithTerm.add("N-term");
        return sitesWithTerm;
      case "Protein C-term":
      case "Any C-term":
        sitesWithTerm.add("C-term");
        return sitesWithTerm;
    }
    return sitesWithTerm;
  }

  private String getSDRFfixedmodTerm(String modSites) {
    if (modSites.toLowerCase().contains("n-term")) {
      if (modSites.toLowerCase().contains("protein")) {
        return  "Protein N-term";
      } else {
        return "Any N-term";
      }
    } else if (modSites.toLowerCase().contains("c-term")) {
      if (modSites.toLowerCase().contains("protein")) {
        return "Protein C-term";
      } else {
        return "Any C-term";
      }
    } else {
      return "Anywhere";
    }
  }

  public String getPrecTolString(){
    Object unit = uiComboPrecursorTolUnits.getSelectedItem();
    if (unit == null || StringUtils.isNullOrWhitespace((String) unit)) {
      return "";
    }

    double tolerance = uiSpinnerPrecTolHi.getActualValue() - uiSpinnerPrecTolLo.getActualValue();
    if (unit.equals("PPM")) {
      return String.format("%.1f %s", tolerance, unit.toString().toLowerCase());
    } else {
      return String.format("%.2f %s", tolerance, unit);
    }
  }

  public String getProdTolString(String logText){
    String unit = "";
    double tolerance = -1;
    boolean useTolFromLog = false;

    // if param optimization was enabled, read the optimized fragment tolerance from log
    Object calibration = uiComboMassCalibrate.getSelectedItem();
    if (calibration != null) {
      if (calibration.equals("Mass calibration, parameter optimization")) {
        Pattern newFragTolPattern = Pattern.compile("New fragment_mass_tolerance = ([0-9\\.]+) ([\\w]+)");
        Matcher matcher = newFragTolPattern.matcher(logText);
        if (matcher.find()) {
          tolerance = Double.parseDouble(matcher.group(1));
          unit = matcher.group(2);
          useTolFromLog = true;
        }
      }
    }

    if (!useTolFromLog) {
      // read original tolerance from MSFragger params
      unit = (String) uiComboFragTolUnits.getSelectedItem();
      if (unit == null || StringUtils.isNullOrWhitespace(unit)) {
        return "";
      }
      tolerance = uiSpinnerFragTol.getActualValue();
    }

    if (unit.equals("PPM")) {
      return String.format("%s %s", tolerance, unit.toLowerCase());
    } else {
      return String.format("%.2f %s", tolerance, unit);
    }
  }

  public static Map<String, String> getInstrumentMap(String logText) {
    Map<String, String> out = new HashMap<>();
    Matcher matcher = pattern.matcher(logText);
    while (matcher.find()) {
      out.put(Paths.get(matcher.group(1)).getFileName().toString().trim(), matcher.group(2).trim());
    }
    return out;
  }
}
