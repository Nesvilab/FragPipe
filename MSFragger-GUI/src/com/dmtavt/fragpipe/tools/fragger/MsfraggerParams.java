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
package com.dmtavt.fragpipe.tools.fragger;

import com.github.chhh.utils.StringUtils;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.dmtavt.fragpipe.api.SearchTypeProp;
import com.dmtavt.fragpipe.params.AbstractParams;
import com.dmtavt.fragpipe.params.Props;
import com.dmtavt.fragpipe.params.Props.Prop;
import com.dmtavt.fragpipe.tools.enums.CleavageType;
import com.dmtavt.fragpipe.tools.enums.FraggerOutputType;
import com.dmtavt.fragpipe.tools.enums.FraggerPrecursorMassMode;
import com.dmtavt.fragpipe.tools.enums.MassTolUnits;
import com.dmtavt.fragpipe.tools.enums.PrecursorMassTolUnits;
import com.github.chhh.utils.SwingUtils;

import javax.swing.*;

/**
 *
 * @author dmitriya
 */
public class MsfraggerParams extends AbstractParams {
    private static final Logger log = LoggerFactory.getLogger(MsfraggerParams.class);

    public static final Pattern reShiftedIonsExclusionRange = Pattern.compile("\\(\\s*(?<v1>-?\\d+(?:\\.\\d+)?)\\s*,\\s*(?<v2>-?\\d+(?:\\.\\d+)?)\\s*\\)");

    public static final String PROP_labile_search_mode = "labile_search_mode";
    public static final String PROP_restrict_deltamass_to = "restrict_deltamass_to";
    public static final String PROP_diagnostic_intensity_filter = "diagnostic_intensity_filter";
    public static final String PROP_Y_type_masses = "Y_type_masses";
    public static final String PROP_diagnostic_fragments = "diagnostic_fragments";
    public static final String PROP_remainder_masses = "remainder_fragment_masses";
    public static final String PROP_min_sequence_matches = "min_sequence_matches";

    public static final String PROP_database_name = "database_name";
    public static final String PROP_decoy_prefix = "decoy_prefix";
    public static final String PROP_fragpipe_ram = "fragpipe_ram";
    public static final String PROP_num_threads = "num_threads";
    public static final String PROP_precursor_mass_lower = "precursor_mass_lower";
    public static final String PROP_precursor_mass_upper = "precursor_mass_upper";
    public static final String PROP_precursor_mass_units = "precursor_mass_units";
    public static final String PROP_precursor_true_tolerance = "precursor_true_tolerance";
    public static final String PROP_precursor_true_units = "precursor_true_units";
    public static final String PROP_fragment_mass_tolerance = "fragment_mass_tolerance";
    public static final String PROP_fragment_mass_units = "fragment_mass_units";
    public static final String PROP_data_type = "data_type";

    public static final String PROP_remove_precursor_peak = "remove_precursor_peak";
    public static final String PROP_remove_precursor_range = "remove_precursor_range";
    public static final String PROP_intensity_transform = "intensity_transform";
    public static final String PROP_reuse_dia_fragment_peaks = "reuse_dia_fragment_peaks";
    public static final String PROP_check_spectral_files = "check_spectral_files";
    public static final String PROP_require_precursor = "require_precursor";
    public static final String PROP_activation_filter = "activation_types";
    public static final String PROP_analyzer_types = "analyzer_types";
    public static final String PROP_write_calibrated_mzml = "write_calibrated_mzml";
    public static final String PROP_write_uncalibrated_mzml = "write_uncalibrated_mzml";
    public static final String PROP_write_mzbin_all = "write_mzbin_all";
    public static final String PROP_mass_diff_to_variable_mod = "mass_diff_to_variable_mod";
    public static final String PROP_group_variable = "group_variable";
    public static final String PROP_calibrate_mass = "calibrate_mass";
    public static final String PROP_use_all_mods_in_first_search = "use_all_mods_in_first_search";
    public static final String PROP_isotope_error = "isotope_error";
    public static final String PROP_deisotope = "deisotope";
    public static final String PROP_deneutralloss = "deneutralloss";
    public static final String PROP_mass_offsets = "mass_offsets";
    public static final String PROP_mass_offsets_detailed = "mass_offsets_detailed";
    public static final String PROP_use_detailed_offsets = "use_detailed_offsets";
    public static final String PROP_precursor_mass_mode = "precursor_mass_mode";
    public static final String PROP_search_enzyme_name_1 = "search_enzyme_name_1";
    public static final String PROP_search_enzyme_name_2 = "search_enzyme_name_2";
    public static final String PROP_search_enzyme_cut_1 = "search_enzyme_cut_1";
    public static final String PROP_search_enzyme_nocut_1 = "search_enzyme_nocut_1";
    public static final String PROP_search_enzyme_cut_2 = "search_enzyme_cut_2";
    public static final String PROP_search_enzyme_nocut_2 = "search_enzyme_nocut_2";
    public static final String PROP_num_enzyme_termini = "num_enzyme_termini";
    public static final String PROP_allowed_missed_cleavage_1 = "allowed_missed_cleavage_1";
    public static final String PROP_search_enzyme_sense_1 = "search_enzyme_sense_1";
    public static final String PROP_search_enzyme_sense_2 = "search_enzyme_sense_2";
    public static final String PROP_allowed_missed_cleavage_2 = "allowed_missed_cleavage_2";
    public static final String PROP_clip_nTerm_M = "clip_nTerm_M";
    
    /** Followed by '_N' (underscore and a number), max 7 mods. */
    public static final String PROP_variable_mod = "variable_mod";
    public static final int VAR_MOD_COUNT_MAX = Short.BYTES * 8;
    public static final String PROP_max_variable_mods_per_peptide = "max_variable_mods_per_peptide";
    public static final String PROP_max_variable_mods_combinations = "max_variable_mods_combinations";
    public static final String PROP_output_format = "output_format";
    public static final String PROP_output_report_topN = "output_report_topN";
    public static final String PROP_output_report_topN_dia1 = "output_report_topN_dia1";
    public static final String PROP_output_report_topN_dda_plus = "output_report_topN_dda_plus";
    public static final String PROP_report_alternative_proteins = "report_alternative_proteins";
    public static final String PROP_output_max_expect = "output_max_expect";
    public static final String PROP_precursor_charge = "precursor_charge";
    public static final String PROP_override_charge = "override_charge";
    public static final String PROP_ms_level = "ms_level";
    public static final String PROP_digest_min_length = "digest_min_length";
    public static final String PROP_digest_max_length = "digest_max_length";
    public static final String PROP_digest_mass_range = "digest_mass_range";
    public static final String PROP_max_fragment_charge = "max_fragment_charge";
    public static final String PROP_fragment_ion_series = "fragment_ion_series";
    public static final String PROP_ion_series_definitions = "ion_series_definitions";

    
    // Open search params
    
    public static final String PROP_track_zero_topN = "track_zero_topN";
    public static final String PROP_zero_bin_accept_expect = "zero_bin_accept_expect";
    public static final String PROP_zero_bin_mult_expect = "zero_bin_mult_expect";
    public static final String PROP_localize_delta_mass = "localize_delta_mass";
    public static final String PROP_delta_mass_exclude_ranges = "delta_mass_exclude_ranges";

    // Spectral processing
    
    public static final String PROP_minimum_peaks = "minimum_peaks";
    public static final String PROP_use_topN_peaks = "use_topN_peaks";
    public static final String PROP_min_fragments_modelling = "min_fragments_modelling";
    public static final String PROP_min_matched_fragments = "min_matched_fragments";
    public static final String PROP_minimum_ratio = "minimum_ratio";
    public static final String PROP_clear_mz_range = "clear_mz_range";
    public static final String PROP_add = "add";
    public static final String PROP_add_enabled = "add_enabled";


    public static final String[] PROP_NAMES = {
        PROP_labile_search_mode,
        PROP_restrict_deltamass_to,
        PROP_diagnostic_intensity_filter,
        PROP_Y_type_masses,
        PROP_diagnostic_fragments,
        PROP_remainder_masses,
        PROP_min_sequence_matches,
        PROP_database_name,
        PROP_decoy_prefix,
        PROP_fragpipe_ram,
        PROP_num_threads,
        PROP_precursor_mass_lower,
        PROP_precursor_mass_upper,
        PROP_precursor_mass_units,
        PROP_precursor_true_tolerance,
        PROP_precursor_true_units,
        PROP_fragment_mass_tolerance,
        PROP_fragment_mass_units,
        PROP_data_type,
        PROP_remove_precursor_peak,
        PROP_remove_precursor_range,
        PROP_intensity_transform,
        PROP_reuse_dia_fragment_peaks,
        PROP_check_spectral_files,
        PROP_require_precursor,
        PROP_write_calibrated_mzml,
        PROP_write_uncalibrated_mzml,
        PROP_write_mzbin_all,
        PROP_mass_diff_to_variable_mod,
        PROP_group_variable,
        PROP_calibrate_mass,
        PROP_use_all_mods_in_first_search,
        PROP_isotope_error,
        PROP_deisotope,
        PROP_deneutralloss,
        PROP_mass_offsets,
        PROP_mass_offsets_detailed,
        PROP_use_detailed_offsets,
        PROP_precursor_mass_mode,
        PROP_search_enzyme_name_1,
        PROP_search_enzyme_name_2,
        PROP_search_enzyme_cut_1,
        PROP_search_enzyme_nocut_1,
        PROP_search_enzyme_cut_2,
        PROP_search_enzyme_nocut_2,
        PROP_num_enzyme_termini,
        PROP_allowed_missed_cleavage_1,
        PROP_allowed_missed_cleavage_2,
        PROP_search_enzyme_sense_1,
        PROP_search_enzyme_sense_2,
        PROP_clip_nTerm_M,
        PROP_variable_mod,
        PROP_max_variable_mods_per_peptide,
        PROP_max_variable_mods_combinations,
        PROP_output_format,
        PROP_output_report_topN,
        PROP_report_alternative_proteins,
        PROP_output_max_expect,
        PROP_precursor_charge,
        PROP_override_charge,
        PROP_ms_level,
        PROP_digest_min_length,
        PROP_digest_max_length,
        PROP_digest_mass_range,
        PROP_max_fragment_charge,
        PROP_fragment_ion_series,
        PROP_ion_series_definitions,
        PROP_track_zero_topN,
        PROP_zero_bin_accept_expect,
        PROP_zero_bin_mult_expect,
        PROP_localize_delta_mass,
        PROP_delta_mass_exclude_ranges,
        PROP_minimum_peaks,
        PROP_use_topN_peaks,
        PROP_min_fragments_modelling,
        PROP_min_matched_fragments,
        PROP_minimum_ratio,
        PROP_clear_mz_range,
        PROP_add,
        PROP_add_enabled,
        PROP_activation_filter,
        PROP_analyzer_types
    };

    public static final Set<String> PROP_NAMES_SET;
    static {
        PROP_NAMES_SET = new HashSet<>(Arrays.asList(PROP_NAMES));
    }

    public static final String[] ADDON_NAMES = {"Cterm_peptide", "Nterm_peptide", "Cterm_protein", "Nterm_protein",
        "G_glycine", "A_alanine", "S_serine", "P_proline", "V_valine", "T_threonine", "C_cysteine", "L_leucine", 
        "I_isoleucine", "N_asparagine", "D_aspartic_acid", "Q_glutamine", "K_lysine", "E_glutamic_acid", "M_methionine", 
        "H_histidine", "F_phenylalanine", "R_arginine", "Y_tyrosine", "W_tryptophan",
        "B_user_amino_acid", "J_user_amino_acid", "O_user_amino_acid", "U_user_amino_acid", "X_user_amino_acid", "Z_user_amino_acid",
    };
    
    public static final String[] ADDONS_HUMAN_READABLE = {"C-Term Peptide", "N-Term Peptide", "C-Term Protein", "N-Term Protein", 
        "G (glycine)", "A (alanine)", "S (serine)", "P (proline)", "V (valine)", "T (threonine)", "C (cysteine)", "L (leucine)", 
        "I (isoleucine)", "N (asparagine)", "D (aspartic acid)", "Q (glutamine)", "K (lysine)", "E (glutamic acid)", "M (methionine)", 
        "H (histidine)", "F (phenylalanine)", "R (arginine)", "Y (tyrosine)", "W (tryptophan)", 
        "B ", "J", "O", "U", "X", "Z", };


    public static final String GLYCO_OPTION_off = "off";
    public static final String GLYCO_OPTION_nglycan = "nglycan";
    public static final String GLYCO_OPTION_labile = "labile";
    public static final List<String> GLYCO_OPTIONS = Arrays
        .asList(GLYCO_OPTION_off, GLYCO_OPTION_nglycan, GLYCO_OPTION_labile);
    public static final String ACTIVATION_TYPE_ALL = "all";
    public static final String ACTIVATION_TYPE_HCD = "HCD";
    public static final String ACTIVATION_TYPE_ETD = "ETD";
    public static final String ACTIVATION_TYPE_CID = "CID";
    public static final String ACTIVATION_TYPE_ECD = "ECD";
    public static final List<String> ACTIVATION_TYPES = Arrays
            .asList(ACTIVATION_TYPE_ALL, ACTIVATION_TYPE_HCD, ACTIVATION_TYPE_ETD, ACTIVATION_TYPE_CID, ACTIVATION_TYPE_ECD);

    public static final String ANALYZER_TYPE_ALL = "all";
    public static final String ANALYZER_TYPE_FTMS = "FTMS";
    public static final String ANALYZER_TYPE_ITMS = "ITMS";
    public static final List<String> ANALYZER_TYPES = Arrays.asList(ANALYZER_TYPE_ALL, ANALYZER_TYPE_FTMS, ANALYZER_TYPE_ITMS);

    public static final Map<String, String> ADDON_MAP_NAME2HUMAN = new HashMap<>(ADDON_NAMES.length);
    public static final Map<String, String> ADDON_MAP_HUMAN2NAME = new HashMap<>(ADDON_NAMES.length);
    static {
        for (int i = 0; i < ADDON_NAMES.length; i++) {
            String name = ADDON_NAMES[i];
            String humanReadable = ADDONS_HUMAN_READABLE[i];
            ADDON_MAP_NAME2HUMAN.put(name, humanReadable);
            ADDON_MAP_HUMAN2NAME.put(humanReadable, name);
        }
    }

    /** This file is in the jar, use getResourceAsStream() to get it.  */
    public static final String CACHE_FILE = "fragger.params";
    public static final String DEFAULT_FILE_OPENSEARCH = "fragger_open.params";
    public static final String DEFAULT_FILE_CLOSEDSEARCH = "fragger_closed.params";
    public static final String DEFAULT_FILE_NONSPECIFICSEARCH = "fragger_nonspecific.params";
    public static final String DEFAULT_FILE_OFFSETSEARCH = "fragger_offset.params";

    private static final DecimalFormat DF = new DecimalFormat("0.##########");
    
        
    public MsfraggerParams() {
        super();
        props = new Props(createComments());
        // preload some defaults to get the correct ordering
        initFromResource(DEFAULT_FILE_CLOSEDSEARCH);
    }

    private MsfraggerParams(SearchTypeProp st) {
        super();
        props = new Props(createComments());
        Map<SearchTypeProp, String> map = new HashMap<>();
        map.put(SearchTypeProp.open, DEFAULT_FILE_OPENSEARCH);
        map.put(SearchTypeProp.closed, DEFAULT_FILE_CLOSEDSEARCH);
        map.put(SearchTypeProp.nonspecific, DEFAULT_FILE_NONSPECIFICSEARCH);
        map.put(SearchTypeProp.offset, DEFAULT_FILE_OFFSETSEARCH);
        String resource = map.get(st);
        if (resource == null)
            throw new IllegalStateException("No mapping for search type: " + st.name());
        initFromResource(resource);
    }

    public MsfraggerParams(MsfraggerParams other) {
        super(other);
    }

    private void initFromResource(String resource) {
        try {
            load(MsfraggerParams.class.getResourceAsStream(resource), true);
        } catch (IOException ex) {
            log.error("Error while initializing new MsfraggerParams from resource: " + resource, ex);
        }
    }

    public static MsfraggerParams getDefault(SearchTypeProp searchType) {
        return new MsfraggerParams(searchType);
    }

    private Map<String, String> createComments() {
        Map<String, String> c= new HashMap<>();
        c.put(PROP_num_threads, "Number of CPU threads to use.");
        c.put(PROP_database_name, "Path to the protein database file in FASTA format.");
        c.put(PROP_precursor_mass_lower, "Lower bound of the precursor mass window.");
        c.put(PROP_precursor_mass_upper, "Upper bound of the precursor mass window.");
        c.put(PROP_precursor_mass_units, "Precursor mass tolerance units (0 for Da, 1 for ppm).");
        c.put(PROP_precursor_true_tolerance, "True precursor mass tolerance (window is +/- this value).");
        c.put(PROP_precursor_true_units, "True precursor mass tolerance units (0 for Da, 1 for ppm).");
        c.put(PROP_fragment_mass_tolerance, "Fragment mass tolerance (window is +/- this value).");
        c.put(PROP_fragment_mass_units, "Fragment mass tolerance units (0 for Da, 1 for ppm).");
        c.put(PROP_data_type, "Data type (0 for DDA, 1 for DIA, 2 for gas-phase fractionation DIA, 3 for DDA+).");
        c.put(PROP_calibrate_mass, "Perform mass calibration (0 for OFF, 1 for ON, 2 for ON and find optimal parameters, 4 for ON and find the optimal fragment mass tolerance).");
        c.put(PROP_use_all_mods_in_first_search, "Use all variable modifications in first search (0 for No, 1 for Yes).");
        c.put(PROP_write_calibrated_mzml, "Write calibrated MS2 scan to a mzML file (0 for No, 1 for Yes).");
        c.put(PROP_write_uncalibrated_mzml, "Write uncalibrated MS2 scan to a MGF file (0 for No, 1 for Yes). Only for .raw and .d formats.");
        c.put(PROP_decoy_prefix, "Prefix of the decoy protein entries. Used for parameter optimization only.");
        c.put(PROP_isotope_error, "Also search for MS/MS events triggered on specified isotopic peaks.");
        c.put(PROP_mass_offsets, "Creates multiple precursor tolerance windows with specified mass offsets.");
        c.put(PROP_mass_offsets_detailed, "Optional detailed mass offset list. Overrides mass_offsets if use_detailed_offsets = 1.");
        c.put(PROP_use_detailed_offsets, "Whether to use the regular (0) or detailed (1) mass offset list.");
        c.put(PROP_restrict_deltamass_to, "Specify amino acids on which delta masses (mass offsets or search modifications) can occur. Allowed values are single letter codes (e.g. ACD) and '-', must be capitalized. Use 'all' to allow any amino acid.");
        c.put(PROP_labile_search_mode, "type of search (nglycan, labile, or off). Off means non-labile/typical search.");
        c.put(PROP_precursor_mass_mode, "One of isolated/selected/corrected.");
        c.put(PROP_localize_delta_mass, "Include fragment ions mass-shifted by unknown modifications (recommended for open and mass offset searches) (0 for OFF, 1 for ON).");
        c.put(PROP_delta_mass_exclude_ranges, "Exclude mass range for shifted ions searching.");
        c.put(PROP_fragment_ion_series, "Ion series used in search, specify any of a,b,c,x,y,z,Y,b-18,y-18 (comma separated).");
        c.put(PROP_ion_series_definitions, "User defined ion series. Example: \"b* N -17.026548;b0 N -18.010565\".");
        c.put(PROP_search_enzyme_name_1, "Name of the first enzyme.");
        c.put(PROP_search_enzyme_name_2, "Name of the second enzyme.");
        c.put(PROP_search_enzyme_cut_1, "First enzyme's cutting amino acid.");
        c.put(PROP_search_enzyme_nocut_1, "First enzyme's protecting amino acid.");
        c.put(PROP_search_enzyme_cut_2, "Second enzyme's cutting amino acid.");
        c.put(PROP_search_enzyme_nocut_2, "Second enzyme's protecting amino acid.");
        c.put(PROP_num_enzyme_termini, "0 for non-enzymatic, 1 for semi-enzymatic, and 2 for fully-enzymatic.");
        c.put(PROP_allowed_missed_cleavage_1, "First enzyme's allowed number of missed cleavages per peptide. Maximum value is 5.");
        c.put(PROP_allowed_missed_cleavage_2, "Second enzyme's allowed number of missed cleavages per peptide. Maximum value is 5.");
        c.put(PROP_search_enzyme_sense_1, "First enzyme's cutting terminal.");
        c.put(PROP_search_enzyme_sense_2, "Second enzyme's cutting terminal.");
        c.put(PROP_clip_nTerm_M, "Specifies the trimming of a protein N-terminal methionine as a variable modification (0 or 1).");
        c.put(PROP_max_variable_mods_per_peptide, "Maximum total number of variable modifications per peptide.");
        c.put(PROP_max_variable_mods_combinations, "Maximum number of modified forms allowed for each peptide (up to 65534).");
        c.put(PROP_mass_diff_to_variable_mod, "Put mass diff as a variable modification. 0 for no; 1 for yes and remove delta mass; 2 for yes and keep delta mass.");
        c.put(PROP_group_variable, "Specify the variable used to decide the PSM group in the group FDR estimation. 0 = no group FDR; 1 = num_enzyme_termini; 2 = PE from protein header.");
        c.put(PROP_output_format, "File format of output files (tsv, pin, pepxml, tsv_pin, tsv_pepxml, pepxml_pin, or tsv_pepxml_pin).");
        c.put(PROP_output_report_topN, "Reports top N PSMs per input spectrum.");
        c.put(PROP_output_max_expect, "Suppresses reporting of PSM if top hit has expectation value greater than this threshold.");
        c.put(PROP_report_alternative_proteins, "Report alternative proteins for peptides that are found in multiple proteins (0 for no, 1 for yes).");
        c.put(PROP_precursor_charge, "Assumed range of potential precursor charge states. Only relevant when override_charge is set to 1.");
        c.put(PROP_override_charge, "Ignores precursor charge and uses charge state specified in precursor_charge range (0 or 1).");
        c.put(PROP_digest_min_length, "Minimum length of peptides to be generated during in-silico digestion.");
        c.put(PROP_digest_max_length, "Maximum length of peptides to be generated during in-silico digestion.");
        c.put(PROP_digest_mass_range, "Mass range of peptides to be generated during in-silico digestion in Daltons.");
        c.put(PROP_max_fragment_charge, "Maximum charge state for theoretical fragments to match (1-4).");
        c.put(PROP_track_zero_topN, "Track top N unmodified peptide results separately from main results internally for boosting features.");
        c.put(PROP_zero_bin_accept_expect, "Ranks a zero-bin hit above all non-zero-bin hit if it has expectation less than this value.");
        c.put(PROP_zero_bin_mult_expect, "Multiplies expect value of PSMs in the zero-bin during  results ordering (set to less than 1 for boosting).");
        c.put(PROP_minimum_peaks, "Minimum number of peaks in experimental spectrum for matching.");
        c.put(PROP_use_topN_peaks, "Pre-process experimental spectrum to only use top N peaks.");
        c.put(PROP_deisotope, "Perform deisotoping or not (0=no, 1=yes and assume singleton peaks single charged, 2=yes and assume singleton peaks single or double charged).");
        c.put(PROP_deneutralloss, "Perform deneutrallossing or not (0=no, 1=yes).");
        c.put(PROP_min_fragments_modelling, "Minimum number of matched peaks in PSM for inclusion in statistical modeling.");
        c.put(PROP_min_matched_fragments, "Minimum number of matched peaks for PSM to be reported.");
        c.put(PROP_minimum_ratio, "Filters out all peaks in experimental spectrum less intense than this multiple of the base peak intensity.");
        c.put(PROP_clear_mz_range, "Removes peaks in this m/z range prior to matching.");
        c.put(PROP_remove_precursor_peak, " Remove precursor peaks from tandem mass spectra. 0 = not remove; 1 = remove the peak with precursor charge; 2 = remove the peaks with all charge states (only for DDA mode).");
        c.put(PROP_remove_precursor_range, "m/z range in removing precursor peaks. Only for DDA mode. Unit: Th.");
        c.put(PROP_intensity_transform, "Transform peaks intensities with sqrt root. 0 = not transform; 1 = transform using sqrt root.");
        c.put(PROP_check_spectral_files, "Checking spectral files before searching.");
        c.put(PROP_reuse_dia_fragment_peaks, "Allow the same peak matches to multiple peptides. For DIA data type only. 0 = no, 1 = yes.");
        c.put(PROP_require_precursor, "If required, PSMs with no precursor peaks will be discarded. For DIA data type only. 0 = no, 1 = yes.");
        c.put(PROP_Y_type_masses, " [nglycan/labile search_mode only]. Specify fragments of labile mods that are commonly retained on intact peptides (e.g. Y ions for glycans). Only used if 'Y' is included in fragment_ion_series.");
        c.put(PROP_diagnostic_fragments, "[nglycan/labile search_mode only]. Specify diagnostic fragments of labile mods that appear in the low m/z region. Only used if diagnostic_intensity_filter > 0.");
        c.put(PROP_remainder_masses, "[labile search_mode only] List of possible remainder fragment ions to consider. Remainder masses are partial modification masses left on b/y ions after fragmentation.");
        c.put(PROP_min_sequence_matches, "[nglycan/labile search_mode only] Minimum number of sequence-specific (not Y) ions to record a match.");
        c.put(PROP_diagnostic_intensity_filter, "[nglycan/labile search_mode only]. Minimum relative intensity for SUM of all detected oxonium ions to achieve for spectrum to contain diagnostic fragment evidence. Calculated relative to spectrum base peak. 0 <= value.");
        c.put(PROP_activation_filter, "Filter to only search scans of provided activation type(s). Allowed: All, HCD, CID, ETD, ECD.");
        c.put(PROP_analyzer_types, "Filter to only include scans matching the provided analyzer type(s) in search. Only support the mzML and raw format. Allowed types: all, FTMS, ITMS.");
        return c;
    }

    @Override
    public void loadDefault() {
        loadDefaults(SearchTypeProp.closed);
    }
    
    @Override
    public Path tempFileName() {
        return Paths.get(CACHE_FILE);
    }

    public void loadDefaults(SearchTypeProp type) {
        try {
            switch (type) {
                case open:
                    load(MsfraggerParams.class.getResourceAsStream(DEFAULT_FILE_OPENSEARCH), true);
                    break;
                case closed:
                    load(MsfraggerParams.class.getResourceAsStream(DEFAULT_FILE_CLOSEDSEARCH), true);
                    break;
                case nonspecific:
                    load(MsfraggerParams.class.getResourceAsStream(DEFAULT_FILE_NONSPECIFICSEARCH), true);
                    break;
                case offset:
                    load(MsfraggerParams.class.getResourceAsStream(DEFAULT_FILE_OFFSETSEARCH), true);
                    break;
                default:
                    throw new AssertionError(type.name());
            }
        } catch (IOException e) {
            throw new IllegalStateException("Could not load MSFragger defaults for " + type.name() + " from the jar itself.", e);
        }
    }

    public String getLabileSearchMode() {
        return props.getProp(PROP_labile_search_mode, "none").value;
    }
    public String getDeltamassAllowedResidues() {
        return props.getProp(PROP_restrict_deltamass_to, "all").value;
    }
    public String getDiagnosticIntensityFilter() {
        return props.getProp(PROP_diagnostic_intensity_filter, "0").value;
    }
    public String getYTypeMasses() {
        return props.getProp(PROP_Y_type_masses, "").value;
    }
    public String getDiagnosticFragments() {
        return props.getProp(PROP_diagnostic_fragments, "").value;
    }
    public String getRemainderMasses() { return props.getProp(PROP_remainder_masses, "").value; }
    public String getMinSequenceMatches() { return props.getProp(PROP_min_sequence_matches, "2").value; }

    public void setLabileSearchMode(String v) {
        props.setProp(PROP_labile_search_mode, v);
    }
    public void setDeltamassAllowedResidues(String v) {
        props.setProp(PROP_restrict_deltamass_to, v);
    }
    public void setDiagnosticIntensityFilter(String v) {
        props.setProp(PROP_diagnostic_intensity_filter, v);
    }
    public void setYTypeMasses(String v) {
        props.setProp(PROP_Y_type_masses, v);
    }
    public void setDiagnosticFragments(String v) {
        props.setProp(PROP_diagnostic_fragments, v);
    }
    public void setRemainderMasses(String v) {
        props.setProp(PROP_remainder_masses, v);
    }
    public void setMinSequenceMatches(String v){ props.setProp(PROP_min_sequence_matches, v); }

    public String getDatabaseName() {
        return props.getProp(PROP_database_name, "").value;
    }
    
    public void setDatabaseName(String databaseName) {
        props.setProp(PROP_database_name, databaseName);
    }
    
    public String getDecoyPrefix() {
        return props.getProp(PROP_decoy_prefix, "rev_").value;
    }

    public void setDecoyPrefix(String decoyPrefix) {
        props.setProp(PROP_decoy_prefix, decoyPrefix);
    }

    public int getFragpipeRam() {
        return Integer.parseInt(props.getProp(PROP_fragpipe_ram, "0").value);
    }
    
    public void setFragpipeRam(int ramGb) {
        props.setProp(PROP_fragpipe_ram, Integer.toString(ramGb));
    }
    
    public int getNumThreads() {
        return Integer.parseInt(props.getProp(PROP_num_threads, "1").value);
    }
    
    public void setNumThreads(int numThreads) {
        props.setProp(PROP_num_threads, Integer.toString(numThreads));
    }

    public void setWriteMzbinAll(boolean v) {
        setInt(PROP_write_mzbin_all, v ? 1 : 0);
    }

    public boolean getWriteMzbinAll() {
        return getInt(PROP_write_mzbin_all, "0") == 1;
    }

    public int getWriteCalibratedMzml() {
        return getInt(PROP_write_calibrated_mzml, "0");
    }

    public void setWriteCalibratedMzml(int v) {
        setInt(PROP_write_calibrated_mzml, v);
    }

    public int getMassDiffToVariableMod() {
        return getInt(PROP_mass_diff_to_variable_mod, "0");
    }

    public int getGroupVariable() {
        return getInt(PROP_group_variable, "0");
    }

    public void setGroupVariable(int v) {
        setInt(PROP_group_variable, v);
    }

    public void setMassDiffToVariableMod(int v) {
        setInt(PROP_mass_diff_to_variable_mod, v);
    }

    public int getRemovePrecursorPeak() {
        return getInt(PROP_remove_precursor_peak, "1");
    }

    public void setRemovePrecursorPeak(int v) {
        setInt(PROP_remove_precursor_peak, v);
    }

    public double[] getRemovePrecursorRange() {
        String s = getString(PROP_remove_precursor_range, "-1.5,1.5");
        String[] split = s.split("\\s*,\\s*");
        if (split.length != 2) {
            throw new IllegalStateException(String.format("Property %s must be of form '-1.5,1.5'", PROP_remove_precursor_range));
        }
        return new double[] {Double.parseDouble(split[0]), Double.parseDouble(split[1])};
    }

    public void setRemovePrecursorRange(double[] v) {
        if (v.length != 2)
            throw new IllegalArgumentException(PROP_remove_precursor_range + " array must have length 2");
        setString(PROP_remove_precursor_range, String.format("%f,%f", v[0], v[1]));
    }

    public PrecursorMassTolUnits getPrecursorMassUnits() {
        return PrecursorMassTolUnits.fromParamsFileRepresentation(props.getProp(PROP_precursor_mass_units, "1").value);
    }

    public void setPrecursorMassUnits(PrecursorMassTolUnits u) {
        props.setProp(PROP_precursor_mass_units, Integer.toString(u.valueInParamsFile()));
    }
    
    public Double getPrecursorMassUpper() {
        Props.Prop prop = props.getProp(PROP_precursor_mass_upper);
        return prop != null ? Double.parseDouble(prop.value) : null;
    }
    
    public void setPrecursorMassUpper(Double v) {
        props.setProp(PROP_precursor_mass_upper, DF.format(v));
    }
    
    public Double getPrecursorMassLower() {
        Props.Prop prop = props.getProp(PROP_precursor_mass_lower);
        return prop != null ? Double.parseDouble(prop.value) : null;
    }
    
    public void setPrecursorMassLower(Double v) {
        props.setProp(PROP_precursor_mass_lower, DF.format(v));
    }
    
    // =======================================================================
    public MassTolUnits getPrecursorTrueUnits() {
        int v = Integer.parseInt(props.getProp(PROP_precursor_true_units, "1").value);
        for (int i = 0; i < MassTolUnits.values().length; i++) {
            MassTolUnits u = MassTolUnits.values()[i];
            if (u.valueInParamsFile() == v)
                return u;
        }
        throw new IllegalStateException("Value for MassTolUnits stored in params file for property " + PROP_precursor_true_units + 
                " does not correspond to enum values of MassTolUnits.");
    }
    
    public void setPrecursorTrueUnits(MassTolUnits u) {
        props.setProp(PROP_precursor_true_units, Integer.toString(u.valueInParamsFile()));
    }
    
    public double getPrecursorTrueTolerance() {
        return Double.parseDouble(props.getProp(PROP_precursor_true_tolerance, "20.0").value);
    }
    
    public void setPrecursorTrueTolerance(double v) {
        props.setProp(PROP_precursor_true_tolerance, DF.format(v));
    }
    
    
    // =======================================================================
    public MassTolUnits getFragmentMassUnits() {
        int v = Integer.parseInt(props.getProp(PROP_fragment_mass_units, "1").value);
        for (int i = 0; i < MassTolUnits.values().length; i++) {
            MassTolUnits u = MassTolUnits.values()[i];
            if (u.valueInParamsFile() == v)
                return u;
        }
        throw new IllegalStateException("Value for MassTolUnits stored in params file for property " + PROP_fragment_mass_units + 
                " does not correspond to enum values of MassTolUnits.");
    }
    
    public void setFragmentMassUnits(MassTolUnits u) {
        props.setProp(PROP_fragment_mass_units, Integer.toString(u.valueInParamsFile()));
    }
    
    public double getFragmentMassTolerance() {
        return Double.parseDouble(props.getProp(PROP_fragment_mass_tolerance, "20.0").value);
    }
    
    public void setFragmentMassTolerance(double v) {
        props.setProp(PROP_fragment_mass_tolerance, DF.format(v));
    }

    public int getCalibrateMass() {
        return Integer.parseInt(props.getProp(PROP_calibrate_mass, "0").value);
    }

    public void setCalibrateMass(int v) {
        props.setProp(PROP_calibrate_mass, Integer.toString(v));
    }

    // =======================================================================
    
    public String getIsotopeError() {
        return props.getProp(PROP_isotope_error, "-1/0/1/2").value;
    }
    
    public void setIsotopeError(String v) {
        props.setProp(PROP_isotope_error, v);
    }
    
    public String getMassOffsets() {
        return props.getProp(PROP_mass_offsets, "0").value;
    }
    
    public void setMassOffsets(String v) {
        props.setProp(PROP_mass_offsets, v);
    }

    public void setPrecursorMassMode(FraggerPrecursorMassMode v) {
        props.setProp(PROP_precursor_mass_mode, v.name());
    }

    public FraggerPrecursorMassMode getPrecursorMassMode() {
        Prop v = props
            .getProp(PROP_precursor_mass_mode, FraggerPrecursorMassMode.isolated.name());
        return FraggerPrecursorMassMode.valueOf(v.value);
    }
    
    public CleavageType getNumEnzymeTermini() {
        int v = Integer.parseInt(props.getProp(PROP_num_enzyme_termini, "2").value);
        for (CleavageType ct : CleavageType.values())
            if (ct.valueInParamsFile() == v)
                return ct;
        throw new IllegalStateException("Unknown cleavage type found in properties.");
    }
    
    public void setNumEnzymeTermini(CleavageType ct) {
        props.setProp(PROP_num_enzyme_termini, Integer.toString(ct.valueInParamsFile()));
    }
    
    public boolean getClipNTermM() {
        int v = Integer.parseInt(props.getProp(PROP_clip_nTerm_M, "1").value);
        return v == 1;
    }
    
    public void setClipNTermM(boolean v) {
        int vInt = v ? 1 : 0;
        props.setProp(PROP_clip_nTerm_M, Integer.toString(vInt));
    }
    
    public FraggerOutputType getOutputFormat() {
        String val = props.getProp(PROP_output_format, "pepXML_pin").value;
        for (FraggerOutputType t : FraggerOutputType.values()) {
            if (t.valueInParamsFile().equalsIgnoreCase(val))
                return t;
        }
        throw new IllegalStateException("Unknown output format stored in properties (property '" + PROP_output_format + "')");
    }
    
    public void setOutputFormat(FraggerOutputType type) {
        props.setProp(PROP_output_format, type.valueInParamsFile());
    }
    
    public int getOutputReportTopN() {
        return Integer.parseInt(props.getProp(PROP_output_report_topN, "1").value);
    }

    public void setDataType(int dataType) {
        props.setProp(PROP_data_type, Integer.toString(dataType));
    }
    
    public void setOutputReportTopN(int v) {
        props.setProp(PROP_output_report_topN, Integer.toString(v));
    }

    public boolean getReportAlternativeProteins() {
        int v = Integer.parseInt(props.getProp(PROP_report_alternative_proteins, "1").value);
        return v == 1;
    }

    public void setReportAlternativeProteins(boolean v) {
        int vInt = v ? 1 : 0;
        props.setProp(PROP_report_alternative_proteins, Integer.toString(vInt));
    }

    public double getOutputMaxExpect() {
        return Double.parseDouble(props.getProp(PROP_output_max_expect, "50").value);
    }
    
    public void setOutputMaxExpect(double v) {
        props.setProp(PROP_output_max_expect, Double.toString(v));
    }
    
    public int[] getPrecursorCharge() {
        String str = props.getProp(PROP_precursor_charge, "0 0").value;
        String[] split = str.split("\\s+");
        if (split.length != 2)
            throw new IllegalStateException(String.format(
                    "The string parsed from properties could not be interpreted as two integers (property '%s')", PROP_precursor_charge));
        
        
        int z0 = Integer.parseInt(split[0]);
        int z1 = Integer.parseInt(split[1]);
        try {
            return new int[] {z0, z1};
        } catch (NumberFormatException nfe) {
            throw new IllegalStateException(String.format(
                    "The string parsed from properties could not be interpreted as two integers (property '%s')", PROP_precursor_charge), nfe);
        }
    }
    
    public void setPrecursorCharge(int[] v) {
        if (v.length != 2)
            throw new IllegalArgumentException("Array length must be 2");
        props.setProp(PROP_precursor_charge, Integer.toString(v[0]) + " " + Integer.toString(v[1]));
    }
    
    public boolean getOverrideCharge() {
        int v = Integer.parseInt(props.getProp(PROP_override_charge, "0").value);
        return v == 1;
    }
    
    public void setOverrideCharge(boolean v) {
        int vInt = v ? 1 : 0;
        props.setProp(PROP_override_charge, Integer.toString(vInt));
    }
    
    public int getDigestMinLength() {
        return Integer.parseInt(props.getProp(PROP_digest_min_length, "5").value);
    }
    
    public void setDigestMinLength(int v) {
        props.setProp(PROP_digest_min_length, Integer.toString(v));
    }
    
    public int getDigestMaxLength() {
        return Integer.parseInt(props.getProp(PROP_digest_max_length, "5").value);
    }
    
    public void setDigestMaxLength(int v) {
        props.setProp(PROP_digest_max_length, Integer.toString(v));
    }
    
    public double[] getDigestMassRange() {
        String str = props.getProp(PROP_digest_mass_range, "500.0 7000.0").value;
        String[] split = str.split("\\s+");
        if (split.length != 2)
            throw new IllegalStateException(String.format(
                    "The string parsed from properties could not be interpreted as two doubles (property '%s')", PROP_digest_mass_range));
        
        
        double m0 = Double.parseDouble(split[0]);
        double m1 = Double.parseDouble(split[1]);
        try {
            return new double[] {m0, m1};
        } catch (NumberFormatException nfe) {
            throw new IllegalStateException(String.format(
                    "The string parsed from properties could not be interpreted as two doubles (property '%s')", PROP_digest_mass_range), nfe);
        }
    }
    
    public void setDigestMassRange(double[] v) {
        if (v.length != 2)
            throw new IllegalArgumentException("Array length must be 2 for property '" + PROP_digest_mass_range + "'");
        props.setProp(PROP_digest_mass_range, Double.toString(v[0]) + " " + Double.toString(v[1]));
    }
    
    public int getMaxFragmentCharge() {
        return Integer.parseInt(props.getProp(PROP_max_fragment_charge, "2").value);
    }
    
    public void setMaxFragmentCharge(int v) {
        props.setProp(PROP_max_fragment_charge, Integer.toString(v));
    }

    public String getFragmentIonSeries() {
        return props.getProp(PROP_fragment_ion_series, "b,y").value;
    }

    public void setFragmentIonSeries(String v) {
        props.setProp(PROP_fragment_ion_series, v);
    }

    public String getIonSeriesDefinitions() {
        return props.getProp(PROP_ion_series_definitions, "").value;
    }

    public void setIonSeriesDefinitions(String v) {
        props.setProp(PROP_ion_series_definitions, v);
    }

    public int getTrackZeroTopN() {
        int v = Integer.parseInt(props.getProp(PROP_track_zero_topN, "0").value);
        return v;
    }
    
    public void setTrackZeroTopN(int v) {
        props.setProp(PROP_track_zero_topN, Integer.toString(v));
    }
    
    public double getZeroBinAcceptExpect() {
        double v = Double.parseDouble(props.getProp(PROP_zero_bin_accept_expect, "0.0").value);
        return v;
    }
    
    public void setZeroBinAcceptExpect(double v) {
        props.setProp(PROP_zero_bin_accept_expect, DF.format(v));
    }
    
    public double getZeroBinMultExpect() {
        double v = Double.parseDouble(props.getProp(PROP_zero_bin_mult_expect, "1.0").value);
        return v;
    }
    
    public void setZeroBinMultExpect(double v) {
        props.setProp(PROP_zero_bin_mult_expect, DF.format(v));
    }
    
    public int getMinimumPeaks() {
        return Integer.parseInt(props.getProp(PROP_minimum_peaks, "6").value);
    }
    
    public void setMinimumPeaks(int v) {
        props.setProp(PROP_minimum_peaks, Integer.toString(v));
    }
    
    public int getUseTopNPeaks() {
        return Integer.parseInt(props.getProp(PROP_use_topN_peaks, "150").value);
    }
    
    public void setUseTopNPeaks(int v) {
        props.setProp(PROP_use_topN_peaks, Integer.toString(v));
    }
    
    public int getMinFragmentsModelling() {
        return Integer.parseInt(props.getProp(PROP_min_fragments_modelling, "2").value);
    }
    
    public void setMinFragmentsModelling(int v) {
        props.setProp(PROP_min_fragments_modelling, Integer.toString(v));
    }
    
    public int getMinMatchedFragments() {
        return Integer.parseInt(props.getProp(PROP_min_matched_fragments, "4").value);
    }
    
    public void setMinMatchedFragments(int v) {
        props.setProp(PROP_min_matched_fragments, Integer.toString(v));
    }
    
    public double getMinimumRatio() {
        return Double.parseDouble(props.getProp(PROP_minimum_ratio, "0.01").value);
    }
    
    public void setMinimumRatio(double v) {
        props.setProp(PROP_minimum_ratio, Double.toString(v));
    }
    
    public double[] getClearMzRange() {
        String str = props.getProp(PROP_clear_mz_range, "0.0 0.0").value;
        String[] split = str.split("\\s+");
        if (split.length != 2)
            throw new IllegalStateException(String.format(
                    "The string parsed from properties could not be interpreted as two doubles (property '%s')", PROP_clear_mz_range));
        
        
        double m0 = Double.parseDouble(split[0]);
        double m1 = Double.parseDouble(split[1]);
        try {
            return new double[] {m0, m1};
        } catch (NumberFormatException nfe) {
            throw new IllegalStateException(String.format(
                    "The string parsed from properties could not be interpreted as two doubles (property '%s')", PROP_clear_mz_range), nfe);
        }
    }
    
    public void setClearMzRange(double[] v) {
        if (v == null || v.length != 2)
            throw new IllegalArgumentException("Array length must be 2");
        props.setProp(PROP_clear_mz_range, v[0] + " " + v[1]);
    }

    public double[] getShiftedIonsExcludeRanges() {
        final String name = PROP_delta_mass_exclude_ranges;
        final String val = props.getProp(name, "(-1.5,3.5)").value;
        Matcher m = reShiftedIonsExclusionRange.matcher(val);
        if (!m.find()) {
            throw new IllegalStateException(String.format(
                "Property named '%s' with value '%s' does not match its regex '%s'", name, val, reShiftedIonsExclusionRange.pattern()));
        }
        final double[] out = new double[2];
        out[0] = Double.parseDouble(m.group("v1"));
        out[1] = Double.parseDouble(m.group("v2"));
        return out;
    }

    public void setShiftedIonsExcludeRanges(double[] v) {
        if (v == null || v.length != 2) {
            throw new IllegalArgumentException("Array length must be 2");
        }
        props.setProp(PROP_delta_mass_exclude_ranges, "(" + v[0] + "," + v[1] + ")");
    }

    public int getIntensityTransform() {
        return getInt(PROP_intensity_transform, "0");
    }

    public void setIntensityTransform(int v) {
        setInt(PROP_intensity_transform, v);
    }

    public boolean getShiftedIons() {
        int v = Integer.parseInt(props.getProp(PROP_localize_delta_mass, "0").value);
        return v == 1;
    }

    public void setShiftedIons(boolean v) {
        int vInt = v ? 1 : 0;
        props.setProp(PROP_localize_delta_mass, Integer.toString(vInt));
    }
    
    public int getMaxVariableModsPerPeptide() {
        return Integer.parseInt(props.getProp(PROP_max_variable_mods_per_peptide, "3").value);
    }
    
    public void setMaxVariableModsPerPeptide(int v) {
        props.setProp(PROP_max_variable_mods_per_peptide, Integer.toString(v));
    }
    
    public int getMaxVariableModsCombinations() {
        return Integer.parseInt(props.getProp(PROP_max_variable_mods_combinations, "100").value);
    }
    
    public void setMaxVariableModsCombinations(int v) {
        props.setProp(PROP_max_variable_mods_combinations, Integer.toString(v));
    }
    
    public List<Mod> getVariableMods() {
        ArrayList<Mod> mods = new ArrayList<>(VAR_MOD_COUNT_MAX);
        boolean haveShownOldParamsFileWarning = false;
        for (int i = 0; i < VAR_MOD_COUNT_MAX; i++) {
            String name = String.format(Locale.ROOT, "%s_%02d", PROP_variable_mod, i+1);
            Props.Prop p = props.getProp(name);
            if (p == null)
                continue;
            String[] split = p.value.split("\\s+");
            for (int j = 0; j < split.length; j++)
                split[j] = split[j].trim();
            if (split.length == 2) {
                // possibly old parameter file
                if (!haveShownOldParamsFileWarning) {
                    SwingUtils.showDialog(null, new JLabel(
                            "<html>" +
                                    "Looks like you might be loading a parameter file from older version of MSFragger.<br/>\n" +
                                    "Variable mods are now expected to have 3 values per mod: sites, mass and max<br/>\n" +
                                    "number of occurrences.<br/><br/>\n" +
                                    "We will still try to parse the file if possible, and set the max number of occurrences<br/>\n" +
                                    "to current defaults. But you are encouraged to load new defaults instead."));
                    haveShownOldParamsFileWarning = true;
                }
            } else if (split.length != 3) {
                throw new IllegalStateException(String.format(
                        "Can't interpret variable mod from properties as delta mass and sites.\n"
                                + "Splitting by '\\s+' regex resulted not in 3 columns, as expected.\n"
                                + "Variable mod string was: \"%s\"", p.value));
            }
            
            float dm;
            try {
                dm = Float.parseFloat(split[0]);
            } catch (NumberFormatException nfe) {
                throw new IllegalStateException(String.format(
                        "Can't interpret variable mod from properties as delta mass and sites.\n"
                        + "Could not parse the first column as a Double.\n"
                        + "Variable mod string was: \"%s\"", p.value));
            }
            final String sites = split[1];
            if (StringUtils.isNullOrWhitespace(sites))
                throw new IllegalStateException(String.format(
                        "Can't interpret variable mod from properties as delta mass and sites.\n"
                        + "The second column was null or whitespace.\n"
                        + "Variable mod string was: \"%s\"", p.value));
            int maxOccurrences;
            try {
                if (split.length <= 2) {
                    switch (sites) {
                        case "M":
                        case "STY":
                            maxOccurrences = 3;
                            break;
                        default:
                            maxOccurrences = 1;
                            break;
                    }
                } else {
                    maxOccurrences = Integer.parseInt(split[2]);
                }
            } catch (Exception e) {
                throw new IllegalStateException("Could not parse max occurrences column for a variable mod in msfragger.");
            }

            mods.add(new Mod(dm, sites, p.isEnabled, maxOccurrences));
        }
        
        return mods;
    }
    
    public void setVariableMods(List<Mod> mods) {
        for (int i = 0; i < mods.size(); i++) {
            Mod vm = mods.get(i);
            String name = String.format(Locale.ROOT, "%s_%02d", PROP_variable_mod, i+1);
            if (vm.maxOccurrences > 5) {
                log.warn("Var mod max occurrences was {}, 5 is max allowed, limiting to 5 for sites: {}, dm: {}",
                        vm.maxOccurrences, vm.sites, vm.massDelta);
            }

            // MSFragger parses it as float. To make is consistent (downstream tools will parse the same value) converting it to float before writing.
            // Must concatenate the strings. Using the String.format() will write additional decimals for float, which causes issues.
            String value = vm.massDelta + " " + vm.sites + " " + vm.maxOccurrences;
            props.setProp(name, value, vm.isEnabled);
        }
    }
    
    public List<Mod> getFixedMods() {
        ArrayList<Mod> mods = new ArrayList<>(ADDON_NAMES.length);
        for (int i = 0; i < ADDON_NAMES.length; i++) {
            String siteName = ADDON_NAMES[i];
            String name = String.format(Locale.ROOT, "%s_%s", PROP_add, siteName);
            Props.Prop p = props.getProp(name);
            if (p == null)
                continue;
            float dm = Float.parseFloat(p.value);
            String sites = ADDON_MAP_NAME2HUMAN.get(siteName);
            if (sites == null)
                throw new IllegalStateException("Could not map addon modification site name to a human readable name.");
            mods.add(new Mod(dm, sites, p.isEnabled, 1));
        }
        
        return mods;
    }
    
    public void setFixedMods(List<Mod> mods) {
        for (int i = 0; i < mods.size(); i++) {
            Mod vm = mods.get(i);
            String siteName = ADDON_MAP_HUMAN2NAME.get(vm.sites);
            if (siteName == null)
                throw new IllegalStateException("Could not map human readable addon modification name to name in properties.");
            String name = String.format(Locale.ROOT, "%s_%s", PROP_add, siteName);

            // MSFragger parses it as float. To make is consistent (downstream tools will parse the same value) converting it to float before writing.
            // Must concatenate the strings. Using the String.format() will write additional decimals for float, which causes issues.
            String value = String.valueOf(vm.massDelta);
            props.setProp(name, value, vm.isEnabled);
        }
    }
}
