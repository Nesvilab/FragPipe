/* 
 * Copyright (C) 2018 Dmitry Avtonomov
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package umich.msfragger.params.fragger;

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import umich.msfragger.params.Props;
import umich.msfragger.params.enums.CleavageType;
import umich.msfragger.params.enums.FraggerOutputType;
import umich.msfragger.params.enums.MassTolUnits;
import umich.msfragger.util.PathUtils;
import umich.msfragger.util.StringUtils;

/**
 *
 * @author dmitriya
 */
public class MsfraggerParams {
    private Props props;
    
    public static final String PROP_database_name = "database_name";
    public static final String PROP_num_threads = "num_threads";
    public static final String PROP_precursor_mass_lower = "precursor_mass_lower";
    public static final String PROP_precursor_mass_upper = "precursor_mass_upper";
    public static final String PROP_precursor_mass_tolerance = "precursor_mass_tolerance";
    public static final String PROP_precursor_mass_units = "precursor_mass_units";
    public static final String PROP_precursor_true_tolerance = "precursor_true_tolerance";
    public static final String PROP_precursor_true_units = "precursor_true_units";
    public static final String PROP_fragment_mass_tolerance = "fragment_mass_tolerance";
    public static final String PROP_fragment_mass_units = "fragment_mass_units";
    public static final String PROP_isotope_error = "isotope_error";
    public static final String PROP_mass_offsets = "mass_offsets";
    public static final String PROP_search_enzyme_name = "search_enzyme_name";
    public static final String PROP_search_enzyme_cutafter = "search_enzyme_cutafter";
    public static final String PROP_search_enzyme_butnotafter = "search_enzyme_butnotafter";
    public static final String PROP_num_enzyme_termini = "num_enzyme_termini";
    public static final String PROP_allowed_missed_cleavage = "allowed_missed_cleavage";
    public static final String PROP_clip_nTerm_M = "clip_nTerm_M";
    
    /** Followed by '_N' (underscore and a number), max 7 mods. */
    public static final String PROP_variable_mod = "variable_mod";
    public static final int VAR_MOD_COUNT_MAX = 7;  
    public static final String PROP_allow_multiple_variable_mods_on_residue = "allow_multiple_variable_mods_on_residue";
    public static final String PROP_max_variable_mods_per_mod = "max_variable_mods_per_mod";
    public static final String PROP_max_variable_mods_combinations = "max_variable_mods_combinations";
    
    public static final String PROP_output_file_extension = "output_file_extension";
    public static final String PROP_output_format = "output_format";
    public static final String PROP_output_report_topN = "output_report_topN";
    public static final String PROP_output_max_expect = "output_max_expect";
    public static final String PROP_precursor_charge = "precursor_charge";
    public static final String PROP_override_charge = "override_charge";
    public static final String PROP_ms_level = "ms_level";
    public static final String PROP_digest_min_length = "digest_min_length";
    public static final String PROP_digest_max_length = "digest_max_length";
    public static final String PROP_digest_mass_range = "digest_mass_range";
    public static final String PROP_max_fragment_charge = "max_fragment_charge";
    
    
    // Open search params
    
    public static final String PROP_track_zero_topN = "track_zero_topN";
    public static final String PROP_zero_bin_accept_expect = "zero_bin_accept_expect";
    public static final String PROP_zero_bin_mult_expect = "zero_bin_mult_expect";
    public static final String PROP_add_topN_complementary = "add_topN_complementary";
    
    // Spectral processing
    
    public static final String PROP_minimum_peaks = "minimum_peaks";
    public static final String PROP_use_topN_peaks = "use_topN_peaks";
    public static final String PROP_min_fragments_modelling = "min_fragments_modelling";
    public static final String PROP_min_matched_fragments = "min_matched_fragments";
    public static final String PROP_minimum_ratio = "minimum_ratio";
    public static final String PROP_clear_mz_range = "clear_mz_range";
    public static final String PROP_add = "add";
    public static final String PROP_add_enabled = "add_enabled";
    //public static final String PROP_ = "";
    
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
    
    public static final String FILE_BASE_NAME = "fragger";
    public static final String FILE_BASE_EXT = "params";
    /** This file is in the jar, use getResourceAsStream() to get it.  */
    public static final String DEFAULT_FILE = "fragger.params";
    public static final String DEFAULT_FILE_OPENSEARCH = "fragger_open.params";
    public static final String DEFAULT_FILE_CLOSEDSEARCH = "fragger_closed.params";
    private static final long serialVersionUID = 1L;

    private static final DecimalFormat DF = new DecimalFormat("0.##########");
    private Map<String, String> comments;
        
    public MsfraggerParams() {
        comments = new HashMap<>();
        comments.put(PROP_num_threads, "0=poll CPU to set num threads; else specify num threads directly (max 64)");
        comments.put(PROP_precursor_mass_lower, "Overrides the lower bound of the window set by precursor_mass_tolerance");
        comments.put(PROP_precursor_mass_upper, "Overrides the upper bound of the window set by precursor_mass_tolerance");
        comments.put(PROP_precursor_mass_units, "0=Daltons, 1=ppm");
        comments.put(PROP_precursor_true_units, "0=Daltons, 1=ppm");
        comments.put(PROP_fragment_mass_units, "0=Daltons, 1=ppm");
        comments.put(PROP_isotope_error, "0=off, -1/0/1/2/3 (standard C13 error)");
        comments.put(PROP_mass_offsets, "allow for additional precursor mass window shifts. Multiplexed with isotope_error. mass_offsets = 0/79.966 can be used as a restricted ‘open’ search that looks for unmodified and phosphorylated peptides (on any residue)");
        comments.put(PROP_num_enzyme_termini, "2 for enzymatic, 1 for semi-enzymatic, 0 for nonspecific digestion");
        comments.put(PROP_allowed_missed_cleavage, "maximum value is 5");
        comments.put(PROP_precursor_charge, "precursor charge range to analyze; does not override any existing charge; 0 as 1st entry ignores parameter");
        comments.put(PROP_override_charge, "0=no, 1=yes to override existing precursor charge states with precursor_charge parameter");
        comments.put(PROP_ms_level, "MS level to analyze, valid are levels 2 (default) or 3");
        comments.put(PROP_digest_mass_range, "MH+ peptide mass range to analyze");
        comments.put(PROP_max_fragment_charge, "set maximum fragment charge state to analyze (allowed max 5)");
        comments.put(PROP_track_zero_topN, "in addition to topN results, keep track of top results in zero bin");
        comments.put(PROP_zero_bin_accept_expect, "boost top zero bin entry to top if it has expect under 0.01 - set to 0 to disable");
        comments.put(PROP_zero_bin_mult_expect, "disabled if above passes - multiply expect of zero bin for ordering purposes (does not affect reported expect)");
        comments.put(PROP_minimum_peaks, "required minimum number of peaks in spectrum to search (default 10)");
        comments.put(PROP_minimum_ratio, "filter peaks below this fraction of strongest peak");
        comments.put(PROP_clear_mz_range, "for iTRAQ/TMT type data; will clear out all peaks in the specified m/z range");
        comments.put(PROP_allow_multiple_variable_mods_on_residue, "static mods are not considered");
        comments.put(PROP_max_variable_mods_per_mod, "maximum of 5");
        comments.put(PROP_max_variable_mods_combinations, "maximum of 65534, limits number of modified peptides generated from sequence");
        props = new Props(comments);        
    }
    
    
    
    public static Path tempFilePath() {
        return Paths.get(PathUtils.getTempDir().toString(), DEFAULT_FILE);
    }
    
    /**
     * Loads properties either from the default properties file stored in the jar
     * or from the temp directory.
     * @throws IOException 
     */
    public void load() throws IOException {
        // first check if there is a temp file saved
        Path tempFilePath = tempFilePath();
        if (Files.exists(tempFilePath)) {
            try (FileInputStream fis = new FileInputStream(tempFilePath.toFile())) {
                load(fis);
            }
        } else {
            loadDefaultsOpenSearch();
        }
    }
    
    public void loadDefaultsOpenSearch() {
        try {
            load(MsfraggerParams.class.getResourceAsStream(DEFAULT_FILE_OPENSEARCH));
        } catch (IOException e) {
            // this is strange, we're loading stuff from our own jar, should not happen
            throw new IllegalStateException("Could not load MSFragger defaults for Open Search from the jar itself.", e);
        }
    }
    
    public void loadDefaultsClosedSearch() {
        try {
            load(MsfraggerParams.class.getResourceAsStream(DEFAULT_FILE_CLOSEDSEARCH));
        } catch (IOException e) {
            // this is strange, we're loading stuff from our own jar, should not happen
            throw new IllegalStateException("Could not load MSFragger defaults for Closed Search from the jar itself.", e);
        }
    }
    
    public void load(InputStream is) throws IOException {
        props.load(is);
    }
    
    public void clear() {
        this.props.clearProps();
    }
    
    /**
     * Saves the current properties contents to a default temp file.
     * @throws IOException 
     */
    public Path save() throws IOException {
        Path temp = tempFilePath();
        if (Files.exists(temp)) {
            Files.delete(temp);
        }
        props.save(new FileOutputStream(temp.toFile()));
        return temp;
    }
    
    /**
     * Saves the current properties contents to a stream. With comments.
     * @param os
     * @throws IOException 
     */
    public void save(OutputStream os) throws IOException {
        props.save(os);
    }

    public Props getProps() {
        return props;
    }
    
    public String getDatabaseName() {
        return props.getProp(PROP_database_name, "").value;
    }
    
    public void setDatabaseName(String databaseName) {
        props.setProp(PROP_database_name, databaseName);
    }
    
    public int getNumThreads() {
        return Integer.parseInt(props.getProp(PROP_num_threads, "1").value);
    }
    
    public void setNumThreads(int numThreads) {
        props.setProp(PROP_num_threads, Integer.toString(numThreads));
    }
    
    
    // =======================================================================
    public MassTolUnits getPrecursorMassUnits() {
        int v = Integer.parseInt(props.getProp(PROP_precursor_mass_units, "1").value);
        for (int i = 0; i < MassTolUnits.values().length; i++) {
            MassTolUnits u = MassTolUnits.values()[i];
            if (u.valueInParamsFile() == v)
                return u;
        }
        throw new IllegalStateException("Value for MassTolUnits stored in params file for property " + PROP_precursor_mass_units + 
                " does not correspond to enum values of MassTolUnits.");
    }
    
    public void setPrecursorMassUnits(MassTolUnits u) {
        props.setProp(PROP_precursor_mass_units, Integer.toString(u.valueInParamsFile()));
    }
    
    public Double getPrecursorMassTolerance() {
        return Double.parseDouble(props.getProp(PROP_precursor_mass_tolerance, "20.0").value);
    }
    
    public void setPrecursorMassTolerance(Double v) {
        props.setProp(PROP_precursor_mass_tolerance, DF.format(v));
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
        return Double.parseDouble(props.getProp(PROP_precursor_true_tolerance, "50.0").value);
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
        return Double.parseDouble(props.getProp(PROP_fragment_mass_tolerance, "50.0").value);
    }
    
    public void setFragmentMassTolerance(double v) {
        props.setProp(PROP_fragment_mass_tolerance, DF.format(v));
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
    
    public String getSearchEnzymeName() {
        return props.getProp(PROP_search_enzyme_name, "Trypsin").value;
    }
    
    public void setSearchEnzymeName(String v) {
        props.setProp(PROP_search_enzyme_name, v);
    }
    
    public String getSearchEnzymeCutAfter() {
        return props.getProp(PROP_search_enzyme_cutafter, "KR").value;
    }
    
    public void setSearchEnzymeCutAfter(String v) {
        props.setProp(PROP_search_enzyme_cutafter, v);
    }
    
    public String getSearchEnzymeButNotAfter() {
        return props.getProp(PROP_search_enzyme_butnotafter, "P").value;
    }
    
    public void setSearchEnzymeButNotAfter(String v) {
        props.setProp(PROP_search_enzyme_butnotafter, v);
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
    
    public int getAllowedMissedCleavage() {
        return Integer.parseInt(props.getProp(PROP_allowed_missed_cleavage, "1").value);
    }
    
    public void setAllowedMissedCleavage(int v) {
        props.setProp(PROP_allowed_missed_cleavage, Integer.toString(v));
    }
    
    public boolean getClipNTermM() {
        int v = Integer.parseInt(props.getProp(PROP_clip_nTerm_M, "1").value);
        return v == 1;
    }
    
    public void setClipNTermM(boolean v) {
        int vInt = v ? 1 : 0;
        props.setProp(PROP_clip_nTerm_M, Integer.toString(vInt));
    }
    
    public String getOutputFileExtension() {
        return props.getProp(PROP_output_file_extension, "pepXML").value;
    }
    
    public void setOutputFileExtension(String v) {
        props.setProp(PROP_output_file_extension, v);
    }
    
    public FraggerOutputType getOutputFormat() {
        String val = props.getProp(PROP_output_format, "pepXML").value;
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
        return Integer.parseInt(props.getProp(PROP_output_report_topN, "3").value);
    }
    
    public void setOutputReportTopN(int v) {
        props.setProp(PROP_output_report_topN, Integer.toString(v));
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
    
//    public int getTrackZeroTopN() {
//        return Integer.parseInt(props.getProp(PROP_track_zero_topN, "0").value);
//    }
//    
//    public void setTrackZeroTopN(int v) {
//        props.setProp(PROP_track_zero_topN, Integer.toString(v));
//    }
    
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
    
    public int getAddTopNComplementary() {
        int v = Integer.parseInt(props.getProp(PROP_add_topN_complementary, "0").value);
        return v;
    }
    
    public void setAddTopNComplementary(int v) {
        props.setProp(PROP_add_topN_complementary, Integer.toString(v));
    }
    
    public int getMinimumPeaks() {
        return Integer.parseInt(props.getProp(PROP_minimum_peaks, "6").value);
    }
    
    public void setMinimumPeaks(int v) {
        props.setProp(PROP_minimum_peaks, Integer.toString(v));
    }
    
    public int getUseTopNPeaks() {
        return Integer.parseInt(props.getProp(PROP_use_topN_peaks, "100").value);
    }
    
    public void setUseTopNPeaks(int v) {
        props.setProp(PROP_use_topN_peaks, Integer.toString(v));
    }
    
    public int getMinFragmentsModelling() {
        return Integer.parseInt(props.getProp(PROP_min_fragments_modelling, "3").value);
    }
    
    public void setMinFragmentsModelling(int v) {
        props.setProp(PROP_min_fragments_modelling, Integer.toString(v));
    }
    
    public int getMinMatchedFragments() {
        return Integer.parseInt(props.getProp(PROP_min_matched_fragments, "6").value);
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
        if (v.length != 2)
            throw new IllegalArgumentException("Array length must be 2");
        props.setProp(PROP_clear_mz_range, Double.toString(v[0]) + " " + Double.toString(v[1]));
    }
    
    public boolean getAllowMultipleVariableModsOnResidue() {
        int v = Integer.parseInt(props.getProp(PROP_allow_multiple_variable_mods_on_residue, "1").value);
        return v == 1;
    }
    
    public void setAllowMultipleVariableModsOnResidue(boolean v) {
        int vInt = v ? 1 : 0;
        props.setProp(PROP_allow_multiple_variable_mods_on_residue, Integer.toString(vInt));
    }
    
    public int getMaxVariableModsPerMod() {
        return Integer.parseInt(props.getProp(PROP_max_variable_mods_per_mod, "3").value);
    }
    
    public void setMaxVariableModsPerMod(int v) {
        props.setProp(PROP_max_variable_mods_per_mod, Integer.toString(v));
    }
    
    public int getMaxVariableModsCombinations() {
        return Integer.parseInt(props.getProp(PROP_max_variable_mods_combinations, "100").value);
    }
    
    public void setMaxVariableModsCombinations(int v) {
        props.setProp(PROP_max_variable_mods_combinations, Integer.toString(v));
    }
    
    public List<Mod> getVariableMods() {
        ArrayList<Mod> mods = new ArrayList<>(VAR_MOD_COUNT_MAX);
        for (int i = 0; i < VAR_MOD_COUNT_MAX; i++) {
            String name = String.format(Locale.ROOT, "%s_%02d", PROP_variable_mod, i+1);
            Props.Prop p = props.getProp(name);
            if (p == null)
                continue;
            String[] split = p.value.split("\\s+");
            if (split.length != 2)
                throw new IllegalStateException(String.format(
                        "Can't interpret variable mod from properties as delta mass and sites.\n"
                        + "Splitting by '\\s+' regex resulted not in 2 columns, as expected.\n"
                        + "Variable mod string was: \"%s\"", p.value));
            for (int j = 0; j < split.length; j++)
                split[j] = split[j].trim();
            
            double dm;
            try {
                dm = Double.parseDouble(split[0]);
            } catch (NumberFormatException nfe) {
                throw new IllegalStateException(String.format(
                        "Can't interpret variable mod from properties as delta mass and sites.\n"
                        + "Could not parse the first column as a Double.\n"
                        + "Variable mod string was: \"%s\"", p.value));
            }
            if (StringUtils.isNullOrWhitespace(split[1]))
                throw new IllegalStateException(String.format(
                        "Can't interpret variable mod from properties as delta mass and sites.\n"
                        + "The second column was null or whitespace.\n"
                        + "Variable mod string was: \"%s\"", p.value));
            
            mods.add(new Mod(dm, split[1], p.isEnabled));
        }
        
        return mods;
    }
    
    public void setVariableMods(List<Mod> mods) {
        for (int i = 0; i < mods.size(); i++) {
            Mod vm = mods.get(i);
            String name = String.format(Locale.ROOT, "%s_%02d", PROP_variable_mod, i+1);
            String value = String.format(Locale.ROOT, "%.5f %s", vm.massDelta, vm.sites);
            props.setProp(name, value, vm.isEnabled);
        }
    }
    
    public List<Mod> getAdditionalMods() {
        ArrayList<Mod> mods = new ArrayList<>(ADDON_NAMES.length);
        for (int i = 0; i < ADDON_NAMES.length; i++) {
            String siteName = ADDON_NAMES[i];
            String name = String.format(Locale.ROOT, "%s_%s", PROP_add, siteName);
            Props.Prop p = props.getProp(name);
            if (p == null)
                continue;
            double dm = Double.parseDouble(p.value);
            String sites = ADDON_MAP_NAME2HUMAN.get(siteName);
            if (sites == null)
                throw new IllegalStateException("Could not map addon modificaiton site name to a human readable name.");
            mods.add(new Mod(dm, sites, p.isEnabled));
        }
        
        return mods;
    }
    
    public void setAdditionalMods(List<Mod> mods) {
        for (int i = 0; i < mods.size(); i++) {
            Mod vm = mods.get(i);
            String siteName = ADDON_MAP_HUMAN2NAME.get(vm.sites);
            if (siteName == null)
                throw new IllegalStateException("Could not map human readable addon modification name to name in properties.");
            String name = String.format(Locale.ROOT, "%s_%s", PROP_add, siteName);
            String value = String.format(Locale.ROOT, "%.6f", vm.massDelta);
            props.setProp(name, value, vm.isEnabled);
        }
    }
    
    
}
