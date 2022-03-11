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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with FragPipe.  If not, see <https://www.gnu.org/licenses/>.
 */
package com.dmtavt.fragpipe.tools.comet;

import com.dmtavt.fragpipe.exceptions.ParsingException;
import com.dmtavt.fragpipe.params.AbstractParams;
import com.dmtavt.fragpipe.params.PropLine;
import com.dmtavt.fragpipe.params.PropertyFileContent;
import com.dmtavt.fragpipe.params.Props;
import com.dmtavt.fragpipe.tools.fragger.Mod;
import com.github.chhh.utils.StringUtils;
import com.github.chhh.utils.SwingUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.*;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.Charset;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Created by Dmitry Avtonomov on 2016-04-27.
 */
public class CometParams extends AbstractParams implements PropertyFileContent {
    private static final Logger log = LoggerFactory.getLogger(CometParams.class);

    public static final String COMET_ENZYME_INFO = "[COMET_ENZYME_INFO]";
    public static final String CACHE_FILE = "comet.params";
    public static int VAR_MOD_COUNT_MAX = 9;
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
    protected Map<Integer, String> cometEnzymeInfos = new TreeMap<>();
    protected String firstLine;
    protected Props props = new Props();
    protected List<String> linesInOriginalFile = new ArrayList<>();
    protected Map<Integer, PropLine> mapLines= new TreeMap<>();
    protected Map<String, Integer> mapProps = new HashMap<>();

    public static final String FILE_BASE_NAME = "comet";
    public static final String FILE_BASE_EXT = "params";

//    protected String binPhilosopher;

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

    /** Followed by '0N' (a 2 digit number with leading zero), max 9 mods. */
    public static final String PROP_variable_mod = "variable_mod";
    public static final String PROP_add = "add";

    public static final String PROP_database_name = "database_name";
    public static final String PROP_decoy_search = "decoy_search";
    public static final String PROP_peff_format = "peff_format";
    public static final String PROP_peff_obo = "peff_obo";

    public static final String PROP_num_threads = "num_threads";

    public static final String PROP_peptide_mass_tolerance = "peptide_mass_tolerance";
    public static final String PROP_peptide_mass_units = "peptide_mass_units";
    public static final String PROP_mass_type_parent = "mass_type_parent";
    public static final String PROP_mass_type_fragment = "mass_type_fragment";
    public static final String PROP_precursor_tolerance_type = "precursor_tolerance_type";
    public static final String PROP_isotope_error = "isotope_error";

    public static final String PROP_search_enzyme_number = "search_enzyme_number";
    public static final String PROP_search_enzyme_number_NAME = "search_enzyme_number.name";
    public static final String PROP_search_enzyme2_number = "search_enzyme2_number";
    public static final String PROP_search_enzyme2_number_NAME = "search_enzyme2_number.name";
    public static final String PROP_num_enzyme_termini = "num_enzyme_termini";
    public static final String PROP_allowed_missed_cleavage = "allowed_missed_cleavage";

    public static final String PROP_variable_mod01 = "variable_mod01";
    public static final String PROP_variable_mod02 = "variable_mod02";
    public static final String PROP_variable_mod03 = "variable_mod03";
    public static final String PROP_variable_mod04 = "variable_mod04";
    public static final String PROP_variable_mod05 = "variable_mod05";
    public static final String PROP_variable_mod06 = "variable_mod06";
    public static final String PROP_variable_mod07 = "variable_mod07";
    public static final String PROP_variable_mod08 = "variable_mod08";
    public static final String PROP_variable_mod09 = "variable_mod09";
    public static final String PROP_max_variable_mods_in_peptide = "max_variable_mods_in_peptide";
    public static final String PROP_require_variable_mod = "require_variable_mod";

    public static final String PROP_fragment_bin_tol = "fragment_bin_tol";
    public static final String PROP_fragment_bin_offset = "fragment_bin_offset";
    public static final String PROP_theoretical_fragment_ions = "theoretical_fragment_ions";
    public static final String PROP_use_A_ions = "use_A_ions";
    public static final String PROP_use_B_ions = "use_B_ions";
    public static final String PROP_use_C_ions = "use_C_ions";
    public static final String PROP_use_X_ions = "use_X_ions";
    public static final String PROP_use_Y_ions = "use_Y_ions";
    public static final String PROP_use_Z_ions = "use_Z_ions";
    public static final String PROP_use_Z1_ions = "use_Z1_ions";
    public static final String PROP_use_NL_ions = "use_NL_ions";

    public static final String PROP_output_sqtfile = "output_sqtfile";
    public static final String PROP_output_txtfile = "output_txtfile";
    public static final String PROP_output_pepxmlfile = "output_pepxmlfile";
    public static final String PROP_output_mzidentmlfile = "output_mzidentmlfile";
    public static final String PROP_output_percolatorfile = "output_percolatorfile";
    public static final String PROP_print_expect_score = "print_expect_score";
    public static final String PROP_num_output_lines = "num_output_lines";

    public static final String PROP_sample_enzyme_number = "sample_enzyme_number";

    public static final String PROP_scan_range = "scan_range";
    public static final String PROP_precursor_charge = "precursor_charge";
    public static final String PROP_override_charge = "override_charge";
    public static final String PROP_ms_level = "ms_level";
    public static final String PROP_activation_method = "activation_method";

    public static final String PROP_digest_mass_range = "digest_mass_range";
    public static final String PROP_peptide_length_range = "peptide_length_range";
    public static final String PROP_num_results = "num_results";
    public static final String PROP_max_duplicate_proteins = "max_duplicate_proteins";
    public static final String PROP_max_fragment_charge = "max_fragment_charge";
    public static final String PROP_max_precursor_charge = "max_precursor_charge";
    public static final String PROP_nucleotide_reading_frame = "nucleotide_reading_frame";
    public static final String PROP_clip_nterm_methionine = "clip_nterm_methionine";
    public static final String PROP_spectrum_batch_size = "spectrum_batch_size";
    public static final String PROP_decoy_prefix = "decoy_prefix";
    public static final String PROP_equal_I_and_L = "equal_I_and_L";
    public static final String PROP_output_suffix = "output_suffix";
    public static final String PROP_mass_offsets = "mass_offsets";
    public static final String PROP_precursor_NL_ions = "precursor_NL_ions";

    public static final String PROP_minimum_peaks = "minimum_peaks";
    public static final String PROP_minimum_intensity = "minimum_intensity";
    public static final String PROP_remove_precursor_peak = "remove_precursor_peak";
    public static final String PROP_remove_precursor_tolerance = "remove_precursor_tolerance";
    public static final String PROP_clear_mz_range = "clear_mz_range";

    public static final String PROP_add_Cterm_peptide = "add_Cterm_peptide";
    public static final String PROP_add_Nterm_peptide = "add_Nterm_peptide";
    public static final String PROP_add_Cterm_protein = "add_Cterm_protein";
    public static final String PROP_add_Nterm_protein = "add_Nterm_protein";

    public static final String PROP_add_G_glycine = "add_G_glycine";
    public static final String PROP_add_A_alanine = "add_A_alanine";
    public static final String PROP_add_S_serine = "add_S_serine";
    public static final String PROP_add_P_proline = "add_P_proline";
    public static final String PROP_add_V_valine = "add_V_valine";
    public static final String PROP_add_T_threonine = "add_T_threonine";
    public static final String PROP_add_C_cysteine = "add_C_cysteine";
    public static final String PROP_add_L_leucine = "add_L_leucine";
    public static final String PROP_add_I_isoleucine = "add_I_isoleucine";
    public static final String PROP_add_N_asparagine = "add_N_asparagine";
    public static final String PROP_add_D_aspartic_acid = "add_D_aspartic_acid";
    public static final String PROP_add_Q_glutamine = "add_Q_glutamine";
    public static final String PROP_add_K_lysine = "add_K_lysine";
    public static final String PROP_add_E_glutamic_acid = "add_E_glutamic_acid";
    public static final String PROP_add_M_methionine = "add_M_methionine";
    public static final String PROP_add_H_histidine = "add_H_histidine";
    public static final String PROP_add_F_phenylalanine = "add_F_phenylalanine";
    public static final String PROP_add_U_selenocysteine = "add_U_selenocysteine";
    public static final String PROP_add_R_arginine = "add_R_arginine";
    public static final String PROP_add_Y_tyrosine = "add_Y_tyrosine";
    public static final String PROP_add_W_tryptophan = "add_W_tryptophan";
    public static final String PROP_add_O_pyrrolysine = "add_O_pyrrolysine";
    public static final String PROP_add_B_user_amino_acid = "add_B_user_amino_acid";
    public static final String PROP_add_J_user_amino_acid = "add_J_user_amino_acid";
    public static final String PROP_add_X_user_amino_acid = "add_X_user_amino_acid";
    public static final String PROP_add_Z_user_amino_acid = "add_Z_user_amino_acid";

    public static final String[] PROP_NAMES = {
            PROP_database_name,
            PROP_decoy_search,
            PROP_peff_format,
            PROP_peff_obo,

            PROP_num_threads,

            PROP_peptide_mass_tolerance,
            PROP_peptide_mass_units,
            PROP_mass_type_parent,
            PROP_mass_type_fragment,
            PROP_precursor_tolerance_type,
            PROP_isotope_error,

            PROP_search_enzyme_number,
            PROP_search_enzyme2_number,
            PROP_num_enzyme_termini,
            PROP_allowed_missed_cleavage,

            PROP_variable_mod01,
            PROP_variable_mod02,
            PROP_variable_mod03,
            PROP_variable_mod04,
            PROP_variable_mod05,
            PROP_variable_mod06,
            PROP_variable_mod07,
            PROP_variable_mod08,
            PROP_variable_mod09,
            PROP_max_variable_mods_in_peptide,
            PROP_require_variable_mod,

            PROP_fragment_bin_tol,
            PROP_fragment_bin_offset,
            PROP_theoretical_fragment_ions,
            PROP_use_A_ions,
            PROP_use_B_ions,
            PROP_use_C_ions,
            PROP_use_X_ions,
            PROP_use_Y_ions,
            PROP_use_Z_ions,
            PROP_use_Z1_ions,
            PROP_use_NL_ions,

            PROP_output_sqtfile,
            PROP_output_txtfile,
            PROP_output_pepxmlfile,
            PROP_output_mzidentmlfile,
            PROP_output_percolatorfile,
            PROP_print_expect_score,
            PROP_num_output_lines,

            PROP_sample_enzyme_number,

            PROP_scan_range,
            PROP_precursor_charge,
            PROP_override_charge,
            PROP_ms_level,
            PROP_activation_method,

            PROP_digest_mass_range,
            PROP_peptide_length_range,
            PROP_num_results,
            PROP_max_duplicate_proteins,
            PROP_max_fragment_charge,
            PROP_max_precursor_charge,
            PROP_nucleotide_reading_frame,
            PROP_clip_nterm_methionine,
            PROP_spectrum_batch_size,
            PROP_decoy_prefix,
            PROP_equal_I_and_L,
            PROP_output_suffix,
            PROP_mass_offsets,
            PROP_precursor_NL_ions,

            PROP_minimum_peaks,
            PROP_minimum_intensity,
            PROP_remove_precursor_peak,
            PROP_remove_precursor_tolerance,
            PROP_clear_mz_range,

            PROP_add_Cterm_peptide,
            PROP_add_Nterm_peptide,
            PROP_add_Cterm_protein,
            PROP_add_Nterm_protein,

            PROP_add_G_glycine,
            PROP_add_A_alanine,
            PROP_add_S_serine,
            PROP_add_P_proline,
            PROP_add_V_valine,
            PROP_add_T_threonine,
            PROP_add_C_cysteine,
            PROP_add_L_leucine,
            PROP_add_I_isoleucine,
            PROP_add_N_asparagine,
            PROP_add_D_aspartic_acid,
            PROP_add_Q_glutamine,
            PROP_add_K_lysine,
            PROP_add_E_glutamic_acid,
            PROP_add_M_methionine,
            PROP_add_H_histidine,
            PROP_add_F_phenylalanine,
            PROP_add_U_selenocysteine,
            PROP_add_R_arginine,
            PROP_add_Y_tyrosine,
            PROP_add_W_tryptophan,
            PROP_add_O_pyrrolysine,
            PROP_add_B_user_amino_acid,
            PROP_add_J_user_amino_acid,
            PROP_add_X_user_amino_acid,
            PROP_add_Z_user_amino_acid,

    };

    /** This file is in the jar, use getResourceAsStream() to get it. */
    public static final String DEFAULT_FILE = "comet.params";

    @Override
    public List<String> getLinesInOriginalFile() {
        return linesInOriginalFile;
    }

    public void setLinesInOriginalFile(List<String> linesInOriginalFile) {
        this.linesInOriginalFile = linesInOriginalFile;
    }

    @Override
    public Map<Integer, PropLine> getMapLines() {
        return mapLines;
    }

    public void setMapLines(Map<Integer, PropLine> mapLines) {
        this.mapLines = mapLines;
    }

    @Override
    public Map<String, Integer> getMapProps() {
        return mapProps;
    }

    public void setMapProps(Map<String, Integer> mapProps) {
        this.mapProps = mapProps;
    }

    public Map<Integer, String> getCometEnzymeInfos() {
        return cometEnzymeInfos;
    }

    public String getFirstLine() {
        return firstLine;
    }

    @Override
    public Path tempFileName() {
        return Paths.get(CACHE_FILE);
    }

    @Override
    public void loadDefault() {
        try {
            load(CometParams.class.getResourceAsStream("comet.params.high-high"), true);
        } catch (IOException e) {
            log.error("Error loading defaults for Comet", e);
        }
    }

    @Override
    public Properties getPropertiesContent() {
        return props.asProperties();
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

    public static CometParams parseDefault() throws ParsingException {
        InputStream is = CometParams.class.getResourceAsStream(DEFAULT_FILE);
        return CometParams.parse(is);
    }

    public static CometParams parse(InputStream is) throws ParsingException {

        CometParams cometParams = new CometParams();
        Properties properties = new Properties();

        Pattern propRegex = Pattern.compile("^\\s*([^=]+?)\\s*=\\s*(.+?)\\s*", Pattern.CASE_INSENSITIVE);


        try (BufferedReader br = new BufferedReader(new InputStreamReader(is, Charset.forName("UTF-8")))) {
            int lineNum = 0;
            String line;
            while ((line = br.readLine()) != null) {
                lineNum++;
                cometParams.linesInOriginalFile.add(line);
                Properties props = new Properties();
                boolean hasComments = false;
                boolean hasProperty = false;
                int indexOfHash = line.indexOf('#');
                if (indexOfHash >= 0) {
                    hasComments = true;
                    if (indexOfHash > 2) {
                        // if it's over 2, then there is a possibility for having a
                        // property before it. A property needs at least one char for
                        // name, then '=' sign and at least one char for value
                        int indexOfEquals = line.substring(0, indexOfHash).indexOf('=');
                        if (indexOfEquals > 2) {
                            // ok, this might be a property
                            String possiblePropString = line.substring(0, indexOfHash);
                            addString(propRegex, possiblePropString, line, indexOfHash, cometParams, lineNum);
                        }
                    } else {
                        // index of hash is small and does not allow for a property, must be a simple string
                        cometParams.mapLines.put(lineNum, new PropLine(line, null, null, null));
                    }
                } else {
                    // it must be a pure property string or just a meaningless string
                    int indexOfEquals = line.indexOf('=');
                    if (indexOfEquals > 0) {
                        String possiblePropString = line;
                        addString(propRegex, possiblePropString, line, indexOfHash, cometParams, lineNum);
                    } else {
                        // it's an ordinary line
                        String simpleLine = line;
                        cometParams.mapLines.put(lineNum, new PropLine(simpleLine, null, null, null));
                    }
                }
            }

        } catch (IOException e) {
            throw new ParsingException("Error reading comet params file", e);
        }
        return cometParams;
    }

    private static void addString(Pattern propRegex, String possiblePropString, String line, int indexOfHash, CometParams cometParams, int lineNum) throws ParsingException {
        Matcher matcher = propRegex.matcher(possiblePropString);
        if (matcher.matches()) {
            String comment = null;
            if (indexOfHash >= 0)
                comment = line.substring(indexOfHash, line.length());
            String propName = matcher.group(1);
            String propVal =  matcher.group(2);
            if (propName.isEmpty())
                throw new ParsingException(String.format(Locale.ROOT, "Property on line number %d had empty name", lineNum));
            if (propVal.isEmpty())
                throw new ParsingException(String.format(Locale.ROOT, "Property on line number %d had empty value", lineNum));
            cometParams.mapLines.put(lineNum, new PropLine(null, propName, propVal, comment));
            cometParams.mapProps.put(propName, lineNum);
            cometParams.props.setProp(propName, propVal);
        } else {
            // we didn't find the property here, it's just a line
            cometParams.mapLines.put(lineNum, new PropLine(line, null, null, null));
        }
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

            double dm;
            try {
                dm = Double.parseDouble(split[0]);
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
                log.warn("Var mod max occurences was {}, 5 is max allowed, limiting to 5 for sites: {}, dm: {}",
                        vm.maxOccurrences, vm.sites, vm.massDelta);
            }
            String value = String.format(Locale.ROOT, "%f %s %d", vm.massDelta, vm.sites, vm.maxOccurrences);
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
                throw new IllegalStateException("Could not map addon modification site name to a human readable name.");
            mods.add(new Mod(dm, sites, p.isEnabled, 1));
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
            String value = String.format(Locale.ROOT, "%f", vm.massDelta);
            props.setProp(name, value, vm.isEnabled);
        }
    }
}
