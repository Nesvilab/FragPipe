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

package com.dmtavt.fragpipe.util;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.FragpipeLocations;
import com.dmtavt.fragpipe.dialogs.MassOffsetLoaderPanel;
import com.dmtavt.fragpipe.tabs.TabMsfragger;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.swing.FileChooserUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.*;
import javax.swing.filechooser.FileFilter;
import javax.swing.filechooser.FileNameExtensionFilter;
import java.awt.*;
import java.io.*;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.*;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import static javax.swing.JOptionPane.OK_CANCEL_OPTION;

public class GlycoMassLoader {
    public String glycoFilePath;
    public List<Double> glycoMasses;
    public MassOffsetLoaderPanel optionsPanel;
    public ArrayList<GlycanResidue> supportedGlycans;
    private final HashMap<String, Double> glycanMasses;

    private static final Logger log = LoggerFactory.getLogger(TabMsfragger.class);
    private static final String GLYCAN_RESIDUES_NAME = "glycan_residues.txt";
    private static final String GLYCAN_MODS_NAME = "glycan_mods.txt";

    private static final Pattern numberPattern = Pattern.compile("[0-9]");
    private static final Pattern letterPattern = Pattern.compile("[a-zA-Z]+");
    private static final Pattern pGlycoPattern = Pattern.compile("[()]?[AGFHNP]+[()]");         // e.g., (N(H))
    private static final Pattern byonicPattern = Pattern.compile("[A-Za-z]+\\([0-9]+\\)");      // e.g., HexNAc(1)Hex(1)
    private static final Pattern MMGlycoPattern = Pattern.compile("[AGFHNPSYCXUM][0-9]+");      // e.g., N1H1
    private static final Pattern oldPTMSPattern = Pattern.compile("[A-Za-z]+-[0-9]+");          // e.g., HexNAc-1_Hex-1

    private static final HashMap<String, String> pGlycoTokenMap;    // map pGlyco tokens to our internal Glycan strings
    private static final HashMap<String, String> MMGlycoTokenMap;    // map MetaMorpheus kind tokens to our internal Glycan strings

    public static final String PROP_FILECHOOSER_LAST_PATH = "glycoloader.filechooser.path";

    static
    {
        pGlycoTokenMap = new HashMap<>();
        pGlycoTokenMap.put("A", "NeuAc");
        pGlycoTokenMap.put("G", "NeuGc");
        pGlycoTokenMap.put("F", "Fuc");
        pGlycoTokenMap.put("H", "Hex");
        pGlycoTokenMap.put("N", "HexNAc");
        pGlycoTokenMap.put("X", "Xyl");

        MMGlycoTokenMap = new HashMap<>();
        MMGlycoTokenMap.put("A", "NeuAc");
        MMGlycoTokenMap.put("G", "NeuGc");
        MMGlycoTokenMap.put("F", "Fuc");
        MMGlycoTokenMap.put("H", "Hex");
        MMGlycoTokenMap.put("N", "HexNAc");
        MMGlycoTokenMap.put("P", "Phospho");
        MMGlycoTokenMap.put("S", "Sulfo");
        MMGlycoTokenMap.put("Y", "Na");
        MMGlycoTokenMap.put("C", "Acetyl");
        MMGlycoTokenMap.put("X", "Xyl");
        MMGlycoTokenMap.put("M", "Formyl");
    }

    public GlycoMassLoader(boolean msfraggerOnly) {
        optionsPanel = new MassOffsetLoaderPanel(msfraggerOnly);
        glycoMasses = new ArrayList<>();

        supportedGlycans = parseGlycoResiduesDB(FragpipeLocations.get().getDirTools().resolve("Glycan_Databases").resolve(GLYCAN_RESIDUES_NAME).toString());
        supportedGlycans.addAll(parseGlycoResiduesDB(FragpipeLocations.get().getDirTools().resolve("Glycan_Databases").resolve(GLYCAN_MODS_NAME).toString()));

        glycanMasses = new HashMap<>();
        for (GlycanResidue glycan: supportedGlycans) {
            glycanMasses.put(glycan.name, glycan.mass);
        }
    }

    private ArrayList<GlycanResidue> parseGlycoResiduesDB(String glycanResiduesPath) {
        ArrayList<GlycanResidue> residues = new ArrayList<>();
        BufferedReader in;
        try {
            in = new BufferedReader(new FileReader(glycanResiduesPath));
            // header
            String line = in.readLine();
            String[] splits = line.split("\t");
            int altIndex = 4;       // default value
            for (int i=0; i < splits.length; i++) {
                if (splits[i].toLowerCase().contains("alt name") || splits[i].toLowerCase().contains("alternate name")) {
                    altIndex = i;
                    break;
                }
            }
            // parse file
            while ((line = in.readLine()) != null) {
                if (line.startsWith("#")) {
                    continue;
                }
                GlycanResidue residue = GlycanResidue.parseResidue(line, altIndex);
                residues.add(residue);
            }
        } catch (IOException ex) {
            ex.printStackTrace();
        }
        return residues;
    }

    private List<Double> loadMassesFromFile(String selectedPath) {
        DatabaseType dbType = detectDBtype(selectedPath);
        glycoMasses = loadTextOffsets(selectedPath, dbType);
        return glycoMasses;
    }

    public List<String> loadCustomOffsets(Component parent) {
        List<Double> masses = new ArrayList<>();
        List<javax.swing.filechooser.FileFilter> glycFilters = new ArrayList<>();
        FileFilter filter = new FileNameExtensionFilter("Glycan or Mod Database (.txt, .csv, .tsv, .glyc, .gdb)", "txt", "csv", "tsv", "glyc", "gdb");
        glycFilters.add(filter);
        String loc = Fragpipe.propsVarGet(PROP_FILECHOOSER_LAST_PATH);
        JFileChooser fc = FileChooserUtils.builder("Select the Glycan or other Mod List file")
                .approveButton("Select").mode(FileChooserUtils.FcMode.FILES_ONLY)
                .acceptAll(false).multi(false).filters(glycFilters)
                .paths(Stream.of(loc)).create();

        String selectedPath;
        int userSelection = fc.showSaveDialog(SwingUtils.findParentFrameForDialog(parent));
        if (JFileChooser.APPROVE_OPTION == userSelection) {
            selectedPath = fc.getSelectedFile().toString();
            Fragpipe.propsVarSet(PROP_FILECHOOSER_LAST_PATH, selectedPath);
            glycoFilePath = selectedPath;
            masses = loadMassesFromFile(selectedPath);
        }
        return loadOffsetsHelper(parent, masses);
    }

    public List<String> loadOffsetsFromFile(Component parent, File glycoFile) {
        List<Double> masses = loadMassesFromFile(glycoFile.toString());
        return loadOffsetsHelper(parent, masses);
    }

    public List<String> loadOffsetsHelper(Component parent, List<Double> masses) {
        if (masses.size() > 0) {
            // combos and filtering
            final int confirmation = SwingUtils.showConfirmDialog2(parent, optionsPanel, "Offset loading options", OK_CANCEL_OPTION);
            if (JOptionPane.OK_OPTION == confirmation) {
                // combine glycan masses if requested (e.g. O-glycans)
                if (optionsPanel.getMaxCombos() > 1) {
                    masses = generateMassCombos(masses, optionsPanel.getMaxCombos(), optionsPanel.useMassFilter(), optionsPanel.getMaxMass(), optionsPanel.getMinMass());
                } else if (optionsPanel.useMassFilter()) {
                    masses = filterMasses(masses, optionsPanel.getMinMass(), optionsPanel.getMaxMass());
                }

                // make sure 0 is included in the mass offsets list
                if (!masses.contains(0.0)) {
                    masses.add(0, 0.0);
                }

                // clean up masses before returning final strings (round off floating point errors at 12 decimal places)
                List<String> massStrings = new ArrayList<>();
                for (double mass : masses) {
                    BigDecimal decimal = new BigDecimal(mass).setScale(12, RoundingMode.HALF_EVEN).stripTrailingZeros();
                    massStrings.add(decimal.toPlainString());
                }
                return massStrings;
            } else {
                return new ArrayList<>();
            }
        } else {
            return new ArrayList<>();
        }
    }

    // Method for filtering if NOT using mass combinations
    private static List<Double> filterMasses(List<Double> inputMasses, double minMass, double maxMass) {
        List<Double> filteredMasses = new ArrayList<>();
        for (double mass : inputMasses) {
            if (mass <= maxMass && mass >= minMass) {
                filteredMasses.add(mass);
            }
        }
        return filteredMasses;
    }

    /**
     * Generate all combinations of provided masses up to the specified max number. Remove duplicates (at 4 decimal places)
     * Uses combinations with repetition since same glycan can occur multiple times on a peptide.
     * @return
     */
    private static List<Double> generateMassCombos(List<Double> masses, int maxCombos, boolean massFilter, double maxMass, double minMass) {
        Set<Long> existingMasses = new HashSet<>();
        List<Double> allMasses = new ArrayList<>();
        for (int count = 1; count <= maxCombos; count++) {
            // iterate combinations
            List<int[]> combos = GlycoMassLoader.combinationsWithRepetition(masses.size(), count);
            for (int[] combo : combos) {
                // calculate mass as the sum of the glycans at selected indices
                double comboMass = 0;
                for (int i : combo) {
                    comboMass += masses.get(i);
                }
                // check for duplicates and add if unique
                long massKey = Math.round(comboMass * 10000);
                if (!existingMasses.contains(massKey)) {
                    if (!massFilter) {
                        allMasses.add(comboMass);
                        existingMasses.add(massKey);
                    } else {
                        // filtering requested
                        if (comboMass <= maxMass && comboMass >= minMass) {
                            allMasses.add(comboMass);
                            existingMasses.add(massKey);
                        }
                    }
                }
            }
        }
        return allMasses;
    }

    /**
     * Peek at first line(s) of the database file and determine what type it is for parsing
     * @param databasePath reader
     * @return db type
     */
    public static DatabaseType detectDBtype(String databasePath) {
        try {
            BufferedReader in = new BufferedReader(new FileReader(databasePath));
            String line;
            int i = 0;
            while ((line = in.readLine()) != null) {
                if (i == 0) {
                    // headers are not required, but may be present
                    if (line.contains("#") || line.contains("\\\\") || line.contains(",") || line.startsWith("%")) {
                        i++;
                        continue;
                    }
                }
                if (line.contains("%")) {
                    line = line.split("%")[0];
                }

                // check if line matches any of the known patterns
                Matcher byonicMatcher = byonicPattern.matcher(line);
                Matcher metamMatcher = MMGlycoPattern.matcher(line);
                Matcher pGlycoMatcher = pGlycoPattern.matcher(line);
                Matcher oldPTMSMatcher = oldPTMSPattern.matcher(line);
                if (byonicMatcher.find()) {
                    return DatabaseType.byonic;
                } else if (oldPTMSMatcher.find()) {
                    return DatabaseType.oldPTMShepherd;
                } else if (metamMatcher.find()) {
                    return DatabaseType.metamorpheus;
                } else if (pGlycoMatcher.find()) {
                    return DatabaseType.pGlyco;
                }
                i++;
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        return DatabaseType.unknown;
    }

    /**
     * Load glycans from a file containing Byonic format glycans (e.g., HexNAc(2)Hex(5) % 1216.4228)
     * @param filePath
     * @return
     */
    public ArrayList<Double> loadTextOffsets(String filePath, DatabaseType dbType) {
        ArrayList<Double> massOffsets = new ArrayList<>();
        HashMap<Long, Boolean> existingMasses = new HashMap<>();
        try {
            BufferedReader reader = new BufferedReader(new FileReader(filePath));
            String readline;
            while ((readline = reader.readLine()) != null) {
                readline = readline.trim();
                if (readline.contains("#") || readline.contains("\\\\") || readline.contains(",") || readline.startsWith("%")) {
                    continue;
                }
                if (readline.isEmpty()) {
                    continue;
                }

                String[] lineSplits;
                // handle csv and tsv inputs that may contain multiple entries per line
                if (readline.contains(",")) {
                    lineSplits = readline.split(",");
                } else if (readline.contains("\t")) {
                    lineSplits = readline.split("\t");
                } else {
                    lineSplits = new String[]{readline};
                }

                // parse glycans
                for (String line : lineSplits) {
                    double glycanMass = 0;
                    switch (dbType) {
                        case byonic:
                            glycanMass = parseGlycanString(line, byonicPattern.matcher(line));
                            break;
                        case pGlyco:
                            glycanMass = parsePGlycoGlycan(line, existingMasses);
                            break;
                        case metamorpheus:
                            glycanMass = parseMMKindGlycanString(line);
                            break;
                        case oldPTMShepherd:
                            glycanMass = parseGlycanString(line, oldPTMSPattern.matcher(line));
                            break;
                    }
                    if (glycanMass == 0) {
                        continue;
                    }
                    massOffsets.add(glycanMass);
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        return massOffsets;
    }

    /**
     * Parse Byonic base format (Glyc1(#)Glyc2(#) ... % Mass)
     * @param glycanString Byonic format glycan string
     * @return mass of glycan
     */
    public double parseGlycanString(String glycanString, Matcher glycanRegexMatcher) {
        double mass = 0;
        while(glycanRegexMatcher.find()) {
            String glycanToken = glycanRegexMatcher.group();
            Matcher glycanName = letterPattern.matcher(glycanToken);
            double residueMass = 0;
            if (glycanName.find()) {
                String glycan = glycanName.group();
                residueMass = getResidueMass(glycan);
            } else {
                log.error(String.format("Warning: Invalid token %s in line %s. This line will be skipped", glycanName, glycanString));
                return 0;
            }
            Matcher glycanCount = numberPattern.matcher(glycanToken);
            if (glycanCount.find()) {
                int count = Integer.parseInt(glycanCount.group());
                mass += count * residueMass;
            } else {
                log.error(String.format("Warning: glycan count not found in glycan %s and was ignored", glycanString));
                return 0;
            }
        }
        return mass;
    }

    public double parsePGlycoGlycan(String readline, HashMap<Long, Boolean> existingMasses) {
        // parse glycans from tokens
        String line = readline.replace("Hp", "P");      // remove the only double character, if present
        Matcher matcher = pGlycoPattern.matcher(line);
        boolean valid = true;
        double glycanMass = 0;
        while(matcher.find()) {
            String glycanToken = matcher.group();
            String glycan = glycanToken.replace("(", "").replace(")", "");
            // handle double residue P = phospho+hex
            if (glycan.matches("P")) {
                glycanMass += glycanMasses.get(findResidueName("Hex").name) + glycanMasses.get(findResidueName("Phospho").name);
            } else if (pGlycoTokenMap.containsKey(glycan)) {
                glycanMass += glycanMasses.get(pGlycoTokenMap.get(glycan).toLowerCase());
            } else {
                log.warn(String.format("Invalid token %s in line %s. This line will be skipped", glycanToken, line));
                valid = false;
                break;
            }
        }
        // check for duplicates and add if unique
        if (valid) {
            long massKey = Math.round(glycanMass * 10000);
            if (!existingMasses.containsKey(massKey)) {
                existingMasses.put(massKey, true);
                return glycanMass;
            }
        }
        return 0;
    }

    /**
     * Parse MetaMorpheus "Kind" glycan strings, which are in the format N2H2A1 (for example)
     * @param glycanString MM format glycan kind string
     * @return mass of glycan
     */
    public double parseMMKindGlycanString(String glycanString) {
        Matcher matcher = MMGlycoPattern.matcher(glycanString);
        double glycanMass = 0;
        while(matcher.find()) {
            String glycanToken = matcher.group();
            String glycanType = glycanToken.substring(0, 1);
            int glycanCount = Integer.parseInt(glycanToken.substring(1));
            // handle double residue U = Hex + succinylation
            if (glycanToken.matches("U")) {
                glycanMass += glycanMasses.get(findResidueName("Hex").name) + glycanMasses.get(findResidueName("Succinyl").name);
            } else if (MMGlycoTokenMap.containsKey(glycanType)) {
                glycanMass += (glycanMasses.get(MMGlycoTokenMap.get(glycanType).toLowerCase()) * glycanCount);
            } else {
                log.warn(String.format("Invalid token %s in line %s. This line will be skipped", glycanToken, glycanString));
                glycanMass = 0;
                break;
            }
        }
        return glycanMass;
    }

    private double getResidueMass(String glycanName) {
        if (glycanMasses.containsKey(glycanName)) {
            return glycanMasses.get(glycanName);
        } else {
            GlycanResidue altName = findResidueName(glycanName);
            if (altName != null) {
                return glycanMasses.get(findResidueName(glycanName).name);
            } else {
                log.error(String.format("Invalid glycan %s is not in the internal database and was ignored. Please add its mass to the database file and retry.", glycanName));
            }
        }
        return 0;
    }

    // recursive combination generator (with repetition)
    public static List<int[]> combinationsWithRepetition(int numItems, int numChoices) {
        List<int[]> combinations = new ArrayList<>();
        combinationsHelper(combinations, new int[numChoices], 0, numItems-1, 0);
        return combinations;
    }

    // helper method for recursive combination generator
    private static void combinationsHelper(List<int[]> combinations, int[] data, int start, int end, int index) {
        if (index == data.length) {
            int[] combination = data.clone();
            combinations.add(combination);
        } else if (start <= end) {
            data[index] = start;
            combinationsHelper(combinations, data, start, end, index + 1);  // include start to allow repetition
            combinationsHelper(combinations, data, start + 1, end, index);
        }
    }

    /**
     * Search all GlycanResidue names and alternate names to try to match this input to one of them.
     * @param inputName string
     * @return
     */
    public GlycanResidue findResidueName(String inputName) {
        for (GlycanResidue residue: supportedGlycans) {
            if (residue.name.equalsIgnoreCase(inputName)) {
                return residue;
            }
        }
        // no exact match: check alternate names
        for (GlycanResidue residue: supportedGlycans) {
            for (String altName : residue.alternateNames) {
                if (altName.equalsIgnoreCase(inputName)) {
                    return residue;
                }
            }
        }
        // no match - return null and warn user
        return null;
    }

    public enum DatabaseType {
        /**
         * Possible glycan atabase types that can be parsed
         */
        pGlyco,
        byonic,
        oldPTMShepherd,
        metamorpheus,
        unknown
    }

    public static class GlycanResidue {
        // base characteristics
        public String name;
        public double mass;
        public String[] alternateNames;

        public GlycanResidue(String name, double mass, String[] alternateNames) {
            this.name = name;
            this.mass = mass;
            this.alternateNames = alternateNames;
        }

        public static GlycanResidue parseResidue(String line, int altIndex) {
            String[] splits = line.replace("\"", "").split("\t");
            double mass = Double.parseDouble(splits[1]);
            String[] altNames = splits[altIndex].split(",");
            return new GlycanResidue(splits[0],
                    mass,
                    altNames
            );
        }

    }
}
