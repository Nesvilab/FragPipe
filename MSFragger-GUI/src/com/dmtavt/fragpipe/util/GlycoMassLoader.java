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

package com.dmtavt.fragpipe.util;

import com.dmtavt.fragpipe.tabs.TabMsfragger;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.*;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class GlycoMassLoader {
    private static final Logger log = LoggerFactory.getLogger(TabMsfragger.class);
    private static final String MASSES_FILE = "glycan_masses.txt";
    private static final Pattern pGlycoPattern = Pattern.compile("[AGFHNXP]");
    private static final HashMap<String, String> pGlycoTokenMap;    // map pGlyco tokens to our internal Glycan strings
    static
    {
        pGlycoTokenMap = new HashMap<>();
        pGlycoTokenMap.put("A", "NeuAc");
        pGlycoTokenMap.put("G", "NeuGc");
        pGlycoTokenMap.put("F", "Fuc");
        pGlycoTokenMap.put("H", "Hex");
        pGlycoTokenMap.put("N", "HexNAc");
        pGlycoTokenMap.put("X", "Xyl");
        pGlycoTokenMap.put("P", "HexPh");
    }

    private static final HashMap<String, Double> glycanMasses;
    static
    {
        glycanMasses = new HashMap<>();
        try {
            BufferedReader reader = new BufferedReader(new InputStreamReader(Objects.requireNonNull(GlycoMassLoader.class.getResourceAsStream(MASSES_FILE))));
            String readline;
            while ((readline = reader.readLine()) != null) {
                String[] splits = readline.split("=");
                glycanMasses.put(splits[0].trim().toLowerCase(), Double.parseDouble(splits[1].trim()));
            }
        } catch (IOException | NullPointerException e) {
            throw new IllegalStateException("Error reading internal glycan masses file");
        }
    }

    // read masses only from a text file. Can have , or \t delimiter, and one or multiple entries per line.
    public static ArrayList<Double> loadTextOffsets(String filePath) {
        ArrayList<Double> massOffsets = new ArrayList<>();
        try {
            BufferedReader reader = new BufferedReader(new FileReader(filePath));
            String readline;
            while ((readline = reader.readLine()) != null) {
                readline = readline.trim();
                int t = readline.indexOf('#');
                if (t >= 0) {
                    readline = readline.substring(0, t);
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

                // parse masses
                for (String mass : lineSplits) {
                    try {
                        massOffsets.add(Double.parseDouble(mass.trim()));
                    } catch (NumberFormatException ex) {
                        log.warn(String.format("Invalid entry %s could not be parsed and will be ignored.", mass));
                    }
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        return massOffsets;
    }

    /**
     * Load pGlyco glycan database file of format (N)(H)(H) or similar. Reads only the composition, ignoring structure.
     * Removes duplicate masses to 4 decimal places
     * @param filePath
     * @return
     */
    public static ArrayList<Double> loadPGlycoFile(String filePath) {
        ArrayList<Double> massOffsets = new ArrayList<>();
        HashMap<Long, Boolean> existingMasses = new HashMap<>();

        try {
            BufferedReader reader = new BufferedReader(new FileReader(filePath));
            String readline;
            while ((readline = reader.readLine()) != null) {
                readline = readline.trim();
                int t = readline.indexOf('#');
                if (t >= 0) {
                    readline = readline.substring(0, t);
                }

                if (readline.isEmpty()) {
                    continue;
                }

                // parse glycans from tokens
                String line = readline.replace("Hp", "P");      // remove the only double character, if present
                Matcher matcher = pGlycoPattern.matcher(line);
                boolean valid = true;
                double glycanMass = 0;
                while(matcher.find()) {
                    String glycanToken = matcher.group();
                    if (pGlycoTokenMap.containsKey(glycanToken)) {
                        glycanMass += glycanMasses.get(pGlycoTokenMap.get(glycanToken).toLowerCase());
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
                        massOffsets.add(glycanMass);
                        existingMasses.put(massKey, true);
                    }
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        return massOffsets;
    }

    /**
     * Load glycans from a file containing Byonic format glycans (e.g., HexNAc(2)Hex(5) % 1216.4228)
     * @param filePath
     * @return
     */
    public static ArrayList<Double> loadByonicFile(String filePath) {
        ArrayList<Double> massOffsets = new ArrayList<>();
        try {
            BufferedReader reader = new BufferedReader(new FileReader(filePath));
            String readline;
            while ((readline = reader.readLine()) != null) {
                readline = readline.trim();
                int t = readline.indexOf('#');
                if (t >= 0) {
                    readline = readline.substring(0, t);
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
                String glycanName;
                for (String line : lineSplits) {
                    double glycanMass;
                    // handle new and old formats
                    if (line.contains("%") || line.contains("(")) {
                        if (line.contains("%")) {
                            // default database includes mass in addition to glycan name - ignore mass and only take name
                            glycanName = line.split("%")[0];
                        } else {
                            glycanName = line;
                        }
                        glycanMass = parseByonicGlycanString(glycanName);
                    } else {
                        if (line.contains("\t")) {
                            // various databases may include other info in addition to glycan name - ignore and only take name (first column)
                            glycanName = line.split("\t")[0];
                        } else if (line.contains("_")) {
                            glycanName = line;
                        } else {
                            // not a recognized format - often other info in the database
                            continue;
                        }
                        glycanMass = parseOldPTMSGlycanString(glycanName);
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
     * Parse old PTM-Shepherd format glycans (HexNAc-2_Hex-5)
     * @param glycanString string to parse
     * @return mass of glycan
     */
    public static double parseOldPTMSGlycanString(String glycanString) {
        double mass = 0;
        String[] splits = glycanString.split("_");
        // Read all residue counts into the composition container
        for (String split: splits) {
            String[] glycanSplits = split.split("-");
            mass = getResidueMass(mass, glycanSplits);
        }
        return mass;
    }

    /**
     * Parse Byonic base format (Glyc1(#)Glyc2(#) ... % Mass)
     * @param glycanString Byonic format glycan string
     * @return mass of glycan
     */
    public static double parseByonicGlycanString(String glycanString) {
        String[] massSplits = glycanString.split(" % ");
        String[] compositionSplits = massSplits[0].split("\\)");
        // Read all residue counts into the composition container
        double mass = 0;
        for (String split: compositionSplits) {
            String[] glycanSplits = split.split("\\(");
            mass = getResidueMass(mass, glycanSplits);
        }
        return mass;
    }

    private static double getResidueMass(double mass, String[] glycanSplits) {
        String glycanResidue = glycanSplits[0].trim().toLowerCase();
        if (glycanMasses.containsKey(glycanResidue)) {
            double residueMass = glycanMasses.get(glycanResidue);
            int count = Integer.parseInt(glycanSplits[1].trim());
            mass += (residueMass * count);
        } else {
            log.error(String.format("Invalid glycan %s is not in the internal database. Please add its mass to the database file and retry.", glycanResidue));
        }
        return mass;
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

}
