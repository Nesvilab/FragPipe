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

import java.io.*;
import java.util.*;

public class GlycoMassLoader {

    private static final String MASSES_FILE = "glycan_masses.txt";


    private static final HashMap<String, Double> glycanMasses;
    static
    {
        glycanMasses = new HashMap<>();
        try {
            BufferedReader reader = new BufferedReader(new InputStreamReader(GlycoMassLoader.class.getResourceAsStream(MASSES_FILE)));
            String readline;
            while ((readline = reader.readLine()) != null) {
                String[] splits = readline.split("=");
                glycanMasses.put(splits[0].trim().toLowerCase(), Double.parseDouble(splits[1].trim()));
            }
        } catch (IOException | NullPointerException e) {
            throw new IllegalStateException("Error reading internal glycan masses file");
        }
    }

    public static ArrayList<Double> loadByonicFile(String filePath) {
        ArrayList<Double> massOffsets = new ArrayList<>();
        try {
            BufferedReader reader = new BufferedReader(new FileReader(filePath));
            String readline;
            while ((readline = reader.readLine()) != null) {
                // skip headers
                if (readline.startsWith("#")) {
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
            System.out.printf("Invalid glycan %s is not in the internal database. Please add its mass to the database file and retry.\n", glycanResidue);
        }
        return mass;
    }

}
