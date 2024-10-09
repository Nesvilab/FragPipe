package com.dmtavt.fragpipe.tools.skyline;

import java.io.*;
import java.nio.file.Path;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class WritePeptideList {
    private static Map<String, Integer> columns;
    private static final Pattern sitePattern = Pattern.compile("(\\d+)\\w\\(");
    private static final Pattern massPattern = Pattern.compile("(\\([\\d.]+\\))");
    private static final Pattern AApattern = Pattern.compile("\\d?([\\w-]+)\\(");

    public static final String COL_ASSIGNED_MODS = "Assigned Modifications";
    public static final String COL_PEPTIDE = "Peptide";
    public static final String COL_CHARGE = "Charge";
    public static final String COL_PROTEIN = "Protein";


    public Map<String, Set<String>> writePeptideList(Set<Path> psmtsvFiles, Path outputPath) throws IOException {
        Map<String, Set<String>> proteinMap = new HashMap<>();
        Map<String, Set<String>> additiveMods = new HashMap<>();

        for (Path psmtsv: psmtsvFiles) {
            BufferedReader reader = new BufferedReader(new FileReader(psmtsv.toFile()));
            String header = initHeader(reader.readLine());

            String line;
            while ((line = reader.readLine()) != null) {
                String[] splits = line.split("\t");
                String protein = splits[columns.get(COL_PROTEIN)];
                String modpep = generateModifiedPeptide(splits, columns, true, additiveMods);
                if (proteinMap.containsKey(protein)) {
                    proteinMap.get(protein).add(modpep);
                } else {
                    // initialize new protein and peptide
                    Set<String> peptides = new HashSet<>();
                    peptides.add(modpep);
                    proteinMap.put(protein, peptides);
                }
            }

            reader.close();
        }

        // write output
        BufferedWriter writer = new BufferedWriter(new FileWriter(outputPath.toFile()));
        for (Map.Entry<String, Set<String>> entry : proteinMap.entrySet()) {
            writer.write(String.format(">>%s\n", entry.getKey()));
            for (String pep : entry.getValue()) {
                writer.write(pep + "\n");
            }
        }
        writer.close();
        return additiveMods;
    }

    /**
     * Generate a Skyline-compatible modified peptide entry from the information in the PSM.
     * Format is PEPT[79.9663]IDE (mods as delta mass in square brackets), optionally with charge appended as a series
     * of "+" characters.
     * @return
     */
    public static String generateModifiedPeptide(String[] psmSplits, Map<String, Integer> columns, boolean addCharge, Map<String, Set<String>> additiveMods) {
        String peptide = psmSplits[columns.get(COL_PEPTIDE)];

        String[] mods = psmSplits[columns.get(COL_ASSIGNED_MODS)].split(",");
        Map<Integer, String> modMap = new TreeMap<>();
        for (String mod : mods) {
            Matcher siteMatch = sitePattern.matcher(mod);
            int site;
            if (siteMatch.find()) {
                site = Integer.parseInt(siteMatch.group(1));
                Matcher massMatch = massPattern.matcher(mod);
                if (massMatch.find()) {
                    if (modMap.containsKey(site)) {
                        // handle multiple mods (e.g., 5C(57.0215),5C(100.00)) by adding masses together into a single mod
                        double mass = Double.parseDouble(massMatch.group(1).replace("(", "").replace(")", ""));
                        mass += Double.parseDouble(modMap.get(site).replace("[", "").replace("]", ""));
                        modMap.put(site, String.format("[%.5f]", mass));
                        // add mod to list for appending to mod.xml
                        additiveMods.computeIfAbsent(String.format("%.4f", mass), k -> new HashSet<>()).add(getSite(mod));
                    } else {
                        modMap.put(site, massMatch.group(1).replace("(", "[").replace(")", "]"));
                    }
                }
            }
        }
        String modPep = insertMods(peptide, modMap);
        int charge = Integer.parseInt(psmSplits[columns.get(COL_CHARGE)]);
        String chargeStr = addCharge ? "+".repeat(charge) : "";
        return String.format("%s%s", modPep, chargeStr);
    }

    /**
     * Generate a modified peptide String with all Assigned modifications placed within it
     */
    private static String insertMods(String peptide, Map<Integer, String> modMap) {
        StringBuilder modifiedPeptide = new StringBuilder(peptide);

        // Offset to account for insertions
        int offset = 0;

        // Iterate through the sorted entries and insert the mods
        for (Map.Entry<Integer, String> entry : modMap.entrySet()) {
            int position = entry.getKey() + offset;
            String mod = entry.getValue();

            if (position >= 0 && position <= modifiedPeptide.length()) {
                modifiedPeptide.insert(position, mod);
                offset += mod.length();
            }
        }
        return modifiedPeptide.toString();
    }

    // Return the AA (or terminus) of a given Assigned Mod
    private static String getSite(String mod) {
        Matcher m = AApattern.matcher(mod);
        if (m.find()) {
            return m.group(1);
        }
        return "";
    }

    private String initHeader(String header) {
        columns = new HashMap<>();
        String[] splits = header.split("\t");
        for (int i = 0 ; i < splits.length ; i++) {
            columns.put(splits[i], i);
        }
        return header;
    }
}
