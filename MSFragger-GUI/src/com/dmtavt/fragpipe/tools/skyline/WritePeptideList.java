package com.dmtavt.fragpipe.tools.skyline;

import java.io.*;
import java.nio.file.Path;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class WritePeptideList {

    private static Map<String, Integer> columns;
    private static final Pattern varModPattern = Pattern.compile("([0-9]+)([A-Z])\\(([\\d.-]+)\\)");
    private static final Pattern nTermModPattern = Pattern.compile("N-term\\(([\\d.-]+)\\)");
    private static final Pattern cTermModPattern = Pattern.compile("C-term\\(([\\d.-]+)\\)");

    public static final String COL_ASSIGNED_MODS = "Assigned Modifications";
    public static final String COL_PEPTIDE = "Peptide";
    public static final String COL_CHARGE = "Charge";
    public static final String COL_PROTEIN = "Protein";


    public Map<Float, Set<String>> writePeptideList(Set<Path> psmtsvFiles, Path outputPath) throws IOException {
        Map<String, Set<String>> proteinMap = new HashMap<>();
        Map<Float, Set<String>> additiveMods = new HashMap<>();

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
    public static String generateModifiedPeptide(String[] psmSplits, Map<String, Integer> columns, boolean addCharge, Map<Float, Set<String>> additiveMods) {
        String peptide = psmSplits[columns.get(COL_PEPTIDE)];
        String mods = psmSplits[columns.get(COL_ASSIGNED_MODS)].trim();
        Map<Integer, Float> modMap = new HashMap<>();

        Matcher m = nTermModPattern.matcher(mods);
        while (m.find()) {
            Float f = modMap.get(1);
            if (f != null) {
                f += Float.parseFloat(m.group(1));
                modMap.put(1, f);
                additiveMods.computeIfAbsent(f, k -> new HashSet<>()).add("n^");
            } else {
                modMap.put(1, Float.parseFloat(m.group(1)));
            }
        }

        m = cTermModPattern.matcher(mods);
        while (m.find()) {
            Float f = modMap.get(peptide.length());
            if (f != null) {
                f += Float.parseFloat(m.group(1));
                modMap.put(peptide.length(), f);
                additiveMods.computeIfAbsent(f, k -> new HashSet<>()).add("c^");
            } else {
                modMap.put(peptide.length(), Float.parseFloat(m.group(1)));
            }
        }

        m = varModPattern.matcher(mods);
        while (m.find()) {
            int site = Integer.parseInt(m.group(1));
            Float f = modMap.get(site);
            if (f != null) {
                f += Float.parseFloat(m.group(3));
                modMap.put(site, f);
                additiveMods.computeIfAbsent(f, k -> new HashSet<>()).add(m.group(2));
            } else {
                modMap.put(site, Float.parseFloat(m.group(3)));
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
    private static String insertMods(String peptide, Map<Integer, Float> modMap) {
        StringBuilder modifiedPeptide = new StringBuilder(peptide.length());
        char[] aas = peptide.toCharArray();
        for (int i = 0; i < aas.length; ++i) {
            Float f = modMap.get(i + 1);
            if (f != null) {
                // With more decimal digits, there will be mismatches between the floating point values from FragPipe and Skyline.
                modifiedPeptide.append(String.format("%c[%.1f]", aas[i], f));
            } else {
                modifiedPeptide.append(aas[i]);
            }
        }
        return modifiedPeptide.toString();
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
