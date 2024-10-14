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

import com.dmtavt.fragpipe.tools.tmtintegrator.QuantLabel;
import org.apache.commons.io.FileUtils;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static com.dmtavt.fragpipe.cmd.ToolingUtils.UNIMOD_OBO;
import static com.dmtavt.fragpipe.cmd.ToolingUtils.getUnimodOboPath;

public class SDRFtable {

    private ArrayList<String[]> rows;
    private ArrayList<String> header;
    private int firstEnzymeIndex;
    private int firstModIndex;
    private static final Pattern uniprotPattern = Pattern.compile("OS=([\\w\\s]+) (OX=|GN=|PE=|SV=)");
    private final String guessedOrganism;

    // column names
    private static final String COL_source = "source name";
    private static final String COL_organism = "characteristics[organism]";
    private static final String COL_strain = "characteristics[strain/breed]";
    private static final String COL_cultivar = "characteristics[ecotype/cultivar]";
    private static final String COL_ancestry = "characteristics[ancestry category]";
    private static final String COL_age = "characteristics[age]";
    private static final String COL_stage = "characteristics[developmental stage]";
    private static final String COL_sex = "characteristics[sex]";
    private static final String COL_disease = "characteristics[disease]";
    private static final String COL_part = "characteristics[organism part]";
    private static final String COL_cellType = "characteristics[cell type]";
    private static final String COL_indvidual = "characteristics[individual]";
    private static final String COL_cellLine = "characteristics[cell line]";
    private static final String COL_bioReplicate = "characteristics[biological replicate]";
    private static final String COL_technology = "technology type";
    private static final String COL_assay = "assay name";
    private static final String COL_datafile = "comment[data file]";
    private static final String COL_replicate = "comment[technical replicate]";
    private static final String COL_fraction = "comment[fraction identifier]";
    private static final String COL_label = "comment[label]";
    private static final String COL_instrument = "comment[instrument]";
    private static final String COL_enzyme = "comment[cleavage agent details]";
    private static final String COL_mods = "comment[modification parameters]";
    private static final String COL_precTol = "comment[precursor mass tolerance]";
    private static final String COL_prodTol = "comment[fragment mass tolerance]";
    private static final String emptyStr = "not available";

    // convert our names to ontology names
    // names from https://www.ebi.ac.uk/ols4/ontologies/ms/classes/http%253A%252F%252Fpurl.obolibrary.org%252Fobo%252FMS_1001045
    private final static HashMap<String, String> enzymesMap = new HashMap<>();
    static {
        enzymesMap.put("custom ", "custom");
        enzymesMap.put("null", "no cleavage");
        enzymesMap.put("trypsin", "Trypsin");
        enzymesMap.put("stricttrypsin", "Trypsin/P");
        enzymesMap.put("argc", "Arg-C");
        enzymesMap.put("aspn", "Asp-N");
        enzymesMap.put("chymotrypsin", "Chymotrypsin");
        enzymesMap.put("cnbr", "CNBr");
        enzymesMap.put("elastase", "leukocyte elastase");
        enzymesMap.put("formicacid", "Formic acid");
        enzymesMap.put("gluc", "glutamyl endopeptidase");
        enzymesMap.put("gluc_bicarb", "glutamyl endopeptidase");
        enzymesMap.put("lysc", "Lys-C");
        enzymesMap.put("lysc-p", "Lys-C/P");
        enzymesMap.put("lysn", "Lys-N");
        enzymesMap.put("lysn_promisc", "Lys-N");
        enzymesMap.put("nonspecific", "unspecific cleavage");
        enzymesMap.put("trypsin/chymotrypsin", "TrypChymo");
        enzymesMap.put("trypsin/cnbr", "Trypsin_CNBr");
        enzymesMap.put("trypsin_gluc", "Trypsin_GluC");
        enzymesMap.put("trypsin_k", "Lys-C");
        enzymesMap.put("trypsin_r", "Arg-C");
        enzymesMap.put("thermolysin", "Thermolysin");
        enzymesMap.put("nocleavage ", "no cleavage");
    }

    // init Unimod DB for mod matching
    public static ArrayList<String> unimodList;
    static {
        try {
            Path unimodPath = getUnimodOboPath(UNIMOD_OBO);
            UnimodOboReader unimodOboReader = new UnimodOboReader(unimodPath);
            unimodList = unimodOboReader.unimodDB;
        } catch (Exception ex) {
            ex.printStackTrace();
            System.exit(1);
        }
    }

    public SDRFtable(SDRFtypes type, int numEnzymes, int numMods, List<String> proteinHeaders) {
        rows = new ArrayList<>();
        header = new ArrayList<>();
        guessedOrganism = guessOrganism(proteinHeaders);

        // set up header for general sample characteristics
        initHeaderCharacteristics(type);

        // headers for lcms run/search information
        header.add(COL_datafile);
        header.add(COL_replicate);
        header.add(COL_fraction);
        header.add(COL_label);
        header.add(COL_instrument);
        header.add(COL_precTol);
        header.add(COL_prodTol);

        // support multiple enzymes and mods, each in their own column
        firstEnzymeIndex = header.size();
        for (int i=0; i < numEnzymes; i++){
            header.add(COL_enzyme);
        }
        firstModIndex = header.size();
        for (int i=0; i < numMods; i++){
            header.add(COL_mods);
        }
    }

    /**
     * Header setup for various types of SDRF files.
     * @param type type of template to generate
     */
    private void initHeaderCharacteristics(SDRFtypes type) {
        header.add(COL_source);
        header.add(COL_organism);
        switch (type) {
            case Human:
                header.add(COL_ancestry);
                header.add(COL_age);
                header.add(COL_stage);
                header.add(COL_sex);
                header.add(COL_indvidual);
                break;
            case Vertebrates:
                header.add(COL_age);
                header.add(COL_stage);
                header.add(COL_sex);
                header.add(COL_indvidual);
                break;
            case NonVertebrates:
                header.add(COL_strain);
                header.add(COL_age);
                header.add(COL_stage);
                header.add(COL_sex);
                header.add(COL_indvidual);
            case Plants:
                header.add(COL_cultivar);
                header.add(COL_age);
                header.add(COL_stage);
                header.add(COL_indvidual);
            case CellLines:
                header.add(COL_cellLine);
        }
        header.add(COL_disease);
        header.add(COL_part);
        header.add(COL_cellType);
        header.add(COL_bioReplicate);
        header.add(COL_technology);
        header.add(COL_assay);
    }

    /**
     * Add a row for an unlabeled sample (single LC-MS file)
     */
    public void addSampleLFQ(String lcmsfileName, String replicate, ArrayList<String> enzymes, ArrayList<String> mods, String precTol, String prodTol, String instrument) {
        String[] row = new String[header.size()];
        row[header.indexOf(COL_organism)] = guessedOrganism;
        row[header.indexOf(COL_datafile)] = lcmsfileName;
        row[header.indexOf(COL_replicate)] = replicate;
        row[header.indexOf(COL_precTol)] = precTol;
        row[header.indexOf(COL_prodTol)] = prodTol;
        row[header.indexOf(COL_instrument)] = instrument == null ? "not available" : ("NT=" + instrument);
        for (int i=0; i < enzymes.size(); i++) {
            row[firstEnzymeIndex + i] = enzymes.get(i);
        }
        for (int i=0; i < mods.size(); i++) {
            row[firstModIndex + i] = mods.get(i);
        }
        rows.add(row);
    }

    public void addSampleTMT(String lcmsfileName, String replicate, ArrayList<String> enzymes, ArrayList<String> mods, QuantLabel quantLabel, String precTol, String prodTol, String instrument) {
        for (String label : quantLabel.getReagentNames()) {
            String[] row = new String[header.size()];
            row[header.indexOf(COL_organism)] = guessedOrganism;
            row[header.indexOf(COL_datafile)] = lcmsfileName;
            row[header.indexOf(COL_replicate)] = replicate;
            row[header.indexOf(COL_precTol)] = precTol;
            row[header.indexOf(COL_prodTol)] = prodTol;
            row[header.indexOf(COL_instrument)] = instrument == null ? "not available" : ("NT=" + instrument);
            row[header.indexOf(COL_label)] = String.format("%s%s", quantLabel.getType(), label);
            for (int i=0; i < enzymes.size(); i++) {
                row[firstEnzymeIndex + i] =  enzymes.get(i);
            }
            for (int i=0; i < mods.size(); i++) {
                row[firstModIndex + i] = mods.get(i);
            }
            rows.add(row);
        }
    }

    public void printTable(Path path) throws IOException {
        Files.deleteIfExists(path);
        ArrayList<String> outputLines = new ArrayList<>();
        outputLines.add(String.join("\t", header));
        for (String[] row : rows) {
            List<String> temp = Arrays.asList(row);
            Collections.replaceAll(temp, null, emptyStr);     // prevent empty values from printing "null"
            outputLines.add(String.join("\t", temp));
        }
        FileUtils.write(path.toFile(), String.join("\n", outputLines), StandardCharsets.UTF_8, false);
    }

    public enum SDRFtypes {
        Default("default"),
        Human("human"),
        CellLines("cell-lines"),
        Vertebrates("vertebrates"),
        NonVertebrates("non-vertebrates"),
        Plants("plants");

        private final String text;
        SDRFtypes(String _text) {
            this.text = _text;
        }
        public String getText() {
            return this.text;
        }
    }

    /**
     * Map our enyzme names to the supported ontology names. If user has input something that's not one of
     * our supported enzymes (isn't in the enzymesMap), just pass that name through.
     */
    public static String mapEnzymeToSDRF(String inputEnzyme) {
        String converted = enzymesMap.get(inputEnzyme);
        if (converted == null) {
            return inputEnzyme;
        }
        return converted;
    }

    /**
     * Use the protein headers from checking the fasta at the beginning of the run to try to guess the organism
     * used in the search. If an organism can't be guessed, returns the SDRF default empty string.
     * @param proteinHeaders list of protein header strings from the fasta
     * @return
     */
    public static String guessOrganism(List<String> proteinHeaders) {
        HashMap<String, Integer> organismCounts = new HashMap<>();
        for (String header : proteinHeaders) {
            Matcher m = uniprotPattern.matcher(header);
            if (m.find()) {
                organismCounts.put(m.group(1), organismCounts.getOrDefault(m.group(1), 0) + 1);
            }
        }
        int highest = 0;
        String highestOrganism = emptyStr;
        for (Map.Entry<String, Integer> entry : organismCounts.entrySet()) {
            if (entry.getValue() > highest) {
                highestOrganism = entry.getKey();
                highest = entry.getValue();
            }
        }
        return highestOrganism;
    }

    /**
     * Match a modification against internal Unimod reference, returning the single match if there is exactly one
     * or null if there are either 0 or >1 matches.
     * @param mass
     * @param sitesWithTermini
     * @return
     */
    public static String matchUnimod(double mass, ArrayList<String> sitesWithTermini) {
        ArrayList<String> matches = new ArrayList<>();
        for (String unimodEntry : unimodList) {
            String[] splits = unimodEntry.split(";");
            float unimodMass = Float.parseFloat(splits[0]); // 0 = mass
            if (Math.abs(mass - unimodMass) < 0.0001) {
                // check if site definitions match
                boolean allSitesMatched = true;
                String unimodSites = splits[3];                   // 3 = site list
                for (String site : sitesWithTermini) {
                    // harmonize terminal sites to the unimod definitions
                    if (site.equals("N-term")) {
                        site = "n";
                    } else if (site.equals("C-term")) {
                        site = "c";
                    }
                    if (!unimodSites.contains(site)) {
                        allSitesMatched = false;
                        break;
                    }
                }
                if (allSitesMatched) {
                    matches.add(splits[2]);                 // 2 = name
                }
            }
        }
        if (matches.size() == 1) {
            return matches.get(0);
        } else {
            return null;
        }
    }

}
