package com.dmtavt.fragpipe.util;

import com.dmtavt.fragpipe.tools.tmtintegrator.QuantLabel;
import org.apache.commons.io.FileUtils;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.*;
import java.util.stream.Collectors;

public class SDRFtable {

    private ArrayList<String[]> rows;
    private ArrayList<String> header;
    private int firstEnzymeIndex;
    private int firstModIndex;

    // column names
    private final String COL_datafile = "comment[data file]";
    private final String COL_replicate = "comment[technical replicate]";
    private final String COL_fraction = "comment[fraction identifier]";
    private final String COL_label = "comment[label]";
    private final String COL_instrument = "comment[instrument]";
    private final String COL_enzyme = "comment[cleavage agent details]";
    private final String COL_mods = "comment[modification parameters]";
    private final String COL_precTol = "comment[precursor mass tolerance]";
    private final String COL_prodTol = "comment[fragment mass tolerance]";

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
    }

    public SDRFtable(int numEnzymes, int numMods) {
        rows = new ArrayList<>();
        header = new ArrayList<>();

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
     * Add a row for an unlabeled sample (single LC-MS file)
     */
    public void addSampleLFQ(String lcmsfileName, String replicate, ArrayList<String> enzymes, ArrayList<String> mods, String precTol, String prodTol) {
        String[] row = new String[header.size()];
        row[header.indexOf(COL_datafile)] = lcmsfileName;
        row[header.indexOf(COL_replicate)] = replicate;
        row[header.indexOf(COL_precTol)] = precTol;
        row[header.indexOf(COL_prodTol)] = prodTol;
        for (int i=0; i < enzymes.size(); i++) {
            row[firstEnzymeIndex + i] = enzymes.get(i);
        }
        for (int i=0; i < mods.size(); i++) {
            row[firstModIndex + i] = mods.get(i);
        }
        rows.add(row);
    }

    public void addSampleTMT(String lcmsfileName, String replicate, ArrayList<String> enzymes, ArrayList<String> mods, QuantLabel quantLabel, String precTol, String prodTol) {
        for (String label : quantLabel.getReagentNames()) {
            String[] row = new String[header.size()];
            row[header.indexOf(COL_datafile)] = lcmsfileName;
            row[header.indexOf(COL_replicate)] = replicate;
            row[header.indexOf(COL_precTol)] = precTol;
            row[header.indexOf(COL_prodTol)] = prodTol;
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
        ArrayList<String> outputLines = new ArrayList<>();
        outputLines.add(String.join("\t", header));
        for (String[] row : rows) {
            List<String> temp = Arrays.asList(row);
            Collections.replaceAll(temp, null, "");     // prevent empty values from printing "null"
            outputLines.add(String.join("\t", temp));
        }
        FileUtils.write(path.toFile(), String.join("\n", outputLines), StandardCharsets.UTF_8, false);
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

}
