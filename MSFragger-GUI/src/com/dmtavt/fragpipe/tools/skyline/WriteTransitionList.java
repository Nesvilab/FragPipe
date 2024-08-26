package com.dmtavt.fragpipe.tools.skyline;

import static com.dmtavt.fragpipe.cmd.ToolingUtils.UNIMOD_OBO;
import static com.dmtavt.fragpipe.cmd.ToolingUtils.getUnimodOboPath;
import static com.dmtavt.fragpipe.util.Utils.AAMasses;

import com.dmtavt.fragpipe.util.UnimodOboReader;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class WriteTransitionList {

    public static Map<String, Float> unimodMasses;
    private static HashMap<String, Integer> columns;
    private static final double AVERAGINE_ISOTOPE = 1.00235;
    private static final Pattern nGlycoPattern = Pattern.compile("(N)\\(UniMod:(\\d+)\\)");
    private static final Pattern oGlycoPattern = Pattern.compile("([ST])\\(UniMod:(\\d+)\\)");
    private static final Pattern unannotatedPattern = Pattern.compile("(\\w)\\[(\\d+\\.\\d+)\\]");

    static {
        // init Unimod DB for mod matching
        try {
            Path unimodPath = getUnimodOboPath(UNIMOD_OBO);
            UnimodOboReader unimodOboReader = new UnimodOboReader(unimodPath);
            unimodMasses = unimodOboReader.unimodMassMap;
        } catch (Exception ex) {
            ex.printStackTrace();
            System.exit(1);
        }
    }

    /**
     * Copy the library.tsv file to a new skyline-compatible transition list. If requested, add precursors
     * and/or convert glycans from Unimod back to mass (to allow Skyline to process the neutral losses correctly).
     */
    public void writeTransitionList(Path libraryTSV, int addPrecursors, int modsMode) throws IOException {
        ArrayList<String> output = new ArrayList<>();

        BufferedReader reader = new BufferedReader(new FileReader(libraryTSV.toFile()));
        String header = initHeader(reader.readLine());
        output.add(header + "\n");

        String line;
        String modifiedPeptide, currentModifiedPeptide = "";
        while ((line = reader.readLine()) != null) {
            String[] splits = line.split("\t");

            // add precursor lines
            if (addPrecursors > 0) {
                modifiedPeptide = splits[columns.get("ModifiedPeptideSequence")];
                if (!currentModifiedPeptide.equals(modifiedPeptide)) {
                    // new peptide entry: add precursor lines here
                    currentModifiedPeptide = modifiedPeptide;
                    double startMz = Double.parseDouble(splits[columns.get("PrecursorMz")]);
                    double charge = Double.parseDouble(splits[columns.get("PrecursorCharge")]);
                    for (int i = 0; i < addPrecursors; i++) {
                        String[] newline = new String[splits.length];
                        String newMz = String.valueOf(startMz + (i * AVERAGINE_ISOTOPE) / charge);
                        for (int j = 0; j < splits.length; j++) {
                            if (j == columns.get("ProductMz")) {
                                newline[j] = newMz;
                            } else if (j == columns.get("Annotation")) {
                                newline[j] = "precursor";
                            } else if (j == columns.get("FragmentType") || j == columns.get("FragmentCharge") || j == columns.get("FragmentSeriesNumber")) {
                                newline[j] = "";
                            } else {
                                newline[j] = splits[j];
                            }
                        }
                        output.add(editModifiedPeptide(newline, modsMode));
                    }
                }
            }
            output.add(editModifiedPeptide(splits, modsMode));
        }

        Path outputPath = libraryTSV.getParent().resolve("library_skyline.tsv").toAbsolutePath();
        BufferedWriter writer = new BufferedWriter(new FileWriter(outputPath.toFile()));
        for (String s : output) {
            writer.write(s);
        }
        writer.close();
    }

    /**
     * Convert glycan mods to masses if using a glycan mode. Mods mode 0 = regular, 1 = O-glyco, 2 = N-glyco
     */
    private String editModifiedPeptide(String[] splits, int modsMode) {
        if (modsMode != 0) {
            String modifiedPeptide = splits[columns.get("ModifiedPeptideSequence")];
            Matcher matcher = modsMode == 1 ? oGlycoPattern.matcher(modifiedPeptide) : nGlycoPattern.matcher(modifiedPeptide);
            StringBuilder sb = new StringBuilder();
            while (matcher.find()) {
                String newGlycanStr = String.format("%s(+%f)", matcher.group(1), getGlycanMass(matcher.group(2)));
                matcher.appendReplacement(sb, newGlycanStr);
            }
            matcher.appendTail(sb);
            splits[columns.get("ModifiedPeptideSequence")] = sb.toString();
        }

        // convert unmatched modifications from easyPQP to a Skyline-acceptable format (otherwise they cause a crash if placed alongside a unimod mod)
        String modifiedPeptide = splits[columns.get("ModifiedPeptideSequence")];
        Matcher matcher = unannotatedPattern.matcher(modifiedPeptide);
        StringBuilder sb = new StringBuilder();
        while (matcher.find()) {
            // subtract amino acid mass and reformat to Skyline format "(+mass)"
            String AA = matcher.group(1);
            double deltaMass = Double.parseDouble(matcher.group(2));
            double finalMass = deltaMass - AAMasses[AA.charAt(0) - 'A'];
            String newModStr = String.format("%s(+%f)", AA, finalMass);
            matcher.appendReplacement(sb, newModStr);
        }
        matcher.appendTail(sb);
        splits[columns.get("ModifiedPeptideSequence")] = sb.toString();

        return String.join("\t", splits) + "\n";
    }

    private float getGlycanMass(String unimodNum) {
        String unimodKey = "unimod:" + unimodNum;
        return unimodMasses.getOrDefault(unimodKey, (float) 0);
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
