package org.nesvilab.fragpipe.tools.skyline;

import java.io.*;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;


public class WriteSSL {

    private static Map<String, Integer> columns;
    public static final String COL_SCANID = "Spectrum";
    public static final String COL_CHARGE = "Charge";
    public static final String COL_SCORE = "Probability";
    public static final String COL_SCORE_2 = "PeptideProphet Probability";
    public static final String COL_RT = "Retention";
    public static final String COL_IONMOBILITY = "Ion Mobility";
    public static final String COL_RT_START = "Retention Time Start";
    public static final String COL_RT_END = "Retention Time End";
    public static final String COL_CV = "Compensation Voltage";

    public boolean hasCv = false;
    public boolean hasIm = false;


    /**
     * Generate SSL file for Skyline from psm.tsv file(s). Format: (tab delim)
     * file     scan    charge  sequence    score-type  score   RT  IM
     * Sequence includes all mods. IM is optional. Score-type can be PERCOLATOR QVALUE or PEPTIDE PROPHET SOMETHING
     */
    public void writeSSL(Set<Path> psmtsvFiles, Path outputPath, boolean isPercolator, Set<String> lcmsFiles, boolean useIonQuantPeaks) throws IOException {
        ArrayList<String> output = new ArrayList<>();

        // map file paths to the file names that will be in the psm.tsv
        Map<String, String> lcmsFileNames = new HashMap<>();
        for (String lcmsFile : lcmsFiles) {
            Path path = Paths.get(lcmsFile);
            String fileName = path.getFileName().toString();
            int dotIndex = fileName.lastIndexOf('.');
            lcmsFileNames.put(fileName.substring(0, dotIndex), lcmsFile.replace("\\", "/").replaceFirst("\\.d$", "_uncalibrated.mzML"));
        }

        for (Path psmtsv: psmtsvFiles) {
            BufferedReader reader = new BufferedReader(new FileReader(psmtsv.toFile()));
            initHeader(reader.readLine(), reader.readLine());
            reader.close();

            reader = new BufferedReader(new FileReader(psmtsv.toFile()));
            String line;
            while ((line = reader.readLine()) != null) {
                if (line.startsWith("Spectrum\tSpectrum File")) {
                    continue;
                }

                String[] splits = line.split("\t");
                StringBuilder sslLine = new StringBuilder();

                String[] scanSplits = splits[columns.get(COL_SCANID)].split("\\.");
                if (lcmsFileNames.containsKey(scanSplits[0])) {
                    sslLine.append(lcmsFileNames.get(scanSplits[0])).append("\t");     // rawfile
                } else {
                    System.out.printf("Error writing SSL file: could not find spectrum file corresponding to PSM entry: %s\n", scanSplits[0]);
                    System.exit(1);
                }
                sslLine.append(scanSplits[1]).append("\t");     // scan num
                sslLine.append(splits[columns.get(COL_CHARGE)]).append("\t");
                String modpep = WritePeptideList.generateModifiedPeptide(splits, columns, false, new HashMap<>());
                sslLine.append(modpep).append("\t");
                String scoreType = isPercolator ? "PERCOLATOR QVALUE" : "PEPTIDE PROPHET SOMETHING";
                sslLine.append(scoreType).append("\t");
                if (columns.containsKey(COL_SCORE)) {
                    if (isPercolator) {
                        sslLine.append(1 - Float.parseFloat(splits[columns.get(COL_SCORE)])).append("\t");
                    } else {
                        sslLine.append(splits[columns.get(COL_SCORE)]).append("\t");
                    }
                } else {
                    if (isPercolator) {
                        sslLine.append(1 - Float.parseFloat(splits[columns.get(COL_SCORE_2)])).append("\t");
                    } else {
                        sslLine.append(splits[columns.get(COL_SCORE_2)]).append("\t");
                    }
                }
                sslLine.append(Float.parseFloat(splits[columns.get(COL_RT)]) / 60).append("\t");      // RT in minutes

                // If there are LC-MS runs with and without ion mobility, the following block of the code might crash.
                // It is an intentional design to let the user know that they should not mixture the runs with and without ion mobility.
                if (hasCv) {
                    sslLine.append(splits[columns.get(COL_CV)]).append("\t");
                    sslLine.append("V").append("\t");
                } else if (hasIm) {
                    sslLine.append(splits[columns.get(COL_IONMOBILITY)]).append("\t");
                    sslLine.append("1/K0").append("\t");
                }

                // add IonQuant peak bounds if requested and present
                if (useIonQuantPeaks) {
                    if (!columns.containsKey(COL_RT_START) || !columns.containsKey(COL_RT_END)) {
                        useIonQuantPeaks = false;
                    } else {
                        sslLine.append(Float.parseFloat(splits[columns.get(COL_RT_START)]) / 60).append("\t");
                        sslLine.append(Float.parseFloat(splits[columns.get(COL_RT_END)]) / 60).append("\t");
                    }
                }
                output.add(sslLine.toString());
            }

            reader.close();
        }

        // write output
        BufferedWriter writer = new BufferedWriter(new FileWriter(outputPath.toFile()));
        String IMstr = (hasCv || hasIm) ? "\tion-mobility\tion-mobility-units" : "";
        String peakBoundsStr = useIonQuantPeaks ? "\tstart-time\tend-time" : "";
        String sslHeader = String.format("file\tscan\tcharge\tsequence\tscore-type\tscore\tretention-time%s%s\n", IMstr, peakBoundsStr);
        writer.write(sslHeader);
        for (String line: output) {
            writer.write(line + "\n");
        }
        writer.close();
    }

    private void initHeader(String header, String firstRow) {
        columns = new HashMap<>();
        boolean hasCv = false;
        boolean hasIm = false;
        String[] splits = header.split("\t");
        String[] splits2 = firstRow.split("\t");
        for (int i = 0 ; i < splits.length ; i++) {
            columns.put(splits[i], i);
            if (splits[i].equals(COL_CV) && !splits2[i].trim().isEmpty() && Float.parseFloat(splits2[i]) != 0) {
                hasCv = true;
            } else if (splits[i].equals(COL_IONMOBILITY) && !splits2[i].trim().isEmpty() && Float.parseFloat(splits2[i]) != 0) {
                hasIm = true;
            }
        }

        if (hasCv && hasIm) {
            throw new RuntimeException("There are both compensation voltages and ion mobilities in the input file:\n" + header + "\n" + firstRow);
        }

        this.hasCv = this.hasCv || hasCv;
        this.hasIm = this.hasIm || hasIm;
    }

}
