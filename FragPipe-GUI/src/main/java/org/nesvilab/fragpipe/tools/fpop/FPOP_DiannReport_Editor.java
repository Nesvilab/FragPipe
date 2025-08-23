package org.nesvilab.fragpipe.tools.fpop;

import static org.nesvilab.fragpipe.cmd.ToolingUtils.UNIMOD_OBO;
import static org.nesvilab.fragpipe.cmd.ToolingUtils.getUnimodOboPath;
import static org.nesvilab.fragpipe.util.AAMasses.AAMasses;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import umich.ms.fileio.filetypes.unimod.UnimodOboReader;

public class FPOP_DiannReport_Editor {

    private static final Pattern modPattern = Pattern.compile("([A-Z]\\(([-\\d.]+)\\))|\\((UniMod:\\d+)\\)");
    private static final String COL_MODIFIED_SEQUENCE = "Modified.Sequence";
    private static final String COL_FPOP = "FPOP Modifications";
    private final Map<String, Float> unimodMassMap;

    private HashMap<String, Integer> columns;

    public static void main(String[] args) {
        Locale.setDefault(Locale.US);
        ArrayList<Path> reportPaths = new ArrayList<>();
        try {
            reportPaths = getReportPaths(args[0]);
        } catch (IOException ex) {
            System.err.printf("Error: unable to read filelist_fpop at %s. Check that the file exists and is not in use.\n", args[0]);
            System.exit(1);
        }
        double[] mods = FPOP_PSMwriter.parseModsArg(args[1]);

        for (Path reportPath : reportPaths) {
            try {
                FPOP_DiannReport_Editor fpopWriter = new FPOP_DiannReport_Editor();
                try {
                    System.out.println("Writing FPOP mods to " + reportPath + "...");
                    fpopWriter.editReportTsv(reportPath, mods);
                } catch (IOException e) {
                    System.err.printf("Error: FPOP mods could not be written, unable to parse report.tsv table %s. Check that the file exists and is not in use.\n", args[0]);
                    System.exit(1);
                }
            } catch (Exception ex) {
                System.err.print("Error: FPOP mods could not be written to report.tsv\n");
                ex.printStackTrace();
                System.exit(1);
            }
        }
    }

    public FPOP_DiannReport_Editor() throws Exception {
        Path unimodPath = getUnimodOboPath(UNIMOD_OBO);
        UnimodOboReader unimodOboReader = new UnimodOboReader(unimodPath);
        unimodMassMap = unimodOboReader.unimodMassMap;
    }

    private static ArrayList<Path> getReportPaths(String fragpipeDir) throws IOException {
        ArrayList<Path> paths = new ArrayList<>();
        try (Stream<Path> files = Files.walk(Path.of(fragpipeDir))) {
            paths = files.filter(p -> p.getFileName().toString().startsWith("report.tsv") && p.getFileName().toString().endsWith("report.tsv")).collect(Collectors.toCollection(ArrayList::new));
        } catch (Exception ex) {
            System.err.println("Error in finding report.tsv file path");
            System.err.println(ex.getMessage());
            System.exit(1);
        }
        return paths;
    }

    public void editReportTsv(Path psmPath, double[] fpop_masses) throws IOException {
        ArrayList<String> output = new ArrayList<>();
        BufferedReader reader = new BufferedReader(new FileReader(psmPath.toFile()));
        String header = initHeader(reader.readLine());

        boolean hasPreviousFPOPcol = false;
        if (!columns.containsKey(COL_FPOP)) {
            // insert FPOP mods col after assigned mods
            List<String> headers = Arrays.stream(header.split("\t")).collect(Collectors.toList());
            headers.add(columns.get(COL_MODIFIED_SEQUENCE) + 1, COL_FPOP);
            output.add(String.join("\t", headers));
        } else {
            hasPreviousFPOPcol = true;
            output.add(header);
        }

        String line;
        while ((line = reader.readLine()) != null) {
            List<String> splits = Arrays.stream(line.split("\t")).collect(Collectors.toList());
            String startingMods = splits.get(columns.get(COL_MODIFIED_SEQUENCE));
            String fpopMods = getFpopMods(startingMods, fpop_masses);
            if (!hasPreviousFPOPcol) {
                splits.add(columns.get(COL_MODIFIED_SEQUENCE) + 1, fpopMods);
            } else {
                splits.set(columns.get(COL_MODIFIED_SEQUENCE) + 1, fpopMods);
            }
            output.add(String.join("\t", splits));
        }
        reader.close();

        BufferedWriter writer = new BufferedWriter(new FileWriter(psmPath.toFile()));
        for (String s : output) {
            writer.write(s + "\n");
        }
        writer.close();
    }

    /**
     * Parse the DIANN modified.sequence string for FPOP mods. Accounts for both Unimod IDs in the form
     * A(UniMod:xx) and mass-only IDs in the form A(87.0320).
     */
    public String getFpopMods(String modifiedSequence, double[] fpop_masses) {
        ArrayList<String> fpopMods = new ArrayList<>();
        if (!modifiedSequence.isEmpty()) {
            Matcher m = modPattern.matcher(modifiedSequence);
            int modCharacterOffset = 0;
            while (m.find()) {
                String match = m.group(2);
                double mass; char aa; int adjustedIndex; int matchLen;

                if (match == null) {
                    // find the mass from Unimod ID lookup
                    match = m.group(3);
                    mass = unimodMassMap.get(match.toLowerCase());
                    if (m.start() == 0) {
                        aa = 'n';   // N-terminal mod
                    } else {
                        aa = modifiedSequence.charAt(m.start() - 1);
                    }
                    adjustedIndex = m.start() - modCharacterOffset;
                    matchLen = m.group().length();
                } else {
                    // mass specified explicitly
                    aa = modifiedSequence.charAt(m.start());
                    mass = Double.parseDouble(match) - AAMasses[aa - 'A'];
                    adjustedIndex = m.start() - modCharacterOffset + 1;     // +1 to index from 1, not 0
                    matchLen = m.group().length() - 1;      // -1 because group includes one residue from the sequence
                }

                for (double fpopMass : fpop_masses) {
                    if (Math.abs(mass - fpopMass) < 0.01) {
                        fpopMods.add(String.format("%d%s(%.4f)", adjustedIndex, aa, fpopMass));
                        break;
                    }
                }
                // Update the cumulative offset to get the index of any subsequent mods
                modCharacterOffset += matchLen;
            }
        }
        return String.join(",", fpopMods);
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

