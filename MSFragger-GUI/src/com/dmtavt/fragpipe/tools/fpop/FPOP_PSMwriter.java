package com.dmtavt.fragpipe.tools.fpop;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class FPOP_PSMwriter {

    private static final Pattern modPattern = Pattern.compile("(\\d+[A-Z]\\(([-\\d.]+)\\))");
    private static final String COL_ASSIGNED_MODS = "Assigned Modifications";
    private static final String COL_FPOP = "FPOP Modifications";

    private HashMap<String, Integer> columns;

    public static void main(String[] args) {
        ArrayList<Path> psmPaths = new ArrayList<>();
        try {
            psmPaths = getPSMPaths(args[0]);
        } catch (IOException ex) {
            System.err.printf("Error: unable to read filelist_fpop at %s. Check that the file exists and is not in use.\n", args[0]);
            System.exit(1);
        }
        double[] mods = parseModsArg(args[1]);

        for (Path psmPath : psmPaths) {
            FPOP_PSMwriter fpopWriter = new FPOP_PSMwriter();
            try {
                System.out.println("Writing FPOP mods to " + psmPath + " ...");
                fpopWriter.editPSMtable(psmPath, mods);
            } catch (IOException e) {
                System.err.printf("Error: FPOP mods could not be written, unable to parse PSM table %s. Check that the file exists and is not in use.\n", args[0]);
                System.exit(1);
            }
        }
    }

    public FPOP_PSMwriter() {
    }

    private static ArrayList<Path> getPSMPaths(String fragpipeDir) throws IOException {
        ArrayList<Path> paths = new ArrayList<>();
        try (Stream<Path> files = Files.walk(Path.of(fragpipeDir))) {
            paths = files.filter(p -> p.getFileName().toString().startsWith("psm.tsv") && p.getFileName().toString().endsWith("psm.tsv")).collect(Collectors.toCollection(ArrayList::new));
        } catch (Exception ex) {
            System.err.println("Error in finding psm.tsv file path(s)");
            System.err.println(ex.getMessage());
            System.exit(1);
        }
        return paths;
    }

    public static double[] parseModsArg(String modArg) {
        String[] splits = modArg.split("[\\s,/;]+");
        double[] mods = new double[splits.length];
        for (int i = 0; i < splits.length; i++) {
            try {
                mods[i] = Double.parseDouble(splits[i]);
            } catch (NumberFormatException e) {
                System.err.printf("Error parsing provided FPOP mods list: %s is not a number.\n", splits[i]);
                System.exit(1);
            }
        }
        return mods;
    }

    public void editPSMtable(Path psmPath, double[] fpop_masses) throws IOException {
        ArrayList<String> output = new ArrayList<>();
        BufferedReader reader = new BufferedReader(new FileReader(psmPath.toFile()));
        String header = initHeader(reader.readLine());

        boolean hasPreviousFPOPcol = false;
        if (!columns.containsKey(COL_FPOP)) {
            // insert FPOP mods col after assigned mods
            List<String> headers = Arrays.stream(header.split("\t")).collect(Collectors.toList());
            headers.add(columns.get(COL_ASSIGNED_MODS) + 1, COL_FPOP);
            output.add(String.join("\t", headers));
        } else {
            hasPreviousFPOPcol = true;
            output.add(header);
        }

        String line;
        while ((line = reader.readLine()) != null) {
            List<String> splits = Arrays.stream(line.split("\t")).collect(Collectors.toList());
            String startingMods = splits.get(columns.get(COL_ASSIGNED_MODS));
            String fpopMods = getFpopMods(startingMods, fpop_masses);
            if (!hasPreviousFPOPcol) {
                splits.add(columns.get(COL_ASSIGNED_MODS) + 1, fpopMods);
            } else {
                splits.set(columns.get(COL_ASSIGNED_MODS) + 1, fpopMods);
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

    private String getFpopMods(String assignedMods, double[] fpop_masses) {
        ArrayList<String> fpopMods = new ArrayList<>();
        if (!assignedMods.isEmpty()) {
            String[] splits = assignedMods.split(",");
            for (String mod : splits) {
                Matcher m = modPattern.matcher(mod);
                if (m.find()) {
                    double mass = Double.parseDouble(m.group(2));
                    for (double fpopMass : fpop_masses) {
                        if (Math.abs(mass - fpopMass) < 0.01) {
                            fpopMods.add(m.group(1));
                            break;
                        }
                    }
                }
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
