package com.dmtavt.fragpipe.tools.comet;

import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class CometPinUpdateDecoyLabel {
    private static boolean isDebug = false;
    public static final String NEW_LINE = System.lineSeparator();
    public static final String NEGATIVE_LABEL = "-1";

    /**
     * Overwrites 'Label' column in a pin file using a reg-exp to find decoys in Protein names.
     * {@literal Usage: CometPinUpdateDecoyLabel <regExp-for-decoy-prot> <path-to-comet-pin> <path-to-fixed-file>}"
     */
    public static void main(String[] args) throws IOException {
        if (args.length != 3) {
            exit("Usage: CometPinUpdateDecoyLabel <regExp-for-decoy-prot> <path-to-comet-pin> <path-to-fixed-file>");
        }
        final Path pathIn = Paths.get(args[1]);
        if (!Files.exists(pathIn)) {
            exit("File not exists: " + pathIn);
        }
        if (Files.isDirectory(pathIn)) {
            exit("Directory given instead of file: " + pathIn);
        }
        final Path pathOutTmp = Paths.get(args[2]);
        if (Files.isDirectory(pathOutTmp)) {
            exit("Output path exists and is a directory: " + pathOutTmp);
        }
        final Pattern re = Pattern.compile(args[0]);

        List<String> lines = Files.readAllLines(pathIn);
        if (lines.isEmpty())
            exit("input Pin file didn't even have a header line");
        final String header = lines.get(0);
        int lineLenMax = lines.stream().mapToInt(String::length).max().orElse(0);
        final char[] cbuf = new char[lineLenMax];

        // find where Proteins column is
        final String[] split = header.split("\t");
        final int[] offsets = new int[split.length];
        final int indexOfProts = Arrays.asList(split).indexOf("Proteins");
        if (indexOfProts < 0)
            exit("Could not find 'Proteins' entry in the header of the given pin file");
        final int indexOfLabel = Arrays.asList(split).indexOf("Label");
        if (indexOfLabel < 0)
            exit("Could not find 'Label' entry in the header of the given pin file");


        try (BufferedWriter f = Files.newBufferedWriter(pathOutTmp, StandardCharsets.UTF_8, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)) {
            f.write(header);
            newLine(f);
            int countDecoys = 0;
            int countTotal = 0;
            for (int lineNum = 1; lineNum <lines.size(); lineNum++) {
                final String line = lines.get(lineNum);
                offsets[0] = 0;
                int idx = 1;
                while (idx <= indexOfProts) {
                    final int offset = line.indexOf('\t', offsets[idx-1]+1);
                    if (offset < 0)
                        throw new IllegalStateException(String.format("Error parsing line [%d], wrong number of columns.\nLine content: [%s]", lineNum, line));
                    offsets[idx] = offset + 1; // +1 for the tab char itself
                    idx += 1;
                }
                if (isDebug)
                    System.out.println(line.subSequence(offsets[indexOfProts], line.length()));
                countTotal += 1;
                final Matcher m = re.matcher(line.subSequence(offsets[indexOfProts], line.length()));
                if (!m.find()) {
                    f.write(line); // not found, copy original line
                    newLine(f);
                    continue;
                }
                // it's a decoy protein, replace fields
                if (isDebug)
                    System.out.println("Found decoy");
                countDecoys += 1;
                int idxLo, idxHi;
                if (indexOfLabel > 0) { // text before the label
                    idxLo = 0;
                    idxHi = offsets[indexOfLabel];
                    line.getChars(idxLo, idxHi, cbuf, 0);
                    f.write(cbuf, 0, idxHi - idxLo);
                }
                f.write(NEGATIVE_LABEL);
                f.write("\t");
                if (indexOfLabel != offsets.length-1) { // text after the label
                    idxLo = offsets[indexOfLabel + 1];
                    idxHi = line.length();
                    line.getChars(idxLo, idxHi, cbuf, 0);
                    f.write(cbuf, 0, idxHi - idxLo);
                }
                newLine(f);
            }
            f.flush();
            System.out.printf("Updated %d decoy entries (out of %d total), output to: %s", countDecoys, lines.size(), pathOutTmp);
        }
    }

    private static void newLine(final BufferedWriter f) throws IOException {
        f.write(NEW_LINE);
    }

    public static void exit(String msg) {
        System.err.println(msg);
        System.exit(1);
    }
}
