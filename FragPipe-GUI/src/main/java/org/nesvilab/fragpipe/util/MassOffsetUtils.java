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

package org.nesvilab.fragpipe.util;

import org.nesvilab.fragpipe.tabs.TabMsfragger;
import org.nesvilab.utils.SwingUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import java.awt.*;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class MassOffsetUtils {

    private static final Pattern sitesPattern = Pattern.compile("([A-Zall-nc\\[\\]^*])");
    private static final Pattern massPattern = Pattern.compile("([\\d.-]+)\\(");
    private static final Pattern diagPattern = Pattern.compile("d=([\\d.\\-,\\s]+)");
    private static final Pattern fragRemPattern = Pattern.compile("f=([\\d.\\-,\\s]+)");
    private static final Pattern pepRemPattern = Pattern.compile("p=([\\d.\\-,\\s]+)");
    private static final Pattern resPattern = Pattern.compile("aa=([A-Z]+)");
    private static final Logger log = LoggerFactory.getLogger(TabMsfragger.class);
    public static final String DELIMITER = ";";


    /**
     * Parse a mass offsets file to read in all mass offset information.
     * File format: one offset per line, tab separated. Fields are: mass, sites, diagnostic ions, peptide remainders, fragment remainders
     * E.g.: 365.1322	ST	204.0866, 366.14	203.07937, 365.1322	203.07937
     * @return
     * @throws IOException
     */
    public static String parseOffsetsFile(String massOffsetFilePath, Component parent) throws IOException, NumberFormatException {
        ArrayList<MassOffset> offsets = new ArrayList<>();
        ArrayList<String> offsetStrs = new ArrayList<>();

        BufferedReader in = new BufferedReader(new FileReader(massOffsetFilePath));
        String line;
        boolean foundZero = false;
        while ((line = in.readLine()) != null) {
            line = line.replace("\"", "");	// in case user saved tsv with Excel
            if (line.startsWith("#")) {
                continue;
            }
            while (line.startsWith("\t")) {
                // can happen when saving from Excel. Correct silently by removing starting tab(s)
                line = line.replaceFirst("\t", "");
            }
            String[] splits = line.split("\t");
            final int EXPECTED_LENGTH = 5;
            if (splits.length < EXPECTED_LENGTH) {
                // empty entries are allowed, and can result in shorter lines (depending on how the file was saved). Fill in the missing entries with empty strings.
                String[] newSplits = new String[EXPECTED_LENGTH];
                for (int i=0; i < newSplits.length; i++) {
                    if (i < splits.length) {
                        newSplits[i] = splits[i];
                    } else {
                        newSplits[i] = "";
                    }
                }
                splits = newSplits;
            }
            // Get mass and sites
            float mass;
            try {
                mass = Float.parseFloat(splits[0]);
            } catch (NumberFormatException ex) {
                log.error(String.format("The mass '%s' could not be parsed as a number from line '%s'. Please check that the offsets file is formatted correctly and retry.", splits[0], line));
                SwingUtils.showErrorDialog(parent, String.format("The mass '%s' could not be parsed as a number from line '%s'. Please check that the offsets file is formatted correctly and retry.", splits[0], line), "Error");
                continue;
            }
            if (mass == 0.0) {
                foundZero = true;
            }
            ArrayList<String> sites = getSites(splits[1]);

            // keep track of all unique fragment and peptide remainder ions for later indexing. Rounded to 6 decimal places
            float[] peptideRems = parseFloats(splits[3], parent);
            float[] fragmentRems = parseFloats(splits[4], parent);

            // generate the MassOffset and its string
            MassOffset offset = new MassOffset(mass, sites.toArray(new String[0]), parseFloats(splits[2], parent), peptideRems, fragmentRems);
            offsets.add(offset);
            offsetStrs.add(offset.toString());
        }
        // make sure 0 offset is included in the list
        if (!foundZero) {
            MassOffset zeroOffset = new MassOffset(0, new String[0], new float[0], new float[0], new float[0]);
            offsets.add(0, zeroOffset);
            offsetStrs.add(0, zeroOffset.toString());
            log.warn("Warning: 0 was not included in the mass offsets file. Adding it to the offsets list.");
        }

        return String.join(DELIMITER, offsetStrs);
    }


    public static ArrayList<String> getSites(String splits) {
        ArrayList<String> sites = new ArrayList<>();
        Matcher matcher = sitesPattern.matcher(splits);
        while (matcher.find()) {
            sites.add(matcher.group());
        }
        return sites;
    }

    private static float[] parseFloats(String floatList, Component parent) throws NumberFormatException {
        if (floatList.isEmpty()) {
            return new float[0];
        }
        floatList = floatList.replaceAll("\"", "");
        String[] splits = floatList.split("[,\\s/]+");
        float[] values = new float[splits.length];
        for (int i=0; i < splits.length; i++) {
            try {
                values[i] = Float.parseFloat(splits[i].trim());
            } catch (NumberFormatException ex) {
                log.error(String.format("The mass '%s' could not be parsed as a number from entry '%s'. Please check that the offsets file is formatted correctly and retry.", splits[0], floatList));
                SwingUtils.showErrorDialog(parent, String.format("The mass '%s' could not be parsed as a number from entry '%s'. Please check that the offsets file is formatted correctly and retry.", splits[i], floatList), "Error");
            }
        }
        return values;
    }

    public static String floatArrToString(float[] input) {
        String[] strs = new String[input.length];
        for (int i=0; i < input.length; i++) {
            strs[i] = String.format("%.5f", input[i]);
        }
        return String.join(",", strs);
    }

    public static class MassOffset {

        public float[] diagnosticIons;
        public float[] peptideRemainderIons;
        public float[] fragmentRemainderIons;
        public float mass;
        public String[] allowedResidues;

        public MassOffset(float mass, String[] allowedResidues, float[] diagnosticIons, float[] peptideRemainderIons, float[] fragmentRemainderIons) {
            this.mass = mass;
            this.diagnosticIons = diagnosticIons;
            this.peptideRemainderIons = peptideRemainderIons;
            this.fragmentRemainderIons = fragmentRemainderIons;
            this.allowedResidues = allowedResidues;
        }

        public MassOffset(String offsetString) {
            // convert from String form back to object for saving to file
            Matcher massMatch = massPattern.matcher(offsetString);
            if (massMatch.find()) {
                this.mass = Float.parseFloat(massMatch.group(1));
            }

            Matcher sitesMatch = resPattern.matcher(offsetString);
            if (sitesMatch.find()) {
                ArrayList<String> sites = getSites(sitesMatch.group(1));
                this.allowedResidues = sites.toArray(new String[0]);
            } else {
                this.allowedResidues = new String[0];
            }

            Matcher diagMatch = diagPattern.matcher(offsetString);
            if (diagMatch.find()) {
                String[] splits = diagMatch.group(1).split("[, ]+");
                float[] diagIons = new float[splits.length];
                for (int i=0; i < splits.length; i++) {
                    diagIons[i] = Float.parseFloat(splits[i]);
                }
                this.diagnosticIons = diagIons;
            } else {
                this.diagnosticIons = new float[0];
            }

            Matcher fragMatch = fragRemPattern.matcher(offsetString);
            if (fragMatch.find()) {
                String[] splits = fragMatch.group(1).split("[, ]+");
                float[] fragIons = new float[splits.length];
                for (int i=0; i < splits.length; i++) {
                    fragIons[i] = Float.parseFloat(splits[i]);
                }
                this.fragmentRemainderIons = fragIons;
            } else {
                this.fragmentRemainderIons = new float[0];
            }

            Matcher pepMatch = pepRemPattern.matcher(offsetString);
            if (pepMatch.find()) {
                String[] splits = pepMatch.group(1).split("[, ]+");
                float[] pepIons = new float[splits.length];
                for (int i=0; i < splits.length; i++) {
                    pepIons[i] = Float.parseFloat(splits[i]);
                }
                this.peptideRemainderIons = pepIons;
            } else {
                this.peptideRemainderIons = new float[0];
            }
        }

        public String getSiteStr() {
            return String.join("", allowedResidues);
        }

        @Override
        public String toString() {
            String diagnostic = diagnosticIons.length > 0 ? "_d=" + floatArrToString(diagnosticIons) : "";
            String peprem = peptideRemainderIons.length > 0 ? "_p=" + floatArrToString(peptideRemainderIons) : "";
            String fragrem = fragmentRemainderIons.length > 0 ? "_f=" + floatArrToString(fragmentRemainderIons) : "";

            return String.format("%.5f(aa=%s%s%s%s)",
                    mass,
                    String.join("", allowedResidues),
                    diagnostic,
                    peprem,
                    fragrem
            );
        }

        // for printing to a template file
        public String toFileString() {
            return String.format("%.4f\t%s\t%s\t%s\t%s\n",
                    mass,
                    String.join("", allowedResidues),
                    floatArrToString(diagnosticIons),
                    floatArrToString(peptideRemainderIons),
                    floatArrToString(fragmentRemainderIons));
        }
    }
}
