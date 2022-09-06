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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with FragPipe.  If not, see <https://www.gnu.org/licenses/>.
 */

package com.dmtavt.fragpipe.util;

import umich.ms.datatypes.LCMSDataSubset;
import umich.ms.datatypes.scan.IScan;
import umich.ms.datatypes.scan.StorageStrategy;
import umich.ms.datatypes.scancollection.impl.ScanCollectionDefault;
import umich.ms.fileio.filetypes.AbstractLCMSDataSource;
import umich.ms.fileio.filetypes.mzml.MZMLFile;
import umich.ms.fileio.filetypes.mzxml.MZXMLFile;

import java.io.PrintWriter;
import java.util.HashMap;
import java.util.Map;
import java.util.TreeMap;


public class PairScans {

    public static void main(String[] args) {
        long time = System.currentTimeMillis();
        try {
            findScanPairs(args[0].trim(), Integer.parseInt(args[1]), args[2].trim(), args[3].trim());
        } catch (Exception e) {
            e.printStackTrace();
            System.exit(1);
        }
        System.out.printf("Done in %d ms.%n", System.currentTimeMillis() - time);
    }

    static void findScanPairs(String spectralPath, int nThreads, String firstActivationStr, String secondActivationStr) throws Exception {
        String ext = spectralPath.substring(spectralPath.lastIndexOf('.') + 1);

        if (!ext.equalsIgnoreCase("mzml") && !ext.equalsIgnoreCase("mzxml")) {
            System.err.println(spectralPath + " not supported. Scan pairing is only supported for mzML and mzXML formats. Scans not paired.");
            return;
        }

        ActivationFilter firstActivation = parseActivationFilter(firstActivationStr);
        ActivationFilter secondActivation = parseActivationFilter(secondActivationStr);
        if (firstActivation == null || secondActivation == null) {
            return;
        }

        ScanCollectionDefault scans = new ScanCollectionDefault();
        scans.setDefaultStorageStrategy(StorageStrategy.STRONG);
        scans.isAutoloadSpectra(true);
        AbstractLCMSDataSource<?> source;

        switch (ext.toLowerCase()) {
            case "mzml":
                source = new MZMLFile(spectralPath);
                break;
            case "mzxml":
                source = new MZXMLFile(spectralPath);
                break;
            default:
                return;
        }

        source.setExcludeEmptyScans(true);
        source.setNumThreadsForParsing(nThreads > 0 ? nThreads : Runtime.getRuntime().availableProcessors());
        scans.setDataSource(source);
        scans.loadData(LCMSDataSubset.STRUCTURE_ONLY);

        TreeMap<Integer, IScan> num2scan = scans.getMapNum2scan();
        TreeMap<Integer, Integer> pairedScans = new TreeMap<>();
//        TreeMap<Integer, String> debug = new TreeMap<>();
        HashMap<Double, Integer> unpairedPrecursorMap = new HashMap<>();
        for (final Map.Entry<Integer, IScan> scanNum_iscan : num2scan.entrySet()) {
            final IScan scan = scanNum_iscan.getValue();
            final int scanNum = scanNum_iscan.getKey();

            if (scan.getMsLevel() == 1) {
                // reset the unpaired precursor list for next set of MS2 scans
                unpairedPrecursorMap = new HashMap<>();
            } else if (scan.getMsLevel() == 2) {
                // find scan pairs given activation types of interest
                String filterString = scan.getFilterString();
                if (filterString != null && filterString.contains("@")) {
                    // get activation type from filter string
                    String[] filterSplits = filterString.split("@");
                    // only check against the primary (first) activation type in the scan, so always take filterSplits[1] and not any additional if present
                    String activationStr = filterSplits[1].substring(0, 3);
                    double precursorMZ = scan.getPrecursor().getMzTarget();

                    if (activationStr.equalsIgnoreCase(firstActivation.getText())) {
                        // first activation - record precursor to look for follow-up scans
                        unpairedPrecursorMap.put(precursorMZ, scanNum);
                    } else if (activationStr.equalsIgnoreCase(secondActivation.getText())) {
                        // second activation - find paired precursor and record the pairing, remove precursor from unpaired
                        if (unpairedPrecursorMap.containsKey(precursorMZ)) {
                            // pair found, record first activation scan #, second activation scan #
                            pairedScans.put(unpairedPrecursorMap.get(precursorMZ), scanNum);
//                            String debugStr = String.format("%d\t%.4f\t%.4f\t%s\t%s", scanNum, precursorMZ, num2scan.get(unpairedPrecursorMap.get(precursorMZ)).getPrecursor().getMzTarget(), scan.getFilterString(), num2scan.get(unpairedPrecursorMap.get(precursorMZ)).getFilterString());
//                            debug.put(unpairedPrecursorMap.get(precursorMZ), debugStr);
                            unpairedPrecursorMap.remove(precursorMZ);
                        } else {
                            // unexpected second activation without first
                            System.out.printf("Unpaired precursor %.4f in scan %d, not paired", precursorMZ, scanNum);
                        }
                    } else {
                        // unexpected activation - ignore
                        System.out.printf("Unspecified activation %s in scan %d, not paired", activationStr, scanNum);
                    }
                } else {
                    // filter string not recognized
                    // todo: add activation directly?
                }

            } else {
                // ignore higher MS levels if present
                continue;
            }


        }
        scans.reset();
        source.close();

        // write scan pairs to file
        String outputPath = spectralPath.substring(0, spectralPath.lastIndexOf('.') + 1) + "pairs";
        PrintWriter out = new PrintWriter(outputPath);
        for (final Map.Entry<Integer, Integer> pairEntry : pairedScans.entrySet()) {
            out.write(String.format("%s\t%s\n", pairEntry.getKey(), pairEntry.getValue()));
        }
//        for (final Map.Entry<Integer, String> pairEntry : debug.entrySet()) {
//            out.write(String.format("%s\t%s\n", pairEntry.getKey(), pairEntry.getValue()));
//        }
        out.flush();
        out.close();
    }

    static ActivationFilter parseActivationFilter(String input) {
        for (ActivationFilter activation : ActivationFilter.values()) {
            if (activation.getText().equalsIgnoreCase(input)) {
                return activation;
            }
        }
        System.err.printf("Activation filter string %s not recognized, pairing will not be performed", input);
        return null;
    }


    // Filter string text used for activation types in Thermo data
    public enum ActivationFilter {
        HCD("HCD"), ETD("ETD"), CID("CID"), ECD("ECD");

        private final String text;

        ActivationFilter(String _text) {
            this.text = _text;
        }

        public String getText() {
            return this.text;
        }
    }
}

