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

import java.util.Locale;
import org.nesvilab.fragpipe.tools.enums.ActivationTypes;
import org.nesvilab.utils.StringUtils;
import java.io.PrintWriter;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import umich.ms.datatypes.LCMSDataSubset;
import umich.ms.datatypes.scan.IScan;
import umich.ms.datatypes.scan.StorageStrategy;
import umich.ms.datatypes.scancollection.impl.ScanCollectionDefault;
import umich.ms.fileio.filetypes.AbstractLCMSDataSource;
import umich.ms.fileio.filetypes.mzml.MZMLFile;
import umich.ms.fileio.filetypes.mzxml.MZXMLFile;
import umich.ms.fileio.filetypes.thermo.ThermoRawFile;


public class PairScans {

    private List<String> pairedScans;
    private Map<Double, Integer> unpairedPrecursorMap;
    private Map<Double, Integer> multipairedPrecursorMap;

    public PairScans() {
        pairedScans = new ArrayList<>();
        unpairedPrecursorMap = new HashMap<>();
        multipairedPrecursorMap = new HashMap<>();
    }

    public static void main(String[] args) {
        Locale.setDefault(Locale.US);
        long time = System.nanoTime();
        PairScans pairer = new PairScans();
        try {
            /* Args:
             * - spectrum file path (string)
             * - num threads (int)
             * - first activation type (string)
             * - second activation type (string)
             * - reversed scan order (boolean)
             * - single scan type (boolean)
             */
            pairer.findScanPairs(args[0].trim(), Integer.parseInt(args[1]), args[2].trim(), args[3].trim(), Boolean.parseBoolean(args[4].trim()), Boolean.parseBoolean(args[5].trim()));
        } catch (Exception e) {
            e.printStackTrace();
            System.exit(1);
        }

        System.out.printf("Done in %.1f s.%n", (System.nanoTime() - time) * 1e-9f);
    }

    void findScanPairs(String spectralPath, int nThreads, String firstActivationStr, String secondActivationStr, boolean reverseOrder, boolean singleScanType) throws Exception {
        String ext = StringUtils.afterLastDot(spectralPath);
        String outputPath = StringUtils.upToLastDot(spectralPath) + ".pairs";

        // skip if file already exists
        if (Files.exists(Paths.get(outputPath))) {
            System.out.printf("Paired scan file already exists for input %s, will use it\n", spectralPath);
            return;
        }

        if (!ext.equalsIgnoreCase("mzml") && !ext.equalsIgnoreCase("raw") && !ext.equalsIgnoreCase("mzxml")) {
            System.err.println(spectralPath + " not supported. Scan pairing is only supported for mzML, mzXML, and raw formats. Scans not paired.\n");
            return;
        }

        ActivationTypes firstActivation = parseActivationFilter(firstActivationStr);
        ActivationTypes secondActivation = parseActivationFilter(secondActivationStr);
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
            case "raw":
                source = new ThermoRawFile(spectralPath);
                break;
            default:
                throw new RuntimeException("Unrecognized file extension: " + ext);
        }

        source.setExcludeEmptyScans(true);
        source.setNumThreadsForParsing(nThreads > 0 ? nThreads : Runtime.getRuntime().availableProcessors());
        scans.setDataSource(source);
        scans.loadData(LCMSDataSubset.STRUCTURE_ONLY);

        TreeMap<Integer, IScan> num2scan = scans.getMapNum2scan();
        for (final Map.Entry<Integer, IScan> scanNum_iscan : num2scan.entrySet()) {
            final IScan scan = scanNum_iscan.getValue();
            final int scanNum = scanNum_iscan.getKey();

            if (scan.getMsLevel() == 1) {
                // reset the unpaired precursor list for next set of MS2 scans
                unpairedPrecursorMap.clear();
                multipairedPrecursorMap.clear();
            } else if (scan.getMsLevel() == 2) {
                if (singleScanType) {
                    // no pairing needed, just set this scan as its own "pair"
                    pairedScans.add(String.format("%d\t%d\n", scanNum, scanNum));
                    continue;
                }

                // find scan pairs given activation types of interest
                String filterString = scan.getFilterString();
                if (filterString != null && filterString.contains("@")) {
                    // get activation type from filter string
                    String[] filterSplits = filterString.split("@");
                    // only check against the primary (first) activation type in the scan, so always take filterSplits[1] and not any additional if present
                    String activationStr = filterSplits[1].substring(0, 3);
                    double precursorMZ = scan.getPrecursor().getMzTarget();
                    String actualFirstActivation = reverseOrder ? secondActivation.getText() : firstActivation.getText();
                    String actualSecondActivation = reverseOrder ? firstActivation.getText() : secondActivation.getText();

                    if (activationStr.equalsIgnoreCase(actualFirstActivation)) {
                        // first activation - record precursor to look for follow-up scans
                        unpairedPrecursorMap.put(precursorMZ, scanNum);
                        multipairedPrecursorMap.put(precursorMZ, scanNum);
                    } else if (activationStr.equalsIgnoreCase(actualSecondActivation)) {
                        // second activation - find paired precursor and record the pairing, remove precursor from unpaired
                        scanPairingHelper(scanNum, precursorMZ, reverseOrder);
                    } else {
                        // unexpected activation - ignore
                        System.out.printf("Unspecified activation %s in scan %d\n", activationStr, scanNum);
                    }
                }
            }
        }
        scans.reset();
        source.close();

        // write scan pairs to file
        PrintWriter out = new PrintWriter(outputPath);
        for (final String pairStr : pairedScans) {
            out.write(pairStr);
        }
        out.flush();
        out.close();
    }

    /**
     * Save the scan pair information. Should be called once we expect the paired scan to be present in
     * unpairedPrecursorMap.
     * @param scanNum scan number of the 2nd scan (to be paired)
     * @param precursorMZ precursor m/z used to find the corresponding 1st scan
     * @param reverseOrder reverse the order when recording because the "child" scan actually comes 1st (e.g., ETD-HCD data)
     */
    private void scanPairingHelper(int scanNum, double precursorMZ, boolean reverseOrder) {
        // second activation - find paired precursor and record the pairing, remove precursor from unpaired
        if (unpairedPrecursorMap.containsKey(precursorMZ)) {
            // pair found, record first activation scan #, second activation scan #
            if (reverseOrder) {
                pairedScans.add(String.format("%d\t%d\n", scanNum, unpairedPrecursorMap.get(precursorMZ)));
            } else {
                pairedScans.add(String.format("%d\t%d\n", unpairedPrecursorMap.get(precursorMZ), scanNum));
            }
            unpairedPrecursorMap.remove(precursorMZ);
        } else {
            // check for multi-paired scans
            if (multipairedPrecursorMap.containsKey(precursorMZ)) {
                if (reverseOrder) {
                    pairedScans.add(String.format("%d\t%d\n", scanNum, multipairedPrecursorMap.get(precursorMZ)));
                } else {
                    pairedScans.add(String.format("%d\t%d\n", multipairedPrecursorMap.get(precursorMZ), scanNum));
                }
            } else {
                // unexpected second activation without first
                System.out.printf("Unpaired precursor %.4f in scan %d\n", precursorMZ, scanNum);
            }
        }
    }

    private static ActivationTypes parseActivationFilter(String input) {
        for (ActivationTypes activation : ActivationTypes.values()) {
            if (activation.getText().equalsIgnoreCase(input)) {
                return activation;
            }
        }
        System.err.printf("Activation filter string %s not recognized, pairing will not be performed\n", input);
        return null;
    }


}

