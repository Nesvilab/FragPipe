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

import com.github.chhh.utils.StringUtils;
import java.io.BufferedReader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import umich.ms.datatypes.lcmsrun.MsSoftware;
import umich.ms.datatypes.scan.IScan;
import umich.ms.datatypes.scan.props.Instrument;
import umich.ms.fileio.filetypes.mzbin.MZBINFile;
import umich.ms.fileio.filetypes.mzml.MZMLWriter;
import umich.ms.fileio.filetypes.mzml.MZMLWriter.ProcessingMethod;
import umich.ms.fileio.filetypes.mzml.jaxb.ProcessingMethodType;

public class WriteSubMzml {

  private static final Pattern pattern = Pattern.compile("^(.+)\\.(\\d+)\\.(\\d+)\\.(\\d+)");

  public static void main(String[] args) {
    long time = System.nanoTime();
    try {
      writeSubMzml(args[0], Paths.get(args[1]), Paths.get(args[2]), Float.parseFloat(args[3]), Integer.parseInt(args[4]) == 1);
    } catch (Exception e) {
      e.printStackTrace();
      System.exit(1);
    }
    System.out.printf("Done in %d s.%n", Math.round((System.nanoTime() - time) * 1e-9));
  }

  private static void writeSubMzml(String lcmsPathStr, Path psmPath, Path outputPath, float probabilityThreshold, boolean deleteMzbinAll) throws Exception {
    Path lcmsPath = Paths.get(StringUtils.upToLastDot(lcmsPathStr) + ".mzBIN_all");

    if (deleteMzbinAll) {
      lcmsPath.toFile().deleteOnExit();
    }

    if (!Files.exists(lcmsPath) || !Files.isRegularFile(lcmsPath) || !Files.isReadable(lcmsPath)) {
      System.err.println("Failed to find " + lcmsPath.toAbsolutePath());
      System.exit(1);
    }

    System.out.println("Found " + lcmsPath.toAbsolutePath() + ".");

    String runName = StringUtils.upToLastDot(lcmsPath.getFileName().toString());

    if (!Files.exists(psmPath) || !Files.isRegularFile(psmPath) || !Files.isReadable(psmPath)) {
      System.err.println(psmPath.toAbsolutePath() + " does not exist. Failed to write " + outputPath.toAbsolutePath());
      System.exit(1);
    }

    Set<Integer> scanNumsToExclude = readPsm(psmPath, runName, probabilityThreshold);

    System.out.println("Found " + scanNumsToExclude.size() + " scans to exclude.");

    List<IScan> iScans = new ArrayList<>();
    MZBINFile mzbinFile = new MZBINFile(1, lcmsPath.toFile(), true);
    for (MZBINFile.MZBINSpectrum mzbinSpectrum : mzbinFile.specs) {
      if (!scanNumsToExclude.contains(mzbinSpectrum.scanNum)) {
        iScans.add(mzbinSpectrum.toIScan());
      }
    }

    writeMzML(lcmsPathStr, runName, outputPath.toAbsolutePath().toString(), iScans);
  }

  private static Set<Integer> readPsm(Path psmPath, String runName, float probabilityThreshold) throws Exception {
    Set<Integer> scanNumsToExclude = new HashSet<>();
    BufferedReader reader = new BufferedReader(Files.newBufferedReader(psmPath));
    String line;
    int scanNameIdx = -1;
    int probabilityThresholdIdx = -1;
    while ((line = reader.readLine()) != null) {
      String[] split = line.split("\t");
      if (line.startsWith("Spectrum\tSpectrum File")) {
        for (int i = 0; i < split.length; i++) {
          if (split[i].equals("Spectrum")) {
            scanNameIdx = i;
          }
          if (split[i].equals("PeptideProphet Probability")) {
            probabilityThresholdIdx = i;
          }
        }
        continue;
      }

      if (scanNameIdx < 0 || probabilityThresholdIdx < 0) {
        System.err.println("Failed to find scan name or probability threshold in " + psmPath.toAbsolutePath());
        System.exit(1);
      }

      Matcher matcher = pattern.matcher(split[scanNameIdx].trim());
      if (matcher.find()) {
        if (matcher.group(1).equals(runName) && Float.parseFloat(split[probabilityThresholdIdx]) > probabilityThreshold) {
          scanNumsToExclude.add(Integer.parseInt(matcher.group(2)));
        }
      } else {
        System.err.println("Failed to parse scan name " + split[scanNameIdx].trim());
        System.exit(1);
      }
    }
    reader.close();

    return scanNumsToExclude;
  }

  private static void writeMzML(String sourceFilePath, String runName, String outputPath, List<IScan> iScanList) throws Exception {
    List<MsSoftware> softwareList = new ArrayList<>(1);
    softwareList.add(new MsSoftware("FragPipe", com.dmtavt.fragpipe.Version.version()));

    ProcessingMethod processingMethod = new ProcessingMethod(0, "FragPipe");
    processingMethod.addParams("FragPipe pre-processing", "");
    processingMethod.addParams("filter out identified scans", "");

    List<ProcessingMethodType> processingMethodTypeList = new ArrayList<>(1);
    processingMethodTypeList.add(processingMethod.processingMethodType);

    Map<String, Instrument> instrumentMap = new HashMap<>();
    instrumentMap.put("unknown", new Instrument());

    MZMLWriter mzmlWriter = new MZMLWriter(softwareList, "FragPipe", processingMethodTypeList, 1, true);
    mzmlWriter.writeMzML(sourceFilePath, runName, "MS:1000768", iScanList.toArray(new IScan[0]), outputPath, null, instrumentMap); // Pretend that the scans are from Thermo. Make it consistent with the scan ID format.
  }
}
