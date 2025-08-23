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

import static umich.ms.fileio.filetypes.mzbin.Utils.shortArray;

import java.util.Arrays;
import java.util.Locale;
import java.util.Map;
import java.util.TreeMap;
import umich.ms.datatypes.LCMSDataSubset;
import umich.ms.datatypes.scan.IScan;
import umich.ms.datatypes.scan.StorageStrategy;
import umich.ms.datatypes.scancollection.impl.ScanCollectionDefault;
import umich.ms.datatypes.spectrum.ISpectrum;
import umich.ms.fileio.filetypes.AbstractLCMSDataSource;
import umich.ms.fileio.filetypes.mzml.MZMLFile;
import umich.ms.fileio.filetypes.mzxml.MZXMLFile;

public class CheckCentroid {

  public static void main(String[] args) {
    Locale.setDefault(Locale.US);
    long time = System.nanoTime();
    try {
      if (!isCentroid(args[0].trim(), Integer.parseInt(args[1]))) {
        System.err.println(args[0].trim() + " has non-centroid scans. Please re-convert it with peakPeaking (https://fragpipe.nesvilab.org/docs/tutorial_convert.html).");
        System.exit(1);
      }
    } catch (Exception e) {
      e.printStackTrace();
      System.exit(1);
    }
    System.out.printf("Done in %.1f s.%n", (System.nanoTime() - time) * 1e-9f);
  }

  static boolean isCentroid(String spectralPath, int nThreads) throws Exception {
    String ext = spectralPath.substring(spectralPath.lastIndexOf('.') + 1);

    if (!ext.equalsIgnoreCase("mzml") && !ext.equalsIgnoreCase("mzxml")) {
      return true;
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
        return true;
    }

    source.setExcludeEmptyScans(true);
    source.setNumThreadsForParsing(nThreads > 0 ? nThreads : Runtime.getRuntime().availableProcessors());
    scans.setDataSource(source);
    scans.loadData(LCMSDataSubset.STRUCTURE_ONLY);

    TreeMap<Integer, IScan> num2scan = scans.getMapNum2scan();
    for (final Map.Entry<Integer, IScan> scanNum_iscan : num2scan.entrySet()) {
      final IScan scan = scanNum_iscan.getValue();

      if (scan.isCentroided() != null) {
        if (!scan.isCentroided()) {
          return false;
        } else {
          continue;
        }
      }

      ISpectrum spectrum = scan.fetchSpectrum();
      int clen = (spectrum == null || spectrum.getMZs() == null) ? 0 : spectrum.getMZs().length;

      if (clen <= 0) {
        continue;
      }

      // Delete zero intensity peaks.
      int idx = 0;
      float[] mzArray = new float[clen];
      for (int i = 0; i < spectrum.getMZs().length; ++i) {
        if (spectrum.getIntensities()[i] > 1e-4) {
          mzArray[idx] = (float) spectrum.getMZs()[i];
          ++idx;
        }
      }

      if (idx <= 0) {
        continue;
      }

      if (idx < clen) {
        mzArray = shortArray(mzArray, idx);
      }

      if (!isCentroid(mzArray)) {
        return false;
      }
    }
    scans.reset();
    source.close();

    return true;
  }


  private static boolean isCentroid(float[] mzArray) {
    if (mzArray.length > 1) {
      float[] ppmArray = new float[mzArray.length - 1];
      int idx = 0;
      while (idx < mzArray.length - 1) {
        ppmArray[idx] = (mzArray[idx] - mzArray[++idx]) * -1e6f / mzArray[idx];
      }
      Arrays.parallelSort(ppmArray);
      return ppmArray[ppmArray.length / 2] > 100;
    } else {
      return true;
    }
  }

}
