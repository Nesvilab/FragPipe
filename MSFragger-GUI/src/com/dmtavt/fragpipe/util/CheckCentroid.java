package com.dmtavt.fragpipe.util;

import static umich.ms.fileio.filetypes.mzbin.Utils.shortArray;

import java.util.Arrays;
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
    long time = System.currentTimeMillis();
    try {
      if (!isCentroid(args[0].trim(), Integer.parseInt(args[1]))) {
        System.err.println(args[0].trim() + " has non-centroid scans. Please re-convert it with peakPeaking (https://msfragger.nesvilab.org/tutorial_convert.html).");
        System.exit(1);
      }
    } catch (Exception e) {
      e.printStackTrace();
      System.exit(1);
    }
    System.out.printf("Done in %d ms.%n", System.currentTimeMillis() - time);
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
    source.setNumThreadsForParsing(nThreads);
    scans.setDataSource(source);
    scans.loadData(LCMSDataSubset.STRUCTURE_ONLY);

    TreeMap<Integer, IScan> num2scan = scans.getMapNum2scan();
    boolean ms1Centroided = false;
    boolean ms2Centroided = false;
    for (final Map.Entry<Integer, IScan> scanNum_iscan : num2scan.entrySet()) {
      final IScan scan = scanNum_iscan.getValue();

      if (scan.isCentroided() != null) {
        if (!scan.isCentroided()) {
          return false;
        } else {
          if (scan.getMsLevel() == 1) {
            ms1Centroided = true;
          } else if (scan.getMsLevel() == 2) {
            ms2Centroided = true;
          }
          if (ms1Centroided && ms2Centroided) {
            return true;
          }
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
      } else {
        if (scan.getMsLevel() == 1) {
          ms1Centroided = true;
        } else if (scan.getMsLevel() == 2) {
          ms2Centroided = true;
        }
        if (ms1Centroided && ms2Centroided) {
          return true;
        }
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
