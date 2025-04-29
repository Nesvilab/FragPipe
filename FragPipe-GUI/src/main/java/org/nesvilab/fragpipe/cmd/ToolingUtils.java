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

package org.nesvilab.fragpipe.cmd;

import static org.apache.commons.lang3.StringUtils.getCommonPrefix;
import static org.nesvilab.utils.PathUtils.testBinaryPath;

import java.awt.Component;
import java.awt.Image;
import java.awt.Toolkit;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import javax.swing.JOptionPane;
import org.apache.commons.io.FilenameUtils;
import org.jooq.lambda.Seq;
import org.nesvilab.fragpipe.Fragpipe;
import org.nesvilab.fragpipe.FragpipeLocations;
import org.nesvilab.fragpipe.api.Bus;
import org.nesvilab.fragpipe.api.InputLcmsFile;
import org.nesvilab.fragpipe.api.LcmsFileGroup;
import org.nesvilab.fragpipe.tabs.TabWorkflow;
import org.nesvilab.utils.FileCopy;
import org.nesvilab.utils.FileDelete;
import org.nesvilab.utils.FileMove;
import org.nesvilab.utils.FileMoveWithExtension;
import org.nesvilab.utils.JarUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ToolingUtils {
  private static final Logger log = LoggerFactory.getLogger(ToolingUtils.class);
  private ToolingUtils() {}

  public static final String BATMASS_IO_JAR = "batmass-io-1.35.1.jar";
  public static final String JFREECHART_JAR = "jfreechart-1.5.3.jar";
  public static final String UNIMOD_OBO = "unimod.obo";

  public static Path getUnimodOboPath(String unimodName) throws Exception {
    final List<Path> tt = FragpipeLocations.checkToolsMissing(Seq.of(unimodName));
    if (tt == null || tt.size() != 1) {
      throw new FileNotFoundException("Could not find unimod.obo file from " + FragpipeLocations.get().getDirTools());
    }
    return tt.get(0);
  }

  /**
   * @return Full absolute normalized path to the output combined protein file.
   */
  public static Path getCombinedProtFilePath(String combinedProtFn, Path workingDir) {
    combinedProtFn = combinedProtFn.trim();
    final String ext = ".prot.xml";
    if (!combinedProtFn.toLowerCase().endsWith(ext)) {
      combinedProtFn = combinedProtFn + ext;
    }
    return workingDir.resolve(combinedProtFn).normalize().toAbsolutePath();
  }

  private enum Op {COPY, MOVE, DELETE, RENAME}

  /**
   * @param jarFragpipe Use {@link JarUtils#getCurrentJarUri()} to get that from the current Jar.
   */
  public static List<ProcessBuilder> pbsCopyFiles(Path jarFragpipe, Path dest, List<Path> files) {
    return pbsCopyMoveDeleteRenameFiles(jarFragpipe, Op.COPY, dest, false, files);
  }

  /**
   * @param jarFragpipe Use {@link JarUtils#getCurrentJarUri()} to get that from the current Jar.
   */
  public static List<ProcessBuilder> pbsMoveFiles(Path jarFragpipe, Path dest,
      boolean ignoreMissingFiles, List<Path> files) {
    return pbsCopyMoveDeleteRenameFiles(jarFragpipe, Op.MOVE, dest, ignoreMissingFiles, files);
  }

  /**
   * @param jarFragpipe Use {@link JarUtils#getCurrentJarUri()} to get that from the current Jar.
   */
  public static List<ProcessBuilder> pbsDeleteFiles(Path jarFragpipe, List<Path> files) {
    return pbsCopyMoveDeleteRenameFiles(jarFragpipe, Op.DELETE, null, false, files);
  }

  public static List<ProcessBuilder> pbsMoveFilesWithExtension(Path jarFragpipe, Path dest, Path originDir, String ext) {
    if (jarFragpipe == null) {
      throw new IllegalArgumentException("jar can't be null");
    }

    List<ProcessBuilder> pbs = new LinkedList<>();

    List<String> cmd = new ArrayList<>();
    cmd.add(Fragpipe.getBinJava());
    cmd.add("-cp");
    cmd.add(jarFragpipe.toAbsolutePath().normalize().toString());
    cmd.add(FileMoveWithExtension.class.getCanonicalName());
    cmd.add(originDir.toAbsolutePath().normalize().toString());
    cmd.add(ext);
    cmd.add(dest.toAbsolutePath().normalize().toString());
    ProcessBuilder pb = new ProcessBuilder(cmd);
    pbs.add(pb);

    return pbs;
  }

  private static List<ProcessBuilder> pbsCopyMoveDeleteRenameFiles(Path jarFragpipe, Op operation, Path dest,
                                                                   boolean ignoreMissingFiles, List<Path> files) {
    if (jarFragpipe == null) {
      throw new IllegalArgumentException("jar can't be null");
    }

    List<ProcessBuilder> pbs = new LinkedList<>();
    for (Path file : files) {
      if (Objects.equals(file.toAbsolutePath().getParent(), (dest))) {
        continue;
      }
      List<String> cmd = new ArrayList<>();
      cmd.add(Fragpipe.getBinJava());
      cmd.add("-cp");
      final String commons_io_jar_path = org.apache.commons.io.FileUtils.class.getProtectionDomain().getCodeSource().getLocation().getPath();
      cmd.add(jarFragpipe.toAbsolutePath() +
              (operation == Op.MOVE ?
                      File.pathSeparator + commons_io_jar_path :
                      ""));
      switch (operation) {
        case COPY:
          cmd.add(FileCopy.class.getCanonicalName());
          break;
        case MOVE:
        case RENAME:
          cmd.add(FileMove.class.getCanonicalName());
          break;
        case DELETE:
          cmd.add(FileDelete.class.getCanonicalName());
          break;
        default:
          throw new IllegalStateException("Unknown enum value: " + operation.toString());
      }
      if (ignoreMissingFiles) {
        cmd.add(FileMove.NO_ERR);
      }
      cmd.add(file.toAbsolutePath().normalize().toString());
      if (dest != null) {
        if (operation != Op.RENAME) {
          cmd.add(dest.resolve(file.getFileName()).toString());
        } else {
          cmd.add(dest.toString());
        }
      }
      ProcessBuilder pb = new ProcessBuilder(cmd);
      pbs.add(pb);
    }
    return pbs;
  }

  public static String getBinJava(Component errroDialogParent, String programsDir) {
    String binJava = "java";
    synchronized (ToolingUtils.class) {
      binJava = testBinaryPath(binJava, programsDir);
      if (binJava != null) {
        return binJava;
      }
    }
    if (Fragpipe.headless) {
      log.error("Java could not be found.");
    } else {
      JOptionPane.showMessageDialog(errroDialogParent, "Java could not be found.\n"
          + "please make sure you have it installed \n"
          + "and that java.exe can be found on PATH", "Error", JOptionPane.ERROR_MESSAGE);
    }

    return null;
  }

  public static List<Image> loadIcon() {
    // Icon attribution string:
    // <div>Icons made by <a href="http://www.freepik.com" title="Freepik">Freepik</a> from <a href="http://www.flaticon.com" title="Flaticon">www.flaticon.com</a> is licensed by <a href="http://creativecommons.org/licenses/by/3.0/" title="Creative Commons BY 3.0" target="_blank">CC 3.0 BY</a></div>
    List<Image> images = new ArrayList<>();
    int[] sizes = {16, 24, 32, 64, 128, 256};
    final String path = "icons/";
    final String baseName = "fragpipe-";
    final String ext = ".png";
    for (int size : sizes) {
      String location = path + baseName + size + ext;
      Image icon = Toolkit.getDefaultToolkit().getImage(Fragpipe.class.getResource(location));
      images.add(icon);
    }
    return images;
  }

  static void generateLFQExperimentAnnotation(Path wd, int type) throws Exception {
    BufferedWriter bufferedWriter = new BufferedWriter(new FileWriter(wd.resolve("experiment_annotation.tsv").toFile()));
    bufferedWriter.write("file\tsample\tsample_name\tcondition\treplicate\n");

    TabWorkflow tabWorkflow = Bus.getStickyEvent(TabWorkflow.class);
    Collection<LcmsFileGroup> ttt = tabWorkflow.getLcmsFileGroups2().values();

    boolean hasDia = false;
    Set<String> fileNameSet = new HashSet<>();
    for (LcmsFileGroup lcmsFileGroup : ttt) {
      for (InputLcmsFile inputLcmsFile : lcmsFileGroup.lcmsFiles) {
        String baseName = FilenameUtils.getBaseName(inputLcmsFile.getPath().getFileName().toString());
        fileNameSet.add(baseName);
        if (inputLcmsFile.getDataType().contentEquals("DIA") || inputLcmsFile.getDataType().contentEquals("DIA-Quant")) {
          hasDia = true;
        }
      }
    }
    String commonPrefix = getCommonPrefix(fileNameSet.toArray(new String[0]));
    int a = commonPrefix.length();

    if (ttt.size() == 1 && ttt.iterator().next().name.isEmpty()) { // There is no group info from the manifest. Parse from the file name.
      for (LcmsFileGroup lcmsFileGroup : ttt) {
        for (InputLcmsFile inputLcmsFile : lcmsFileGroup.lcmsFiles) {
          if (filterRun(hasDia, inputLcmsFile)) {
            continue;
          }
          String baseName = FilenameUtils.getBaseName(inputLcmsFile.getPath().getFileName().toString());
          String sampleName = baseName.substring(a);
          String[] tt = sampleName.split("_");
          if (tt.length == 3) {
            try {
              int replicate = Integer.parseInt(tt[2]);
              bufferedWriter.write(inputLcmsFile.getPath().toAbsolutePath() + "\t" + inputLcmsFile.getPath().toAbsolutePath() + "\t" + tt[0] + "_" + tt[1] + "\t" + replicate + "\n");
            } catch (Exception ex) {
              bufferedWriter.write(inputLcmsFile.getPath().toAbsolutePath() + "\t" + inputLcmsFile.getPath().toAbsolutePath() + "\t" + tt[1] + "_" + tt[2] + "\t1\n");
            }
          } else if (tt.length == 2) {
            try {
              int replicate = Integer.parseInt(tt[1]);
              bufferedWriter.write(inputLcmsFile.getPath().toAbsolutePath() + "\t" + inputLcmsFile.getPath().toAbsolutePath() + "\t" + sampleName + "\t" + tt[0] + "\t" + replicate + "\n");
            } catch (Exception ex) {
              bufferedWriter.write(inputLcmsFile.getPath().toAbsolutePath() + "\t" + inputLcmsFile.getPath().toAbsolutePath() + "\t" + sampleName + "\t" + tt[1] + "\t1\n");
            }
          } else {
            bufferedWriter.write(inputLcmsFile.getPath().toAbsolutePath() + "\t" + inputLcmsFile.getPath().toAbsolutePath() + "\t" + sampleName + "\t" + sampleName + "\t1\n");
          }
        }
      }
    } else { // There is group info from the manifest.
      for (LcmsFileGroup lcmsFileGroup : ttt) {
        for (InputLcmsFile inputLcmsFile : lcmsFileGroup.lcmsFiles) {
          if (filterRun(hasDia, inputLcmsFile)) {
            continue;
          }
          String[] parts = inputLcmsFile.getExperiment().split("_");
          if (type == 0) {
            bufferedWriter.write(inputLcmsFile.getPath().toAbsolutePath() + "\t" + inputLcmsFile.getGroup2() + "\t" + inputLcmsFile.getGroup2() + "\t" + parts[0].trim() + "\t" + (inputLcmsFile.getReplicate() == null ? 1 : inputLcmsFile.getReplicate()) + "\n");
          } else if (type == 1) {
            bufferedWriter.write(inputLcmsFile.getPath().toAbsolutePath() + "\t" + inputLcmsFile.getPath().toAbsolutePath() + "\t" + inputLcmsFile.getGroup2() + "\t" + parts[0].trim() + "\t" + (inputLcmsFile.getReplicate() == null ? 1 : inputLcmsFile.getReplicate()) + "\n");
          }
        }
      }
    }
    bufferedWriter.close();
  }

  private static boolean filterRun(boolean hasDia, InputLcmsFile inputLcmsFile) {
    if (inputLcmsFile.getDataType().contentEquals("GPF-DIA") || inputLcmsFile.getDataType().contentEquals("DIA-Lib")) {
      return true;
    }
    if (hasDia && !(inputLcmsFile.getDataType().contentEquals("DIA") || inputLcmsFile.getDataType().contentEquals("DIA-Quant"))) {
      return true;
    }
    return false;
  }

  public static void writeIsobaricQuantExperimentAnnotation(Path wd, Map<LcmsFileGroup, Path> annotations) throws Exception {
    String line;
    BufferedWriter bufferedWriter = new BufferedWriter(new FileWriter(wd.resolve("experiment_annotation.tsv").toFile()));
    bufferedWriter.write("plex\tchannel\tsample\tsample_name\tcondition\treplicate\n");
    for (Map.Entry<LcmsFileGroup, Path> e : annotations.entrySet()) {
      BufferedReader bufferedReader = new BufferedReader(new FileReader(e.getValue().toFile()));
      while ((line = bufferedReader.readLine()) != null) {
        line = line.trim();
        if (!line.isEmpty()) {
          String[] parts = line.split("\\s");
          if (parts.length < 2 || parts[1].trim().equalsIgnoreCase("na")) {
            continue;
          }
          String[] parts2 = parts[1].trim().split("_");
          if (parts2.length == 3) {
            try {
              int replicate = Integer.parseInt(parts2[2]);
              bufferedWriter.write(e.getKey().name + "\t" + parts[0].trim() + "\t" + parts[1].trim() + "\t" + parts2[0].trim() + "\t" + parts2[1].trim() + "\t" + replicate + "\n");
            } catch (Exception ex) {
              bufferedWriter.write(e.getKey().name + "\t" + parts[0].trim() + "\t" + parts[1].trim() + "\t" + parts2[0].trim() + "_" + parts2[1].trim() + "\t" + parts2[2].trim() + "\t1\n");
            }
          } else if (parts2.length == 2) {
            try {
              int replicate = Integer.parseInt(parts2[1]);
              bufferedWriter.write(e.getKey().name + "\t" + parts[0].trim() + "\t" + parts[1].trim() + "\t" + parts2[0].trim() + "\t" + parts2[0].trim() + "\t" + replicate + "\n");
            } catch (NumberFormatException ex) {
              bufferedWriter.write(e.getKey().name + "\t" + parts[0].trim() + "\t" + parts[1].trim() + "\t" + parts2[0].trim() + "\t" + parts2[1].trim() + "\t1\n");
            }
          } else {
            bufferedWriter.write(e.getKey().name + "\t" + parts[0].trim() + "\t" + parts[1].trim() + "\t" + parts[1].trim() + "\t" + parts[1].trim() + "\t1\n");
          }
        }
      }
      bufferedReader.close();
    }
    bufferedWriter.close();
  }

}
