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

package com.dmtavt.fragpipe.cmd;

import static com.github.chhh.utils.PathUtils.testFilePath;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.api.InputLcmsFile;
import com.dmtavt.fragpipe.api.PyInfo;
import com.dmtavt.fragpipe.exceptions.NoStickyException;
import com.dmtavt.fragpipe.messages.NoteConfigPython;
import com.dmtavt.fragpipe.tools.dbsplit.DbSplit2;
import com.dmtavt.fragpipe.tools.enums.FraggerOutputType;
import com.dmtavt.fragpipe.tools.enums.MassTolUnits;
import com.dmtavt.fragpipe.tools.enums.PrecursorMassTolUnits;
import com.dmtavt.fragpipe.tools.fragger.Mod;
import com.dmtavt.fragpipe.tools.fragger.MsfraggerParams;
import com.github.chhh.utils.OsUtils;
import com.github.chhh.utils.StringUtils;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.UsageTrigger;
import java.awt.Component;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.TreeMap;
import java.util.function.Predicate;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import javax.swing.JOptionPane;
import javax.swing.filechooser.FileFilter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CmdMsfragger extends CmdBase {

  private static final Logger log = LoggerFactory.getLogger(CmdMsfragger.class);
  private static final Pattern pattern = Pattern.compile("[A-Znc\\[\\]\\^\\*]+");
  public static final String NAME = "MSFragger";

  private static volatile FileFilter ff = null;
  private static volatile Predicate<File> supportedFilePredicate = null;
  private static final Path PATH_NONE = Paths.get("");
  private static volatile Path pathThermo = PATH_NONE;
  private static volatile Path pathBruker = PATH_NONE;
  private static final List<String> timsdataPattern = Arrays.asList("^timsdata.*\\.dll", "^libtimsdata.*\\.so");
  private final FraggerOutputType fraggerOutputType;
  private final int outputReportTopNDia1;
  private final int outputReportTopNDdaPlus;
  private MsfraggerParams paramsDda;
  private MsfraggerParams paramsDdaPlus;
  private MsfraggerParams paramsDia;
  private MsfraggerParams paramsGpfDia;

  public CmdMsfragger(boolean isRun, Path workDir, FraggerOutputType fraggerOutputType, int outputReportTopNDia1, int outputReportTopNDdaPlus) {
    super(isRun, workDir);
    this.fraggerOutputType = fraggerOutputType;
    this.outputReportTopNDia1 = outputReportTopNDia1;
    this.outputReportTopNDdaPlus = outputReportTopNDdaPlus;
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  private String getPepxmlFn(InputLcmsFile f, String ext, int rank) {
    if (rank > 0) {
      return StringUtils.upToLastDot(f.getPath().getFileName().toString()) + "_rank" + rank + "." + ext;
    } else {
      return StringUtils.upToLastDot(f.getPath().getFileName().toString()) + "." + ext;
    }
  }

  public Map<InputLcmsFile, List<Path>> outputs(List<InputLcmsFile> inputs, String ext, Path workDir) {
    Map<InputLcmsFile, List<Path>> m = new HashMap<>();
    for (InputLcmsFile f : inputs) {
      if (!f.getDataType().contentEquals("DDA") && !ext.contentEquals("tsv") && !ext.contentEquals("pin")) {
        int maxRank = 5;
        if (f.getDataType().contentEquals("DIA") || f.getDataType().contentEquals("DIA-Lib")) {
          if (paramsDia == null) {
            maxRank = outputReportTopNDia1;
          } else {
            maxRank = paramsDia.getOutputReportTopN();
          }
        } else if (f.getDataType().contentEquals("GPF-DIA")) {
          if (paramsGpfDia == null) {
            maxRank = outputReportTopNDia1;
          } else {
            maxRank = paramsGpfDia.getOutputReportTopN();
          }
        } else if (f.getDataType().contentEquals("DDA+")) {
          if (paramsDdaPlus == null) {
            maxRank = outputReportTopNDdaPlus;
          } else {
            maxRank = paramsDdaPlus.getOutputReportTopN();
          }
        }

        for (int rank = 1; rank <= maxRank; ++rank) {
          String pepxmlFn = getPepxmlFn(f, ext, rank);
          List<Path> t = m.get(f);
          if (t == null) {
            t = new ArrayList<>();
            t.add(f.outputDir(workDir).resolve(pepxmlFn));
            m.put(f, t);
          } else {
            t.add(f.outputDir(workDir).resolve(pepxmlFn));
          }
        }
      } else {
        String pepxmlFn = getPepxmlFn(f, ext, 0);
        List<Path> tempList = new ArrayList<>(1);
        tempList.add(f.outputDir(workDir).resolve(pepxmlFn));
        m.put(f, tempList);
      }
    }
    return m;
  }

//  public static Predicate<String> getRawLcmsFnPredicate(List<Path> searchPaths) {
//    final List<String> exts = new ArrayList<>(Arrays.asList(".mzml", ".mzxml", ".mgf"));
//    if (searchExtLibsBruker(searchPaths) != null) {
//      exts.add(".d");
//    }
//    if (searchExtLibsThermo(searchPaths) != null) {
//      exts.add(".raw");
//    }
//    return fn -> {
//      for (String ext : exts) {
//        if (fn.endsWith(ext)) {
//          return true;
//        }
//      }
//      return false;
//    };
//  }

  /**
   * @param searchPaths Paths were to search for native libraries. Pass in the location of
   * MSFragger.jar.
   */
  public static FileFilter getFileChooserFilter(List<Path> searchPaths) {
    FileFilter local = ff;
    if (local == null) {
      synchronized (CmdMsfragger.class) {
        local = ff;
        if (local == null) {
          ff = local = createFileChooserFilter(searchPaths);
        }
      }
    }
    return local;
  }

  public static Predicate<File> getSupportedFilePredicate(List<Path> searchPaths) {
    Predicate<File> local;
    synchronized (CmdMsfragger.class) {
      final GetSupportedExts exts = new GetSupportedExts(searchPaths).invoke();
      supportedFilePredicate = local = file -> {
        String fnLoCase = file.getName().toLowerCase();
        for (String ext : exts.exts) {
          if (fnLoCase.endsWith(ext)) {
            return true;
          }
        }
        return false;
      };
    }
    return local;
  }

  private static javax.swing.filechooser.FileFilter createFileChooserFilter(List<Path> searchPaths) {
    GetSupportedExts getSupportedExts = new GetSupportedExts(searchPaths).invoke();
    List<String> desc = getSupportedExts.getDesc();
    List<String> exts = getSupportedExts.getExts();

    javax.swing.filechooser.FileFilter filter = new javax.swing.filechooser.FileFilter() {
      @Override
      public boolean accept(File f) {
        if (f.isDirectory()) {
          return true;
        }
        String fnLoCase = f.getName().toLowerCase();
        for (String ext : exts) {
          if (fnLoCase.endsWith(ext)) {
            return true;
          }
        }
        return false;
      }

      @Override
      public String getDescription() {
        return String.format("LCMS files (%s)", String.join(", ", desc));
      }
    };

    return filter;
  }

  /**
   * Search for the presence of library files in relative paths.
   *
   * @return Path where the 'ext' folder with needed libraries was found, otherwise null.
   */
  private static Path searchExtLibsByPath(List<Path> searchLocations, List<Path> anyPresent) {
    Optional<Path> found = searchLocations.stream()
        .filter(loc -> anyPresent.stream()
            .anyMatch(rel -> loc.resolve(rel).toFile().exists())).findFirst();
    return found.orElse(null);
  }

  /**
   * Search for the presence of library files in relative paths.
   *
   * @return Path where the 'ext' folder with needed libraries was found, otherwise null.
   */
  private static Path searchExtLibsByPattern(List<Path> searchLocations, List<Pattern> fileNamePattern) {
    Optional<Path> found = searchLocations.stream()
        .filter(loc -> fileNamePattern.stream().allMatch(re -> {
          try {
            return Files.list(loc).anyMatch(file ->
                re.matcher(file.getFileName().toString()).find());
          } catch (IOException ignored) {
            return false;
          }
        }))
        .findFirst();
    return found.orElse(null);
  }

  public static Path searchExtLibsBruker(List<Path> searchLocations) {
    synchronized (CmdMsfragger.class) {
      Path rel = Paths.get("ext/bruker");
      List<Path> dirs = searchLocations.stream()
          .map(path -> Files.isDirectory(path) ? path : path.toAbsolutePath().getParent()).distinct().collect(
              Collectors.toList());
      List<Path> locs = createRelSearchPaths(dirs, rel);
      pathBruker = searchExtLibsByPattern(locs, timsdataPattern.stream().map(Pattern::compile).collect(Collectors.toList()));
    }
    return pathBruker;
  }

  public static Path searchExtLibsThermo(List<Path> searchLocations) {
    synchronized (CmdMsfragger.class) {
      Path rel = Paths.get("ext/thermo");
      List<String> files = Arrays.asList(
          "ThermoFisher.CommonCore.Data.dll",
          "ThermoFisher.CommonCore.RawFileReader.dll",
          "BatmassIoThermoServer",
          "BatmassIoThermoServer.exe"
      );
      List<Path> dirs = searchLocations.stream()
          .map(path -> Files.isDirectory(path) ? path : path.toAbsolutePath().getParent()).distinct().collect(
              Collectors.toList());
      List<Path> locs = createRelSearchPaths(dirs, rel);
      pathThermo = searchExtLibsByPath(locs, files.stream().map(Paths::get).collect(Collectors.toList()));
    }
    return pathThermo;
  }

  private static List<Path> createRelSearchPaths(List<Path> searchLocations, Path rel) {
    List<Path> locs = new ArrayList<>(searchLocations);
    searchLocations.forEach(p -> {
      if (p != null) {
        if (Files.isDirectory(p)) {
          locs.add(p.resolve(rel));
        } else {
          Path parent = p.toAbsolutePath().getParent();
          if (parent != null)
          locs.add(parent.resolve(rel));
        }
      }
    });
    return locs;
  }

  public boolean configure(Component comp,
      boolean isDryRun,
      Path jarFragpipe,
      UsageTrigger binFragger,
      String pathFasta,
      MsfraggerParams params,
      int numSlices,
      int ramGb,
      List<InputLcmsFile> lcmsFiles,
      final String decoyTag,
      boolean hasDda,
      boolean hasDia,
      boolean hasGpfDia,
      boolean hasDiaLib,
      boolean hasDdaPlus,
      boolean isRunDiaU,
      boolean isRunDiaTracer,
      boolean writeMzbinAll) {

    initPreConfig();

    for (Mod mod : params.getVariableMods()) {
      if (mod.isEnabled && !pattern.matcher(mod.sites).matches()) {
        SwingUtils.showErrorDialog(null, "Invalid variable modification sites: " + mod.sites + ".\nPlease check the tooltip or documentations.", "Variable modification sites error");
        return false;
      }
    }

    final boolean isSlicing = numSlices > 1;
    if (isSlicing) {
      if (params.getShiftedIons()) {
        SwingUtils.showErrorDialog(comp, "<html><code>Split database</code> is incompatible with <code>localize mass shift</code>.", "Incompatible options");
        return false;
      }

      if ((!isRunDiaU && !isRunDiaTracer) && (hasDia || hasGpfDia || hasDiaLib || hasDdaPlus)) {
        SwingUtils.showErrorDialog(comp, "<html>MSFrgger-DIA <code>split database</code> is incompatible with DIA, GPF-DIA, DIA-Lib, or DDA+ data types.\n"
            + "Please set the split database to 1.\n"
            + "For DIA and DIA-Lib data types, you can also use the DIA-Umpire based DIA workflow (DIA_DIA-Umpire_SpecLib_Quant workflow), which supports the split database option.", "Incompatible options");
        return false;
      }

      if (!DbSplit2.get().isInitialized()) {
        if (Fragpipe.headless) {
          log.error("MSFragger: database splitting in more than 1 chunk. However not all preconditions for enabling slicing were met, check that Python is installed and meets minimum version requirements.");
        } else {
          JOptionPane.showMessageDialog(comp,
              "MSFragger: database splitting in more than 1 chunk.\n"
                  + "However not all preconditions for enabling slicing were met.\n"
                  + "Check the bottom of 'Config' tab for details.",
              "Error", JOptionPane.ERROR_MESSAGE);
        }
        return false;
      }
    }

    if (StringUtils.isNullOrWhitespace(binFragger.getBin())) {
      if (Fragpipe.headless) {
        log.error("Binary for running MSFragger can not be an empty string.");
      } else {
        JOptionPane.showMessageDialog(comp, "Binary for running MSFragger can not be an empty string.\n", "Error", JOptionPane.ERROR_MESSAGE);
      }
      return false;
    }
    if (testFilePath(binFragger.getBin(), "") == null) {
      if (Fragpipe.headless) {
        log.error("Binary for running MSFragger not found or could not be run. Neither on PATH, nor in the working directory");
      } else {
        JOptionPane.showMessageDialog(comp, "Binary for running MSFragger not found or could not be run.\nNeither on PATH, nor in the working directory", "Error", JOptionPane.ERROR_MESSAGE);
      }
      return false;
    }

    boolean isThermoRaw = lcmsFiles.stream().anyMatch(f -> f.getPath().toString().toLowerCase().endsWith(".raw"));
    if (isThermoRaw) {
      Path fraggerJarLoc = Paths.get(binFragger.getBin()).toAbsolutePath().getParent();
      Path libs = searchExtLibsThermo(Collections.singletonList(fraggerJarLoc));
      if (libs == null) {
        if (Fragpipe.headless) {
          log.error("Thermo RAW files were used as input. 'ext/thermo' folder was not found next to MSFragger jar file.");
        } else {
          JOptionPane.showMessageDialog(comp, "Thermo RAW files were used as input.\n"
                  + "'ext/thermo' folder was not found next to MSFragger jar file.\n"
                  + "You can obtain it by upgrading your MSFragger from the Config tab.\n",
              "Libraries missing", JOptionPane.ERROR_MESSAGE);
        }
        return false;
      }
    }

    // Fasta file
    if (pathFasta == null) {
      if (Fragpipe.headless) {
        log.error("Fasta file path can't be empty");
      } else {
        JOptionPane.showMessageDialog(comp, "FASTA file path can't be empty", "Error", JOptionPane.ERROR_MESSAGE);
      }
      return false;
    }

    // localize delta mass is required for fragment remainder ions (relevant in labile modes only)
    if (!params.getLabileSearchMode().equals(MsfraggerParams.GLYCO_OPTION_off)) {
      if (params.getRemainderMasses().length() > 0 && !params.getShiftedIons()) {
        if (Fragpipe.headless) {
          log.error("Fragment remainder masses are specified for MSFragger, but localize_delta_mass is disabled. Please enable it to continue (it is required for fragment remainder masses).");
        } else {
          JOptionPane.showMessageDialog(comp, "Fragment remainder masses are specified for MSFragger, but localize_delta_mass is disabled. Please enable it to continue (it is required for fragment remainder masses).", "Error", JOptionPane.ERROR_MESSAGE);
        }
        return false;
      }
    }

    // check if 0 is in the mass offset list if it is a mass offset search
    String[] offsets = params.getMassOffsets().trim().split("[\\s/]+");
    boolean foundZero = Arrays.stream(offsets).map(Float::parseFloat).anyMatch(offset -> offset == 0);
    if (!foundZero) {
      if (Fragpipe.headless) {
        log.warn("Warning! 0 is not included in the MSFragger mass offset list. This can cause unexpected behavior. Please add 0 to the list unless you are sure you know what you are doing.");
      } else {
        JOptionPane.showMessageDialog(comp, "Warning! 0 is not included in the MSFragger mass offset list. This can cause unexpected behavior. Please add 0 to the list unless you are sure you know what you are doing.", "Warning", JOptionPane.WARNING_MESSAGE);
      }
    }

    // Search parameter file
    params.setDatabaseName(pathFasta);
    params.setDecoyPrefix(decoyTag);
    params.setWriteMzbinAll(writeMzbinAll);
    Path savedDdaParamsPath = (hasDia || hasGpfDia || hasDiaLib) ? wd.resolve("fragger_dda.params") : wd.resolve("fragger.params");
    Path savedDdaPlusParamsPath = wd.resolve("fragger_dda_plus.params");
    Path savedDiaParamsPath = wd.resolve("fragger_dia.params");
    Path savedGpfDiaParamsPath = wd.resolve("fragger_gpfdia.params");

    paramsDda = new MsfraggerParams(params);
    paramsDia = new MsfraggerParams(params);
    paramsGpfDia = new MsfraggerParams(params);
    paramsDdaPlus = new MsfraggerParams(params);

    paramsDda.setDataType(0);
    adjustMSFraggerParams(params, paramsDia, "DIA");
    adjustMSFraggerParams(params, paramsGpfDia, "GPF-DIA");
    adjustMSFraggerParams(params, paramsDdaPlus, "DDA+");

    if (!isDryRun) {
      try {
        if (hasDda || isRunDiaU || isRunDiaTracer) {
          paramsDda.save(Files.newOutputStream(savedDdaParamsPath));
        }
        if ((hasDia || hasDiaLib) && !isRunDiaU && !isRunDiaTracer) {
          paramsDia.save(Files.newOutputStream(savedDiaParamsPath));
        }
        if (hasGpfDia && !isRunDiaU && !isRunDiaTracer) {
          paramsGpfDia.save(Files.newOutputStream(savedGpfDiaParamsPath));
        }
        if (hasDdaPlus) {
          paramsDdaPlus.save(Files.newOutputStream(savedDdaPlusParamsPath));
        }

        // cache the params
        params.save();
      } catch (IOException ex) {
        if (Fragpipe.headless) {
          log.error("Could not save fragger_*.params file to working dir.");
        } else {
          JOptionPane.showMessageDialog(comp, "Could not save fragger_*.params file to working dir.\n", "Error", JOptionPane.ERROR_MESSAGE);
        }
        return false;
      }
    }

    // 32k symbols splitting for regular command.
    // But for slicing it's all up to the python script.
    // final int commandLenLimit = isSlicing ? Integer.MAX_VALUE : 32000;
    final int commandLenLimit = 32000; // Make is a little bit smaller than 1 << 15 to make sure that it won't crash.

    /* disable deletion of temp dir when error occurs
    if (isSlicing) {
      // schedule to always try to delete the temp dir when FragPipe finishes execution
      final String tempDirName = "split_peptide_index_tempdir";
      Path toDelete = wd.resolve(tempDirName).toAbsolutePath().normalize();
      toDelete.toFile().deleteOnExit();
      ProcessManager.addFilesToDelete(Collections.singleton(toDelete));
      try {
        if (Files.exists(toDelete)) {
          FileUtils.deleteDirectory(toDelete.toFile());
        }
      } catch (IOException e) {
        log.error("Could not delete leftover temporary directory from DB Splitting", e);
      }
    }
    */

    StringBuilder sb = new StringBuilder();

    Map<InputLcmsFile, List<Path>> mapLcmsToPepxml = outputs(lcmsFiles, "pepXML", wd);
    Map<InputLcmsFile, List<Path>> mapLcmsToTsv = outputs(lcmsFiles, "tsv", wd);
    Map<InputLcmsFile, List<Path>> mapLcmsToPin = outputs(lcmsFiles, "pin", wd);

    final List<String> javaCmd = Arrays.asList(
        Fragpipe.getBinJava(), "-jar", "-Dfile.encoding=UTF-8", "-Xmx" + ramGb + "G");
    final List<String> slicingCmd;

    if (!isSlicing) {
      slicingCmd = null;
    } else {
      try {
        NoteConfigPython configPython = Fragpipe.getSticky(NoteConfigPython.class);
        slicingCmd = Arrays.asList(
            configPython.pi.getCommand(),
            DbSplit2.get().getScriptDbslicingPath().toAbsolutePath().normalize().toString(),
            Integer.toString(numSlices),
            OsUtils.asSingleArgument(String.join(" ", javaCmd))
        );
      } catch (NoStickyException e) {
        if (Fragpipe.headless) {
          log.error("DbSplit was enabled, but Python was not configured.");
        } else {
          JOptionPane.showMessageDialog(comp, "DbSplit was enabled, but Python was not configured.", "Error", JOptionPane.ERROR_MESSAGE);
        }
        return false;
      }
    }

    Map<String, List<InputLcmsFile>> t = new TreeMap<>();

    for (InputLcmsFile inputLcmsFile : lcmsFiles) {
      if (inputLcmsFile.getDataType().contentEquals("DDA")) {
        sub(t, inputLcmsFile, "DDA");
      } else if (inputLcmsFile.getDataType().contentEquals("DIA") || inputLcmsFile.getDataType().contentEquals("DIA-Lib")) { // searching DIA and DIA-Lib together
        sub(t, inputLcmsFile, "DIA");
      } else if (inputLcmsFile.getDataType().contentEquals("GPF-DIA")) {
        sub(t, inputLcmsFile, "GPF-DIA");
      } else if (inputLcmsFile.getDataType().contentEquals("DDA+")) {
        sub(t, inputLcmsFile, "DDA+");
      } else {
        throw new IllegalStateException("Unknown data type: " + inputLcmsFile.getDataType());
      }
    }

    for (Map.Entry<String, List<InputLcmsFile>> e : t.entrySet()) {
      int fileIndex = 0;
      while (fileIndex < e.getValue().size()) {
        List<String> cmd = new ArrayList<>();
        if (isSlicing) {
          cmd.addAll(slicingCmd);
        } else {
          cmd.addAll(javaCmd);
        }
        cmd.add(binFragger.useBin());

        // Execution order after sorting: DDA, DIA, GPF-DIA, DDA+. MSFragger would stop if there were wide isolation windows in DDA mode, which makes it better to let DDA be executed first.
        if (e.getKey().contentEquals("DDA")) {
          cmd.add(savedDdaParamsPath.toString());
        } else if (e.getKey().contentEquals("DIA")) {
          cmd.add(savedDiaParamsPath.toString());
        } else if (e.getKey().contentEquals("GPF-DIA")) {
          cmd.add(savedGpfDiaParamsPath.toString());
        } else if (e.getKey().contentEquals("DDA+")) {
          cmd.add(savedDdaPlusParamsPath.toString());
        }

        // check if the command length is ok so far
        sb.append(String.join(" ", cmd));
        if (sb.length() > commandLenLimit) {
          if (Fragpipe.headless) {
            log.error("MSFragger command line length too large even for a single file.");
          } else {
            JOptionPane.showMessageDialog(comp, "MSFragger command line length too large even for a single file.", "Error", JOptionPane.ERROR_MESSAGE);
          }
          return false;
        }

        List<InputLcmsFile> addedLcmsFiles = new ArrayList<>();
        while (fileIndex < e.getValue().size()) {
          InputLcmsFile f = e.getValue().get(fileIndex);
          // if adding this file to the command line will make the command length
          // longer than the allowed maximum, stop adding files
          if (sb.length() + f.getPath().toString().length() > commandLenLimit) {
            break;
          }
          sb.append(f.getPath().toString()).append(" ");
          cmd.add(f.getPath().toString());
          addedLcmsFiles.add(f);
          fileIndex++;
        }

        ProcessBuilder pb = new ProcessBuilder(cmd);

        if (isSlicing) {
          PyInfo.modifyEnvironmentVariablesForPythonSubprocesses(pb);
          pb.environment().put("PYTHONIOENCODING", "utf-8");
          pb.environment().put("PYTHONUNBUFFERED", "true");
        }

        pb.directory(wd.toFile());

        pbis.add(PbiBuilder.from(pb));
        sb.setLength(0);

        // move the pepxml files if the output directory is not the same as where
        // the lcms files were
        for (InputLcmsFile f : addedLcmsFiles) {
          if (fraggerOutputType.valueInParamsFile().contains("pepXML")) {
            List<Path> pepxmlWhereItShouldBeList = mapLcmsToPepxml.get(f);
            if (pepxmlWhereItShouldBeList == null || pepxmlWhereItShouldBeList.isEmpty())
              throw new IllegalStateException("LCMS file mapped to no pepxml file");
            for (Path pepxmlWhereItShouldBe : pepxmlWhereItShouldBeList) {
              String pepxmlFn = pepxmlWhereItShouldBe.getFileName().toString();
              Path pepxmlAsCreatedByFragger = f.getPath().toAbsolutePath().getParent().resolve(pepxmlFn);
              if (!pepxmlAsCreatedByFragger.equals(pepxmlWhereItShouldBe)) {
                List<ProcessBuilder> pbsMove = ToolingUtils
                    .pbsMoveFiles(jarFragpipe, pepxmlWhereItShouldBe.toAbsolutePath().getParent(), true,
                        Collections.singletonList(pepxmlAsCreatedByFragger));
                pbis.addAll(PbiBuilder.from(pbsMove, NAME + " move pepxml"));
              }
            }
          }

          if (params.getShiftedIons() || fraggerOutputType.valueInParamsFile().contains("tsv")) {
            List<Path> tsvWhereItShouldBeList = mapLcmsToTsv.get(f);
            for (Path tsvWhereItShouldBe : tsvWhereItShouldBeList) {
              String tsvFn = tsvWhereItShouldBe.getFileName().toString();
              Path tsvAsCreatedByFragger = f.getPath().toAbsolutePath().getParent().resolve(tsvFn);
              if (!tsvAsCreatedByFragger.equals(tsvWhereItShouldBe)) {
                List<ProcessBuilder> pbsMove = ToolingUtils
                    .pbsMoveFiles(jarFragpipe, tsvWhereItShouldBe.toAbsolutePath().getParent(), true,
                        Collections.singletonList(tsvAsCreatedByFragger));
                pbis.addAll(PbiBuilder.from(pbsMove, NAME + " move tsv"));
              }
            }
          }

          if (!f.getDataType().contentEquals("DDA") || fraggerOutputType.valueInParamsFile().contains("pin")) {
            List<Path> pinWhereItShouldBeList = mapLcmsToPin.get(f);
            for (Path pinWhereItShouldBe : pinWhereItShouldBeList) {
              String pinFn = pinWhereItShouldBe.getFileName().toString();
              Path pinAsCreatedByFragger = f.getPath().toAbsolutePath().getParent().resolve(pinFn);
              if (!pinAsCreatedByFragger.equals(pinWhereItShouldBe)) {
                List<ProcessBuilder> pbsMove = ToolingUtils
                    .pbsMoveFiles(jarFragpipe, pinWhereItShouldBe.toAbsolutePath().getParent(), true,
                        Collections.singletonList(pinAsCreatedByFragger));
                pbis.addAll(PbiBuilder.from(pbsMove, NAME + " move pin"));
              }
            }
          }
        }
      }
    }

    isConfigured = true;
    return true;
  }

  private void sub(Map<String, List<InputLcmsFile>> t, InputLcmsFile inputLcmsFile, String dataType) {
    List<InputLcmsFile> tt = t.get(dataType);
    if (tt == null) {
      tt = new ArrayList<>(1);
      tt.add(inputLcmsFile);
      t.put(dataType, tt);
    } else {
      tt.add(inputLcmsFile);
    }
  }

  private void adjustMSFraggerParams(MsfraggerParams params, MsfraggerParams paramsNew, String dataType) {
    paramsNew.setReportAlternativeProteins(true);
    paramsNew.setShiftedIons(false);
    paramsNew.setLabileSearchMode("off");
    paramsNew.setDeltamassAllowedResidues("all");

    paramsNew.setIsotopeError("0");
    paramsNew.setUseTopNPeaks(Math.max(300, params.getUseTopNPeaks()));
    paramsNew.setMinimumRatio(0);
    paramsNew.setIntensityTransform(1);
    paramsNew.setRemovePrecursorPeak(1);

    if (dataType.contentEquals("DIA")) {
      paramsNew.setDataType(1);
      paramsNew.setOutputReportTopN(outputReportTopNDia1);
      paramsNew.setPrecursorTrueUnits(MassTolUnits.PPM);
      paramsNew.setPrecursorTrueTolerance(10);
      if (params.getPrecursorMassUnits() == PrecursorMassTolUnits.PPM) {
        paramsNew.setPrecursorMassLower(Math.max(-10, params.getPrecursorMassLower()));
        paramsNew.setPrecursorMassUpper(Math.min(10, params.getPrecursorMassUpper()));
      } else {
        paramsNew.setPrecursorMassUnits(PrecursorMassTolUnits.PPM);
        paramsNew.setPrecursorMassLower(-10.0);
        paramsNew.setPrecursorMassUpper(10.0);
      }
    } else if (dataType.contentEquals("GPF-DIA")) {
      paramsNew.setDataType(2);
      paramsNew.setOutputReportTopN(outputReportTopNDia1);
    } else if (dataType.contentEquals("DDA+")) {
      paramsNew.setDataType(3);
      paramsNew.setOutputReportTopN(outputReportTopNDdaPlus);
    }
  }


  private static class GetSupportedExts {

    private List<Path> searchPaths;
    private List<String> desc;
    private List<String> exts;

    public GetSupportedExts(List<Path> searchPaths) {
      this.searchPaths = searchPaths;
    }

    public List<String> getDesc() {
      return desc;
    }

    public List<String> getExts() {
      return exts;
    }

    public GetSupportedExts invoke() {
      desc = new ArrayList<>(Arrays.asList("mzML", "mzXML", "mgf", "mzBIN"));
      exts = new ArrayList<>(Arrays.asList(".mgf", ".mzml", ".mzxml", ".mzbin"));
//      if (searchPaths != null && !searchPaths.isEmpty()) {
//        if (searchExtLibsThermo(searchPaths) != null) {
          desc.add("Thermo RAW");
          exts.add(".raw");
//        }
//        if (searchExtLibsBruker(searchPaths) != null) {
          desc.add("Buker PASEF .d");
          exts.add(".d");
//        }
//      }
      return this;
    }
  }
}
