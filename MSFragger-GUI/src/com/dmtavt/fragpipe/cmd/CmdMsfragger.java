package com.dmtavt.fragpipe.cmd;

import static com.github.chhh.utils.PathUtils.testFilePath;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.api.InputLcmsFile;
import com.dmtavt.fragpipe.api.PyInfo;
import com.dmtavt.fragpipe.exceptions.NoStickyException;
import com.dmtavt.fragpipe.messages.NoteConfigPython;
import com.dmtavt.fragpipe.process.ProcessManager;
import com.dmtavt.fragpipe.tools.dbsplit.DbSplit2;
import com.dmtavt.fragpipe.tools.enums.CleavageType;
import com.dmtavt.fragpipe.tools.enums.FraggerOutputType;
import com.dmtavt.fragpipe.tools.enums.MassTolUnits;
import com.dmtavt.fragpipe.tools.enums.PrecursorMassTolUnits;
import com.dmtavt.fragpipe.tools.fragger.MsfraggerParams;
import com.github.chhh.utils.OsUtils;
import com.github.chhh.utils.StringUtils;
import com.github.chhh.utils.UsageTrigger;
import java.awt.Component;
import java.io.File;
import java.io.FileOutputStream;
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
import org.apache.commons.io.FileUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CmdMsfragger extends CmdBase {
  private static final Logger log = LoggerFactory.getLogger(CmdMsfragger.class);
  public static final String NAME = "MSFragger";

  private static volatile FileFilter ff = null;
  private static volatile Predicate<File> supportedFilePredicate = null;
  private static final Path PATH_NONE = Paths.get("");
  private static volatile Path pathThermo = PATH_NONE;
  private static volatile Path pathBruker = PATH_NONE;
  private static final List<String> timsdataPattern = Arrays.asList("^timsdata.*\\.dll", "^libtimsdata.*\\.so");
  private final FraggerOutputType fraggerOutputType;
  private MsfraggerParams paramsDda;
  private MsfraggerParams paramsDia;
  private MsfraggerParams paramsGpfDia;

  public CmdMsfragger(boolean isRun, Path workDir, FraggerOutputType fraggerOutputType) {
    super(isRun, workDir);
    this.fraggerOutputType = fraggerOutputType;
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
        if (f.getDataType().contentEquals("DIA")) {
          if (paramsDia == null) {
            maxRank = 5; // The report_topN_rank is 5 by default for DIA data.
          } else {
            maxRank = paramsDia.getOutputReportTopN();
          }
        } else if (f.getDataType().contentEquals("GPF-DIA")) {
          if (paramsGpfDia == null) {
            maxRank = 3; // The report_topN_rank is 3 by default for GPF-DIA data.
          } else {
            maxRank = paramsGpfDia.getOutputReportTopN();
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
    Path local;
    synchronized (CmdMsfragger.class) {
      Path rel = Paths.get("ext/bruker");
      List<Path> dirs = searchLocations.stream()
          .map(path -> Files.isDirectory(path) ? path : path.getParent()).distinct().collect(
              Collectors.toList());
      List<Path> locs = createRelSearchPaths(dirs, rel);
      pathBruker = local = searchExtLibsByPattern(locs, timsdataPattern.stream().map(Pattern::compile).collect(Collectors.toList()));
    }
    return local;
  }

  public static Path searchExtLibsThermo(List<Path> searchLocations) {
    Path local = pathThermo;
    if (PATH_NONE.equals(local)) {
      synchronized (CmdMsfragger.class) {
        local = pathThermo;
        if (PATH_NONE.equals(local)) {
          Path rel = Paths.get("ext/thermo");
          List<String> files = Arrays.asList(
              "ThermoFisher.CommonCore.Data.dll",
              "ThermoFisher.CommonCore.RawFileReader.dll",
              "BatmassIoThermoServer",
              "BatmassIoThermoServer.exe"
          );
          List<Path> dirs = searchLocations.stream()
              .map(path -> Files.isDirectory(path) ? path : path.getParent()).distinct().collect(
                  Collectors.toList());
          List<Path> locs = createRelSearchPaths(dirs, rel);
          pathThermo = local = searchExtLibsByPath(locs, files.stream().map(Paths::get).collect(Collectors.toList()));
        }
      }
    }
    return local;
  }

  private static List<Path> createRelSearchPaths(List<Path> searchLocations, Path rel) {
    List<Path> locs = new ArrayList<>(searchLocations);
    searchLocations.forEach(p -> {
      if (p != null) {
        if (Files.isDirectory(p)) {
          locs.add(p.resolve(rel));
        } else {
          Path parent = p.getParent();
          if (parent != null)
          locs.add(parent.resolve(rel));
        }
      }
    });
    return locs;
  }

  public boolean configure(Component comp, boolean isDryRun, Path jarFragpipe, UsageTrigger binFragger, String pathFasta, MsfraggerParams params, int numSlices, int ramGb, List<InputLcmsFile> lcmsFiles, final String decoyTag, boolean hasDda, boolean hasDia, boolean hasGpfDia, boolean isRunDiaU) {

    initPreConfig();

    final boolean isSlicing = numSlices > 1;
    if (isSlicing) {
      // slicing requested
      if (!DbSplit2.get().isInitialized()) {
        JOptionPane.showMessageDialog(comp,
            "MSFragger: database splitting in more than 1 chunk.\n"
                + "However not all preconditions for enabling slicing were met.\n"
                + "Check the bottom of \"Config\" tab for details.",
            "Error", JOptionPane.ERROR_MESSAGE);
        return false;
      }
    }

    if (StringUtils.isNullOrWhitespace(binFragger.getBin())) {
      JOptionPane
          .showMessageDialog(comp, "Binary for running Fragger can not be an empty string.\n",
              "Error", JOptionPane.ERROR_MESSAGE);
      return false;
    }
    if (testFilePath(binFragger.getBin(), "") == null) {
      JOptionPane
          .showMessageDialog(comp, "Binary for running Fragger not found or could not be run.\n"
                  + "Neither on PATH, nor in the working directory",
              "Error", JOptionPane.ERROR_MESSAGE);
      return false;
    }

    boolean isThermoRaw = lcmsFiles.stream().anyMatch(f -> f.getPath().toString().toLowerCase().endsWith(".raw"));
    if (isThermoRaw) {
      Path fraggerJarLoc = Paths.get(binFragger.getBin()).getParent();
      Path libs = searchExtLibsThermo(Collections.singletonList(fraggerJarLoc));
      if (libs == null) {
        JOptionPane
            .showMessageDialog(comp, "Thermo RAW files were used as input.\n"
                    + "'ext/thermo' folder was not found next to MSFragger jar file.\n"
                    + "You can obtain it by upgrading your MSFragger from the Config tab.\n",
                "Libraries missing", JOptionPane.ERROR_MESSAGE);
        return false;
      }
    }

    // Fasta file
    if (pathFasta == null) {
      JOptionPane.showMessageDialog(comp, "Fasta file path (Fragger) can't be empty",
          "Error", JOptionPane.ERROR_MESSAGE);
      return false;
    }

    if ((hasDia || hasGpfDia) && !isRunDiaU && params.getNumEnzymeTermini() == CleavageType.NONSPECIFIC) {
      JOptionPane.showMessageDialog(comp, "MSFragger cannot perform nonspecific search for DIA or GPF-DIA data.\nPlease enable DIA-Umpire instead.",
          "Error", JOptionPane.ERROR_MESSAGE);
      return false;
    }

    // Search parameter file
    params.setDatabaseName(pathFasta);
    params.setDecoyPrefix(decoyTag);
    Path savedDdaParamsPath = (hasDia || hasGpfDia) ? wd.resolve("fragger_dda.params") : wd.resolve("fragger.params");
    Path savedDiaParamsPath = wd.resolve("fragger_dia.params");
    Path savedGpfDiaParamsPath = wd.resolve("fragger_gpfdia.params");

    paramsDda = new MsfraggerParams(params);
    paramsDia = new MsfraggerParams(params);
    paramsGpfDia = new MsfraggerParams(params);

    paramsDda.setDataType(0);
    adjustDiaParams(params, paramsDia, "DIA");
    adjustDiaParams(params, paramsGpfDia, "GPF-DIA");

    if (!isDryRun) {
      try {
        if (hasDda || isRunDiaU) {
          paramsDda.save(new FileOutputStream(savedDdaParamsPath.toFile()));
        }
        if (hasDia && !isRunDiaU) {
          paramsDia.save(new FileOutputStream(savedDiaParamsPath.toFile()));
        }
        if (hasGpfDia && !isRunDiaU) {
          paramsGpfDia.save(new FileOutputStream(savedGpfDiaParamsPath.toFile()));
        }

        // cache the params
        params.save();
      } catch (IOException ex) {
        JOptionPane.showMessageDialog(comp,
            "Could not save fragger_*.params file to working dir.\n",
            "Error", JOptionPane.ERROR_MESSAGE);
        return false;
      }
    }

    // 32k symbols splitting for regular command.
    // But for slicing it's all up to the python script.
    // final int commandLenLimit = isSlicing ? Integer.MAX_VALUE : 32000;
    final int commandLenLimit = 32000; // Make is a little bit smaller than 1 << 15 to make sure that it won't crash.

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
        JOptionPane.showMessageDialog(comp,
            "DbSplit was enabled, but Python was not configured.",
            "Error", JOptionPane.ERROR_MESSAGE);
        return false;
      }
    }

    Map<String, List<InputLcmsFile>> t = new TreeMap<>();

    for (InputLcmsFile inputLcmsFile : lcmsFiles) {
      if (inputLcmsFile.getDataType().contentEquals("DDA")) {
        List<InputLcmsFile> tt = t.get("DDA");
        if (tt == null) {
          tt = new ArrayList<>();
          tt.add(inputLcmsFile);
          t.put("DDA", tt);
        } else {
          tt.add(inputLcmsFile);
        }
      } else if (inputLcmsFile.getDataType().contentEquals("DIA")) {
        List<InputLcmsFile> tt = t.get("DIA");
        if (tt == null) {
          tt = new ArrayList<>();
          tt.add(inputLcmsFile);
          t.put("DIA", tt);
        } else {
          tt.add(inputLcmsFile);
        }
      } else if (inputLcmsFile.getDataType().contentEquals("GPF-DIA")) {
        List<InputLcmsFile> tt = t.get("GPF-DIA");
        if (tt == null) {
          tt = new ArrayList<>();
          tt.add(inputLcmsFile);
          t.put("GPF-DIA", tt);
        } else {
          tt.add(inputLcmsFile);
        }
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

        // Execution order after sorting: DDA, DIA, GPF-DIA. MSFragger would stop if there were wide isolation windows in DDA mode, which makes it better to let DDA be executed first.
        if (e.getKey().contentEquals("DDA")) {
          cmd.add(savedDdaParamsPath.toString());
        } else if (e.getKey().contentEquals("DIA")) {
          cmd.add(savedDiaParamsPath.toString());
        } else if (e.getKey().contentEquals("GPF-DIA")) {
          cmd.add(savedGpfDiaParamsPath.toString());
        }

        // check if the command length is ok so far
        sb.append(String.join(" ", cmd));
        if (sb.length() > commandLenLimit) {
          JOptionPane.showMessageDialog(comp,
              "MSFragger command line length too large even for a single file.",
              "Error", JOptionPane.ERROR_MESSAGE);
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
              Path pepxmlAsCreatedByFragger = f.getPath().getParent().resolve(pepxmlFn);
              if (!pepxmlAsCreatedByFragger.equals(pepxmlWhereItShouldBe)) {
                List<ProcessBuilder> pbsMove = ToolingUtils
                    .pbsMoveFiles(jarFragpipe, pepxmlWhereItShouldBe.getParent(), true,
                        Collections.singletonList(pepxmlAsCreatedByFragger));
                pbis.addAll(PbiBuilder.from(pbsMove, NAME + " move pepxml"));
              }
            }
          }

          if (params.getShiftedIons() || fraggerOutputType.valueInParamsFile().contains("tsv")) {
            List<Path> tsvWhereItShouldBeList = mapLcmsToTsv.get(f);
            for (Path tsvWhereItShouldBe : tsvWhereItShouldBeList) {
              String tsvFn = tsvWhereItShouldBe.getFileName().toString();
              Path tsvAsCreatedByFragger = f.getPath().getParent().resolve(tsvFn);
              if (!tsvAsCreatedByFragger.equals(tsvWhereItShouldBe)) {
                List<ProcessBuilder> pbsMove = ToolingUtils
                    .pbsMoveFiles(jarFragpipe, tsvWhereItShouldBe.getParent(), true,
                        Collections.singletonList(tsvAsCreatedByFragger));
                pbis.addAll(PbiBuilder.from(pbsMove, NAME + " move tsv"));
              }
            }
          }

          if (!f.getDataType().contentEquals("DDA") || fraggerOutputType.valueInParamsFile().contains("pin")) {
            List<Path> pinWhereItShouldBeList = mapLcmsToPin.get(f);
            for (Path pinWhereItShouldBe : pinWhereItShouldBeList) {
              String pinFn = pinWhereItShouldBe.getFileName().toString();
              Path pinAsCreatedByFragger = f.getPath().getParent().resolve(pinFn);
              if (!pinAsCreatedByFragger.equals(pinWhereItShouldBe)) {
                List<ProcessBuilder> pbsMove = ToolingUtils
                    .pbsMoveFiles(jarFragpipe, pinWhereItShouldBe.getParent(), true,
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

  private void adjustDiaParams(MsfraggerParams params, MsfraggerParams paramsNew, String dataType) {
    paramsNew.setReportAlternativeProteins(true);
    paramsNew.setShiftedIons(false);
    paramsNew.setLabileSearchMode("off");
    paramsNew.setDeltamassAllowedResidues("all");
    paramsNew.setRemovePrecursorPeak(0);
    if (paramsNew.getCalibrateMass() > 1) {
      paramsNew.setCalibrateMass(1);
    }
    paramsNew.setMassDiffToVariableMod(0);
    paramsNew.setIsotopeError("0");
    paramsNew.setMassOffsets("0");
    paramsNew.setUseTopNPeaks(300);
    paramsNew.setMinimumRatio(0);
    paramsNew.setIntensityTransform(1);

    if (dataType.contentEquals("DIA")) {
      paramsNew.setDataType(1);
      paramsNew.setOutputReportTopN(Math.max(5, params.getOutputReportTopN()));
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
      paramsNew.setOutputReportTopN(Math.max(3, params.getOutputReportTopN()));
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
