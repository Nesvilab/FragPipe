package umich.msfragger.cmd;

import static umich.msfragger.util.PathUtils.testFilePath;

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
import java.util.function.Predicate;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import javax.swing.JOptionPane;
import javax.swing.filechooser.FileFilter;
import org.apache.commons.io.FileUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import umich.msfragger.gui.InputLcmsFile;
import umich.msfragger.params.dbslice.DbSlice;
import umich.msfragger.params.fragger.FraggerMigPanel;
import umich.msfragger.params.fragger.MsfraggerParams;
import umich.msfragger.util.OsUtils;
import umich.msfragger.util.PythonInfo;
import umich.msfragger.util.StringUtils;
import umich.msfragger.util.UsageTrigger;

public class CmdMsfragger extends CmdBase {
  private static final Logger log = LoggerFactory.getLogger(CmdMsfragger.class);
  public static final String NAME = "MsFragger";

  private static volatile FileFilter ff = null;
  private static volatile Predicate<File> supportedFilePredicate = null;
  private static final Path PATH_NONE = Paths.get("");
  private static volatile Path pathThermo = PATH_NONE;
  private static volatile Path pathBruker = PATH_NONE;

  public CmdMsfragger(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  private String getPepxmlFn(InputLcmsFile f, String ext) {
    return StringUtils.upToLastDot(f.path.getFileName().toString()) + "." + ext;
  }

  public Map<InputLcmsFile, Path> outputs(List<InputLcmsFile> inputs, String ext, Path workDir) {
    Map<InputLcmsFile, Path> m = new HashMap<>();
    for (InputLcmsFile f : inputs) {
      String pepxmlFn = getPepxmlFn(f, ext);
      m.put(f, f.outputDir(workDir).resolve(pepxmlFn));
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
    Predicate<File> local = supportedFilePredicate;
    if (local == null) {
      synchronized (CmdMsfragger.class) {
        local = supportedFilePredicate;
        if (local == null) {
          final GetSupportedExts exts = new GetSupportedExts(searchPaths).invoke();
          supportedFilePredicate = local = new Predicate<File>() {
            @Override
            public boolean test(File file) {
              String fnLoCase = file.getName().toLowerCase();
              for (String ext : exts.exts) {
                if (fnLoCase.endsWith(ext)) {
                  return true;
                }
              }
              return false;
            }
          };
        }
      }
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
  private static Path searchExtLibsByPath(List<Path> searchLocations, List<Path> mustBePresent) {
    Optional<Path> found = searchLocations.stream()
        .filter(loc -> mustBePresent.stream()
            .allMatch(rel -> loc.resolve(rel).toFile().exists())).findFirst();
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
    Path local = pathBruker;
    if (PATH_NONE.equals(local)) {
      synchronized (CmdMsfragger.class) {
        local = pathBruker;
        if (PATH_NONE.equals(local)) {
          Path rel = Paths.get("ext/bruker");
          List<String> filenamePatterns = Arrays.asList(
              "^timsdata.*?\\.dll",
              "^libtimsdata.*?\\.so"
          );
          List<Path> dirs = searchLocations.stream()
              .map(path -> Files.isDirectory(path) ? path : path.getParent()).distinct().collect(
                  Collectors.toList());
          List<Path> locs = createRelSearchPaths(dirs, rel);
          pathBruker = local = searchExtLibsByPattern(locs, filenamePatterns.stream().map(Pattern::compile).collect(Collectors.toList()));
        }
      }
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
              "ThermoFisher.CommonCore.RawFileReader.dll"
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
    ArrayList<Path> locs = new ArrayList<>(searchLocations);
    searchLocations.forEach(p -> {
      if (Files.isDirectory(p)) {
        locs.add(p.resolve(rel));
      } else {
        locs.add(p.getParent().resolve(rel));
      }
    });
    return locs;
  }

  public boolean configure(Component comp, boolean isDryRun,
      FraggerMigPanel fp, Path jarFragpipe, UsageTrigger binFragger, String pathFasta,
      List<InputLcmsFile> lcmsFiles, final String decoyTag) {

    pbis.clear();
    final int numSlices = fp.getNumDbSlices();
    final boolean isSlicing = numSlices > 1;
    if (isSlicing) {
      // slicing requested
      if (!DbSlice.get().isInitialized()) {
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

    boolean isThermoRaw = lcmsFiles.stream().anyMatch(f -> f.path.toString().toLowerCase().endsWith(".raw"));
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

    // Search parameter file
    MsfraggerParams params = fp.getParams();
    params.setDatabaseName(pathFasta);
    params.setDecoyPrefix(decoyTag);
    Path savedParamsPath = wd.resolve(MsfraggerParams.CACHE_FILE);
    if (!isDryRun) {
      try {
        params.save(new FileOutputStream(savedParamsPath.toFile()));
        // cache the params
        params.save();
      } catch (IOException ex) {
        JOptionPane.showMessageDialog(comp,
            "Could not save fragger.params file to working dir.\n",
            "Error", JOptionPane.ERROR_MESSAGE);
        return false;
      }
    }

    final int ramGb = fp.getRamGb() > 0 ? fp.getRamGb() :
        (int) (((com.sun.management.OperatingSystemMXBean) java.lang.management.ManagementFactory
            .getOperatingSystemMXBean()).getFreePhysicalMemorySize() / 1024.0 / 1024.0 / 1024.0);

    // 32k symbols splitting for regular command.
    // But for slicing it's all up to the python script.
    //final int commandLenLimit = isSlicing ? Integer.MAX_VALUE : 1 << 15;
    final int commandLenLimit = 1 << 15;

    if (isSlicing) {
      // schedule to always try to delete the temp dir when FragPipe finishes execution
      final String tempDirName = "split_peptide_index_tempdir";
      Path toDelete = wd.resolve(tempDirName).toAbsolutePath().normalize();
      try {
        if (Files.exists(toDelete)) {
          FileUtils.deleteDirectory(toDelete.toFile());
        }
      } catch (IOException e) {
        log.warn("Could not delete leftover temporary directory: {}", toDelete.toString());
      }
      toDelete.toFile().deleteOnExit();
    }

    int fileIndex = 0;
    StringBuilder sb = new StringBuilder();

    final String ext = fp.getOutputFileExt();
    Map<InputLcmsFile, Path> mapLcmsToPepxml = outputs(lcmsFiles, ext, wd);
    Map<InputLcmsFile, Path> mapLcmsToTsv = outputs(lcmsFiles, "tsv", wd);

    final List<String> javaCmd = Arrays.asList("java", "-jar", "-Dfile.encoding=UTF-8", "-Xmx" + ramGb + "G");
    final List<String> slicingCmd = isSlicing ?
        Arrays.asList(
            PythonInfo.get().getCommand(),
            DbSlice.get().getScriptDbslicingPath().toAbsolutePath().normalize().toString(),
            Integer.toString(numSlices),
            OsUtils.isWindows() ?
                "\"" + String.join(" ", javaCmd) + "\"" :
                String.join(" ", javaCmd)
        )
        : null;
    while (fileIndex < lcmsFiles.size()) {
      ArrayList<String> cmd = new ArrayList<>();
      if (isSlicing) {
        cmd.addAll(slicingCmd);
      } else {
        cmd.addAll(javaCmd);
      }
      cmd.add(binFragger.useBin());
      cmd.add(savedParamsPath.toString());

      // check if the command length is ok so far
      sb.append(String.join(" ", cmd));
      if (sb.length() > commandLenLimit) {
        JOptionPane.showMessageDialog(comp,
            "MSFragger command line length too large even for a single file.",
            "Error", JOptionPane.ERROR_MESSAGE);
        return false;
      }

      List<InputLcmsFile> addedLcmsFiles = new ArrayList<>();
      while (fileIndex < lcmsFiles.size()) {
        InputLcmsFile f = lcmsFiles.get(fileIndex);
        // if adding this file to the command line will make the command length
        // longer than the allowed maximum, stop adding files
        if (sb.length() + f.path.toString().length() + 1 > commandLenLimit) {
          break;
        }
        sb.append(f.path.toString()).append(" ");
        cmd.add(f.path.toString());
        addedLcmsFiles.add(f);
        fileIndex++;
      }

      ProcessBuilder pb = new ProcessBuilder(cmd);
      PythonInfo.modifyEnvironmentVariablesForPythonSubprocesses(pb);
      pb.directory(wd.toFile());
      pb.environment().put("PYTHONIOENCODING", "utf-8");
      pbis.add(PbiBuilder.from(pb));
      sb.setLength(0);

      // move the pepxml files if the output directory is not the same as where
      // the lcms files were
      for (InputLcmsFile f : addedLcmsFiles) {
        Path pepxmlWhereItShouldBe = mapLcmsToPepxml.get(f);
        if (pepxmlWhereItShouldBe == null)
          throw new IllegalStateException("LCMS file mapped to no pepxml file");
        String pepxmlFn = pepxmlWhereItShouldBe.getFileName().toString();
        Path pepxmlAsCreatedByFragger = f.path.getParent().resolve(pepxmlFn);
        if (!pepxmlAsCreatedByFragger.equals(pepxmlWhereItShouldBe)) {
          List<ProcessBuilder> pbsMove = ToolingUtils
              .pbsMoveFiles(jarFragpipe, pepxmlWhereItShouldBe.getParent(),
                  Collections.singletonList(pepxmlAsCreatedByFragger));
          pbis.addAll(PbiBuilder.from(pbsMove));
        }
        Path tsvWhereItShouldBe = mapLcmsToTsv.get(f);
        String tsvFn = tsvWhereItShouldBe.getFileName().toString();
        Path tsvAsCreatedByFragger = f.path.getParent().resolve(tsvFn);
        if (!tsvAsCreatedByFragger.equals(tsvWhereItShouldBe) && params.getShiftedIons()) {
          List<ProcessBuilder> pbsMove = ToolingUtils
              .pbsMoveFiles(jarFragpipe, tsvWhereItShouldBe.getParent(), true,
                  Collections.singletonList(tsvAsCreatedByFragger));
          pbis.addAll(PbiBuilder.from(pbsMove));
        }
      }
    }

    isConfigured = true;
    return true;
  }

  @Override
  public int getPriority() {
    return 50;
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
      desc = new ArrayList<>(Arrays.asList("mzML", "mzXML", "mgf"));
      exts = new ArrayList<>(Arrays.asList(".mgf", ".mzml", ".mzxml"));
      if (searchPaths != null && !searchPaths.isEmpty()) {
        if (searchExtLibsThermo(searchPaths) != null) {
          desc.add("Thermo RAW");
          exts.add(".raw");
        }
        if (searchExtLibsBruker(searchPaths) != null) {
          desc.add("Buker PASEF .d");
          exts.add(".d");
        }
        for (int i = 0; i < exts.size(); i++) {
          exts.set(i, exts.get(i).toLowerCase());
        }
      }
      return this;
    }
  }
}
