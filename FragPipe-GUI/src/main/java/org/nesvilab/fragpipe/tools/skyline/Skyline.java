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

package org.nesvilab.fragpipe.tools.skyline;

import static org.nesvilab.fragpipe.tabs.TabMsfragger.ENZYMES;
import static org.nesvilab.fragpipe.tabs.TabWorkflow.workflowExt;
import static org.nesvilab.utils.OsUtils.isWindows;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.jooq.lambda.Seq;
import org.nesvilab.fragpipe.FragpipeLocations;
import org.nesvilab.fragpipe.api.PropsFile;
import org.nesvilab.fragpipe.cmd.PbiBuilder;
import org.nesvilab.fragpipe.cmd.ProcessBuilderInfo;
import org.nesvilab.fragpipe.exceptions.UnexpectedException;
import org.nesvilab.fragpipe.exceptions.ValidationException;
import org.nesvilab.fragpipe.process.ProcessResult;
import org.nesvilab.fragpipe.tools.fragger.MsfraggerEnzyme;
import org.nesvilab.utils.PathUtils;
import org.nesvilab.utils.ProcessUtils;
import org.nesvilab.utils.StringUtils;

public class Skyline {

  private static final String DOWNLOAD_URL = "https://skyline.ms/project/home/software/Skyline/begin.view";
  private static final Pattern versionWithSpacesPattern = Pattern.compile(" ([\\d.]+) ?");
  private static final Pattern versionNoSpacesPattern = Pattern.compile("([\\d.]+)");

  public static DefaultArtifactVersion skylineVersionT = new DefaultArtifactVersion("23.1.1.0");
  private static DefaultArtifactVersion skylineVersion = null;
  private static String skylineRunnerPath = null;
  private static DefaultArtifactVersion skylineDailyVersion = null;
  private static String skylineDailyRunnerPath = null;

  // convert our names to Skyline names
  // names from Skyline cmd documentation (version 24.0-daily, 7/19/2024)
  private final static Map<String, String> enzymesMap = new HashMap<>();
  static {
    enzymesMap.put("trypsin", "Trypsin");
    enzymesMap.put("stricttrypsin", "Trypsin/P");
    enzymesMap.put("argc", "ArgC");
    enzymesMap.put("aspn", "AspN");
    enzymesMap.put("chymotrypsin", "Chymotrypsin");
    enzymesMap.put("cnbr", "CNBr");
    enzymesMap.put("elastase", "Elastase");
    enzymesMap.put("formicacid", "Formic Acid");
    enzymesMap.put("gluc", "GluC");
    enzymesMap.put("gluc_bicarb", "GluC bicarb");
    enzymesMap.put("lysc", "LysC");
    enzymesMap.put("lysc-p", "LysC/P");
    enzymesMap.put("lysn", "LysN");
    enzymesMap.put("lysn_promisc", "LysN promisc");
    enzymesMap.put("trypsin/cnbr", "Trypsin-CNBr");
    enzymesMap.put("trypsin_gluc", "Trypsin-GluC");
    enzymesMap.put("trypsin_k", "LysC");
    enzymesMap.put("trypsin_r", "ArgC");
  }

  public static void main(String[] args) {
    try {
      runSkyline(args[0], Paths.get(args[1]), args[2], Integer.parseInt(args[3]), Boolean.parseBoolean(args[4]), Integer.parseInt(args[5]), Integer.parseInt(args[6]));
    } catch (Exception e) {
      e.printStackTrace();
      System.exit(1);
    }
  }

  private static void runSkyline(String skylinePath, Path wd, String skylineVersion, int modsMode, boolean useSsl, int precursorTolerance, int fragmentTolerance) throws Exception {
    if (skylinePath == null || skylinePath.isEmpty()) {
      throw new RuntimeException("Cannot find the Skyline executable file.");
    } else {
      Path workflowPath = wd.resolve("fragpipe" + workflowExt);
      PropsFile pf = new PropsFile(workflowPath, "for Skyline");
      pf.load();

      Set<String> lcmsFiles = new TreeSet<>();
      Set<String> ddaAndDIAfiles = new TreeSet<>();

      String dataType = "DDA";
      String line;
      BufferedReader reader = Files.newBufferedReader(wd.resolve("fragpipe-files.fp-manifest"));
      while ((line = reader.readLine()) != null) {
        line = line.trim();
        if (line.isEmpty()) {
          continue;
        }
        String[] parts = line.split("\t");
        if (parts[3].contains("DIA")) {
          dataType = "DIA";
        }
      }
      reader.close();
      boolean useSpeclib = dataType.contentEquals("DIA") && !useSsl;

      reader = Files.newBufferedReader(wd.resolve("fragpipe-files.fp-manifest"));
      while ((line = reader.readLine()) != null) {
        line = line.trim();
        if (line.isEmpty()) {
          continue;
        }
        String[] parts = line.split("\t");
        if (dataType.contentEquals("DIA")) {
          if (parts[3].contains("DIA")) {
            lcmsFiles.add(parts[0]);
          }
          ddaAndDIAfiles.add(parts[0]);
        } else {
          lcmsFiles.add(parts[0]);
        }
      }
      reader.close();

      List<Path> speclibFiles;
      try (Stream<Path> paths = Files.walk(wd.resolve("dia-quant-output"))) {
        speclibFiles = paths.filter(p -> p.getFileName().toString().endsWith(".skyline.speclib")).collect(Collectors.toList());
      } catch (IOException ex) {
        // dia-quant-output directory does not always exist (e.g., for DDA data). Avoid crashing if it doesn't.
        speclibFiles = Collections.emptyList();
      }

      try (Stream<Path> paths = Files.walk(wd.resolve("dia-quant-output"))) {
        speclibFiles = paths.filter(p -> p.getFileName().toString().endsWith(".speclib")).collect(Collectors.toList());
      } catch (IOException ex) {
        // dia-quant-output directory does not always exist (e.g., for DDA data). Avoid crashing if it doesn't.
        speclibFiles = Collections.emptyList();
      }

      Set<Path> psmTsvFiles = Files.walk(wd).filter(p -> p.getFileName().toString().startsWith("psm.tsv") && p.getFileName().toString().endsWith("psm.tsv")).collect(Collectors.toCollection(TreeSet::new));

      if (useSpeclib && speclibFiles.isEmpty()) {
        System.out.println("No DIA-NN .speclib files found in " + wd.resolve("dia-quant-output") + " but Skyline was set to use the spectral library as input and DIA-NN was enabled. Did the DIA-NN run fail? No Skyline document will be generated.");
        System.exit(1);
      }

      Path skylineFilesDir = wd.resolve("skyline_files");
      Files.createDirectories(skylineFilesDir);

      DefaultArtifactVersion v = new DefaultArtifactVersion(skylineVersion);
      boolean matchUnimod = v.compareTo(new DefaultArtifactVersion("23.1.1.418")) >= 0;

      Path peptideListPath = skylineFilesDir.resolve("peptide_list.txt").toAbsolutePath();
      WritePeptideList pepWriter = new WritePeptideList();
      Map<Float, Set<String>> addedMods = pepWriter.writePeptideList(psmTsvFiles, peptideListPath);

      WriteSSL sslWriter = null;
      Path sslPath = skylineFilesDir.resolve("psm.ssl").toAbsolutePath();
      if (!useSpeclib) {
        boolean isPercolator = Boolean.parseBoolean(pf.getProperty("percolator.run-percolator"));
        sslWriter = new WriteSSL();
        if (dataType.contentEquals("DIA")) {
          // psm.tsv refers to DDA files, need to provide as well
          sslWriter.writeSSL(psmTsvFiles, sslPath, isPercolator, ddaAndDIAfiles, !useSsl);
        } else {
          sslWriter.writeSSL(psmTsvFiles, sslPath, isPercolator, lcmsFiles, !useSsl);
        }
      }

      Path skylineTemplateXmlPath = wd.resolve("skyline_template.xml");
      WriteSkylineTemplate writeSkylineTemplate = new WriteSkylineTemplate(skylineTemplateXmlPath, pf, modsMode, matchUnimod, !useSpeclib, addedMods, sslWriter != null && sslWriter.hasCv);

      Path pp = wd.resolve("filelist_skyline.txt");

      BufferedWriter writer = Files.newBufferedWriter(pp);

      if (writeSkylineTemplate.needTemplate) {
        writer.write("--overwrite ");
        writer.write("--in=" + skylineTemplateXmlPath.toAbsolutePath() + " ");
        writer.write("--out=" + skylineFilesDir.resolve("fragpipe.sky").toAbsolutePath() + "\n");
        writer.write("--in=" + skylineFilesDir.resolve("fragpipe.sky").toAbsolutePath() + " ");
        writer.write("--save" + " ");
      } else {
        writer.write("--overwrite ");
        writer.write("--new=" + skylineFilesDir.resolve("fragpipe.sky").toAbsolutePath() + " ");
      }

      if (!writeSkylineTemplate.unimodMods.isEmpty()) {
        for (UnimodData unimodData : writeSkylineTemplate.unimodMods) {
          writer.write("--pep-add-mod=\"" + unimodData.name + "\" ");
        }
      }

      writer.write("--full-scan-acquisition-method=" + dataType + " ");

      // always import pep list (it replaces fasta import, not search result import)
      writer.write("--import-pep-list=" + peptideListPath.toAbsolutePath() + " ");
      if (!useSpeclib) {
        writer.write("--import-search-file=" + sslPath.toAbsolutePath() + " ");
      } else {
        // DIA-NN
        writer.write("--import-search-cutoff-score=0.01 ");
        writer.write("--import-search-file=" + speclibFiles.get(0).toAbsolutePath() + " ");
      }
      writer.write("--import-search-include-ambiguous ");

      if (v.compareTo(new DefaultArtifactVersion("23.1.1.335")) >= 0) {
        writer.write("--pep-max-missed-cleavages=" + pf.getProperty("msfragger.allowed_missed_cleavage_1") + " ");
        writer.write("--pep-min-length=" + pf.getProperty("msfragger.digest_min_length") + " ");
        writer.write("--pep-max-length=" + pf.getProperty("msfragger.digest_max_length") + " ");
        writer.write("--pep-exclude-nterminal-aas=0 ");
        int maxVarmods = Integer.parseInt(pf.getProperty("msfragger.max_variable_mods_per_peptide"));
        if (maxVarmods != 3){
          writer.write(String.format("--pep-max-variable-mods=%s ", maxVarmods));
        }
        writer.write(getEnzyme(pf));
      }
      writer.write("--tran-precursor-ion-charges=\"2,3,4,5,6\" ");
      writer.write("--tran-product-ion-charges=\"1,2\" ");
      writer.write("--tran-product-ion-types=\"y,b,p\" ");
      writer.write("--tran-product-start-ion=\"ion 3\" ");
      writer.write("--tran-product-end-ion=\"last ion\" ");
      writer.write("--tran-product-clear-special-ions ");
      writer.write("--library-match-tolerance=" + fragmentTolerance + "ppm ");
      writer.write("--library-product-ions=12 ");
      writer.write("--library-min-product-ions=1 ");
      writer.write("--library-pick-product-ions=filter ");
      writer.write("--full-scan-precursor-analyzer=centroided ");
      writer.write("--full-scan-precursor-isotopes=Count ");
      writer.write("--full-scan-precursor-threshold=3 ");
      writer.write("--full-scan-product-analyzer=centroided ");
      writer.write("--full-scan-precursor-res=" + precursorTolerance + " ");
      writer.write("--full-scan-product-res=" + fragmentTolerance + " ");
      writer.write("--full-scan-rt-filter=ms2_ids ");
      writer.write("--full-scan-rt-filter-tolerance=2 ");
      writer.write("--instrument-min-mz=50 ");
      writer.write("--instrument-max-mz=2000 ");
      writer.write("--full-scan-precursor-isotopes=Count ");

      if (dataType.contentEquals("DIA")) {
        writer.write("--full-scan-isolation-scheme=\"Results only\" ");
      }

      writer.write("--import-search-exclude-library-sources ");
      for (String s : lcmsFiles) {
        writer.write("--import-file=" + s + " ");
      }

      writer.close();

      List<String> cmd = new ArrayList<>();
      cmd.add(skylinePath);
      cmd.add("--timestamp");
      cmd.add("--dir=" + wd.toAbsolutePath());
      cmd.add("--batch-commands=" + pp.toAbsolutePath());

      Process skylineProcess = null;
      try {
        System.out.println("Running command: " + String.join(" ", cmd));

        System.out.println("Content of " + pp.toAbsolutePath() + ":");
        System.out.println(new String(Files.readAllBytes(pp)));

        ProcessBuilder pb = new ProcessBuilder(cmd);
        ProcessBuilderInfo pbi = new PbiBuilder().setPb(pb).setName(pb.toString()).setFnStdOut(null).setFnStdErr(null).setParallelGroup(null).create();
        ProcessResult pr = new ProcessResult(pbi);
        skylineProcess = pr.start();

        redirectOutputToConsole(skylineProcess.getInputStream());

        int exitValue = skylineProcess.waitFor();
        if (exitValue != 0) {
          String errStr = pr.appendErr(pr.pollStdErr());
          System.err.println("Process " + pb + " returned non zero value. Message:\n" + (errStr == null ? "" : errStr.trim()));
          System.exit(exitValue);
        } else {
          Files.deleteIfExists(wd.resolve("mod.skyd"));
          System.out.println("DONE! The Skyline files locate in " + skylineFilesDir.toAbsolutePath());
        }
      } catch (Exception ex) {
        System.err.println(ExceptionUtils.getStackTrace(ex));
        if (skylineProcess != null && skylineProcess.isAlive()) {
          skylineProcess.destroyForcibly();
        }
        System.exit(1);
      } finally {
        if (skylineProcess != null && skylineProcess.isAlive()) {
          skylineProcess.destroyForcibly(); // todo: If clicking "stop", the Skyline process won't be killed.
        }
      }
    }
  }

  private static void redirectOutputToConsole(InputStream inputStream) {
    new Thread(() -> {
      try (BufferedReader reader = new BufferedReader(new InputStreamReader(inputStream))) {
        String line;
        while ((line = reader.readLine()) != null) {
          System.out.println(line);
        }
      } catch (IOException e) {
        System.err.println(ExceptionUtils.getStackTrace(e));
      }
    }).start();
  }

  public static String getSkylineRunnerPath() {
    if (skylineRunnerPath != null) {
      return skylineRunnerPath;
    }
    try {
      sub("SkylineRunner.exe");
    } catch (Exception e) {
      e.printStackTrace();
    }
    return skylineRunnerPath;
  }

  public static String getSkylineDailyRunnerPath() {
    if (skylineDailyRunnerPath != null) {
      return skylineDailyRunnerPath;
    }
    try {
      sub("SkylineDailyRunner.exe");
    } catch (Exception e) {
      e.printStackTrace();
    }
    return skylineDailyRunnerPath;
  }

  public static DefaultArtifactVersion getSkylineVersion() {
    if (skylineVersion != null) {
      return skylineVersion;
    }
    try {
      sub("SkylineRunner.exe");
    } catch (Exception e) {
      e.printStackTrace();
    }
    return skylineVersion;
  }

  public static DefaultArtifactVersion getSkylineDailyVersion() {
    if (skylineDailyVersion != null) {
      return skylineDailyVersion;
    }
    try {
      sub("SkylineDailyRunner.exe");
    } catch (Exception e) {
      e.printStackTrace();
    }
    return skylineDailyVersion;
  }

  /**
   * Generate an enzyme parameter for Skyline. If either MSFragger enzyme is an allowed Skyline enzyme,
   * use it. If both are allowed, default to the first one (since Skyline only supports 1 on command line).
   *
   * Note: Matches enzymes using the search cuts/nocuts/sense rather than name to ensure match to the actual
   * search settings.
   */
  private static String getEnzyme(PropsFile pf) {
    String cuts1 = pf.getProperty("msfragger.search_enzyme_cut_1");
    String cuts2 = pf.getProperty("msfragger.search_enzyme_cut_2");
    String nocuts1 = pf.getProperty("msfragger.search_enzyme_nocut_1");
    String nocuts2 = pf.getProperty("msfragger.search_enzyme_nocut_2");
    String sense1 = pf.getProperty("msfragger.search_enzyme_sense_1");
    String sense2 = pf.getProperty("msfragger.search_enzyme_sense_2");

    List<MsfraggerEnzyme> enzymes = ENZYMES.stream()
            .map(e -> new MsfraggerEnzyme(e.name, StringUtils.sortedChars(e.cut),
                    StringUtils.sortedChars(e.nocuts), e.sense))
            .collect(Collectors.toList());
    List<MsfraggerEnzyme> matching1 = enzymes.stream()
            .filter(e -> e.cut.equals(cuts1) && e.nocuts.equals(nocuts1) && e.sense.equals(sense1))
            .collect(Collectors.toList());
    List<MsfraggerEnzyme> matching2 = enzymes.stream()
            .filter(e -> e.cut.equals(cuts2) && e.nocuts.equals(nocuts2) && e.sense.equals(sense2))
            .collect(Collectors.toList());

    // return the first enzyme with a matching name, since Skyline only supports a single enzyme via cmd
    for (MsfraggerEnzyme enzyme : matching1) {
      if (enzymesMap.containsKey(enzyme.name)) {
        return String.format("--pep-digest-enzyme=%s ", enzymesMap.get(enzyme.name));
      }
    }
    for (MsfraggerEnzyme enzyme : matching2) {
      if (enzymesMap.containsKey(enzyme.name)) {
        return String.format("--pep-digest-enzyme=%s ", enzymesMap.get(enzyme.name));
      }
    }
    // no known mapping, do not specify parameter to avoid crashing Skyline
    System.out.println("Warning: no enzyme from MSFragger search matched a Skyline-supported enzyme. Skyline will default to trypsin.");
    return "";
  }

  private static void sub(String s) throws Exception {
    List<Path> classpathJars = FragpipeLocations.checkToolsMissing(Seq.of(s));
    if (classpathJars != null && !classpathJars.isEmpty()) {
      Version version = validate(classpathJars.get(0).toAbsolutePath().normalize().toString());
      if (version != null) {
        if (s.contentEquals("SkylineRunner.exe")) {
          skylineVersion = new DefaultArtifactVersion(version.strippedVersion);
          skylineRunnerPath = classpathJars.get(0).toAbsolutePath().normalize().toString();
        } else if (s.contentEquals("SkylineDailyRunner.exe")) {
          skylineDailyVersion = new DefaultArtifactVersion(version.strippedVersion);
          skylineDailyRunnerPath = classpathJars.get(0).toAbsolutePath().normalize().toString();
        } else {
          throw new RuntimeException("Unknown Skyline runner: " + s);
        }
      }
    }
  }

  public static String sub2(String s) throws Exception {
    List<Path> classpathJars = FragpipeLocations.checkToolsMissing(Seq.of(s));
    if (classpathJars != null && !classpathJars.isEmpty()) {
      Version version = validate(classpathJars.get(0).toAbsolutePath().normalize().toString());
      if (version != null) {
        return version.strippedVersion;
      }
    }
    return null;
  }

  public static Version validate(String path) throws ValidationException, UnexpectedException {
    if (!isWindows()) {
      return null;
    }

    String binPath = PathUtils.testBinaryPath(path);
    if (binPath == null) {
      throw new ValidationException("Does not appear to be an executable file: \"" + path + "\"");
    }

    StringBuilder sbVer = new StringBuilder();

    ProcessBuilder pb = new ProcessBuilder(binPath, "--version");
    pb.redirectErrorStream(true);
    ProcessUtils.consumeLines(pb, line -> {
      sbVer.append(line);
      return sbVer.length() == 0;
    });

    if (sbVer.length() == 0 || sbVer.toString().startsWith("Error")) {
      return null;
    }

    return new Version(sbVer.toString(), false, DOWNLOAD_URL);
  }

  public static class Version {

    public final String versionStr;
    public final String strippedVersion;
    public final boolean isNewVersionFound;
    public final String downloadUrl;

    public Version(String versionStr, boolean isNewVersionFound, String downloadUrl) {
      this.versionStr = versionStr;
      this.isNewVersionFound = isNewVersionFound;
      this.downloadUrl = downloadUrl;
      this.strippedVersion = getStrippedVersion(versionStr);
    }

    private String getStrippedVersion(String version) {
      Matcher match;
      if (version.contains(" ")) {
        match = versionWithSpacesPattern.matcher(version);
      } else {
        match = versionNoSpacesPattern.matcher(version);
      }
      if (match.find()) {
        return match.group(1);
      } else {
        return version;
      }
    }
  }
}
