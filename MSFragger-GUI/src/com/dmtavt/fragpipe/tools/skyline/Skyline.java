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

package com.dmtavt.fragpipe.tools.skyline;

import static com.dmtavt.fragpipe.tabs.TabWorkflow.workflowExt;

import com.dmtavt.fragpipe.FragpipeLocations;
import com.dmtavt.fragpipe.api.PropsFile;
import com.dmtavt.fragpipe.cmd.PbiBuilder;
import com.dmtavt.fragpipe.cmd.ProcessBuilderInfo;
import com.dmtavt.fragpipe.exceptions.UnexpectedException;
import com.dmtavt.fragpipe.exceptions.ValidationException;
import com.dmtavt.fragpipe.process.ProcessResult;
import com.github.chhh.utils.PathUtils;
import com.github.chhh.utils.ProcessUtils;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.jooq.lambda.Seq;

public class Skyline {

  private static final String DOWNLOAD_URL = "https://skyline.ms/project/home/software/Skyline/begin.view";
  private static final Pattern versionWithSpacesPattern = Pattern.compile(" ([\\d.]+) ?");
  private static final Pattern versionNoSpacesPattern = Pattern.compile("([\\d.]+)");
  private static final Pattern pattern = Pattern.compile("Converged to [\\d.]+ % FDR with \\d+ Ions\"?\\s+decoy=\\d+ threshold=([\\d.]+) total=\\d+");

  public static DefaultArtifactVersion skylineVersionT = new DefaultArtifactVersion("23.1.1.0");
  private static DefaultArtifactVersion skylineVersion = null;
  private static String skylineRunnerPath = null;
  private static DefaultArtifactVersion skylineDailyVersion = null;
  private static String skylineDailyRunnerPath = null;


  public static void main(String[] args) {
    try {
      runSkyline(args[0], Paths.get(args[1]), args[2], Integer.parseInt(args[3]));
    } catch (Exception e) {
      e.printStackTrace();
      System.exit(1);
    }
  }

  private static void runSkyline(String skylinePath, Path wd, String skylineVersion, int mode) throws Exception {
    if (skylinePath == null || skylinePath.isEmpty()) {
      throw new RuntimeException("Cannot find the Skyline executable file.");
    } else {
      Path workflowPath = wd.resolve("fragpipe" + workflowExt);
      PropsFile pf = new PropsFile(workflowPath, "for Skyline");
      pf.load();

      TreeSet<String> lcmsFiles = new TreeSet<>();

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
        } else {
          lcmsFiles.add(parts[0]);
        }
      }
      reader.close();

      float probThreshold = 1.1f;
      TreeSet<Path> pepxmlFiles = new TreeSet<>();
      List<Path> speclibFiles = Files.walk(wd).filter(p -> p.getFileName().toString().endsWith(".speclib")).collect(Collectors.toList());

      if (mode == 0 && speclibFiles.isEmpty()) {
        System.out.println("No speclib files found in " + wd + " but Skyline was set to use the speclib as input. Let Skyline build its own speclib.");
        mode = 1;
      }

      if (mode == 1) {
        List<Path> logFiles = Files.walk(wd).filter(p -> p.getFileName().toString().contentEquals("filter.log")).collect(Collectors.toList());

        if (logFiles.isEmpty()) {
          throw new FileNotFoundException("No filter.log files found in " + wd);
        }

        reader = Files.newBufferedReader(logFiles.get(logFiles.size() - 1));
        while ((line = reader.readLine()) != null) {
          line = line.trim();
          if (line.isEmpty()) {
            continue;
          }
          Matcher matcher = pattern.matcher(line);
          if (matcher.find()) {
            float f = Float.parseFloat(matcher.group(1));
            if (f < probThreshold) {
              probThreshold = f;
            }
          }
        }
        reader.close();

        if (probThreshold > 1) {
          throw new RuntimeException("Could not find the probability threshold in the filter.log.");
        }

        pepxmlFiles = Files.walk(wd).filter(p -> p.getFileName().toString().startsWith("interact-") && p.getFileName().toString().endsWith(".pep.xml")).collect(Collectors.toCollection(TreeSet::new));
        pepxmlFiles.addAll(Files.walk(wd).filter(p -> p.getFileName().toString().contentEquals("interact.pep.xml")).collect(Collectors.toCollection(TreeSet::new)));
      }

      Path modSkyPath = wd.resolve("mod.sky");
      new WriteSky(modSkyPath, pf.getProperty("msfragger.table.fix-mods"), pf.getProperty("msfragger.table.var-mods"), pf.getProperty("msfragger.mass_offsets"), pf.getProperty("msfragger.restrict_deltamass_to"), pf.getProperty("msfragger.remainder_fragment_masses"), pf.getProperty("msfragger.mass_offsets_detailed"));

      Path pp = wd.resolve("filelist_skyline.txt");

      BufferedWriter writer = Files.newBufferedWriter(pp);
      writer.write("--in=" + modSkyPath.toAbsolutePath() + " ");
      writer.write("--overwrite ");
      if (mode == 0) {
        writer.write("--out=fragpipe.sky ");
      } else if (mode == 1) {
        writer.write("--out=fragpipe_skylib.sky ");
      } else {
        throw new RuntimeException("Unsupported Skyline mode: " + mode);
      }
      writer.write("--import-search-add-mods ");
      writer.write("--full-scan-acquisition-method=" + dataType + " ");

      if (mode == 1) {
        writer.write("--import-search-cutoff-score=" + probThreshold + " ");
        for (Path p : pepxmlFiles) {
          writer.write("--import-search-file=" + p.toAbsolutePath() + " ");
        }
      } else {
        writer.write("--import-search-cutoff-score=0.01 ");
        writer.write("--import-search-file=" + speclibFiles.get(0).toAbsolutePath() + " ");
      }

      DefaultArtifactVersion v = new DefaultArtifactVersion(skylineVersion);
      if (v.compareTo(skylineVersionT) > 0) {
        // parameters added after released 23.1 version
        writer.write("--pep-max-missed-cleavages=" + pf.getProperty("msfragger.allowed_missed_cleavage_1") + " ");
        writer.write("--pep-min-length=" + pf.getProperty("msfragger.digest_min_length") + " ");
        writer.write("--pep-max-length=" + pf.getProperty("msfragger.digest_max_length") + " ");
        writer.write("--pep-exclude-nterminal-aas=0 ");
      }
      writer.write("--tran-precursor-ion-charges=\"2,3,4,5,6\" ");
      writer.write("--tran-product-ion-charges=\"1,2\" ");
      writer.write("--tran-product-ion-types=\"y,b,p\" ");
      writer.write("--tran-product-start-ion=\"ion 3\" ");
      writer.write("--tran-product-end-ion=\"last ion\" ");
      writer.write("--tran-product-clear-special-ions ");
      if (pf.getProperty("msfragger.fragment_mass_units").contentEquals("1")) {
        writer.write("--library-match-tolerance=" + Math.min(10, Float.parseFloat(pf.getProperty("msfragger.fragment_mass_tolerance"))) + "ppm ");
      } else {
        writer.write("--library-match-tolerance=" + pf.getProperty("msfragger.fragment_mass_tolerance") + "mz ");
      }
      writer.write("--library-product-ions=12 ");
      writer.write("--library-min-product-ions=" + pf.getProperty("msfragger.min_matched_fragments") + " ");
      writer.write("--library-pick-product-ions=filter ");
      writer.write("--full-scan-precursor-analyzer=centroided ");
      writer.write("--full-scan-precursor-isotopes=Count ");
      writer.write("--full-scan-precursor-threshold=3 ");
      writer.write("--full-scan-product-analyzer=centroided ");

      if (pf.getProperty("msfragger.precursor_true_units").contentEquals("1")) {
        writer.write("--full-scan-precursor-res=" + Math.min(10, Float.parseFloat(pf.getProperty("msfragger.precursor_true_tolerance"))) + " ");
      } else {
        writer.write("--full-scan-precursor-res=" + Math.min(10, Float.parseFloat(pf.getProperty("msfragger.precursor_true_tolerance")) * 1000) + " ");
      }

      if (pf.getProperty("msfragger.fragment_mass_units").contentEquals("1")) {
        writer.write("--full-scan-product-res=" + Math.min(10, Float.parseFloat(pf.getProperty("msfragger.fragment_mass_tolerance"))) + " ");
      } else {
        writer.write("--full-scan-product-res=" + Math.min(10, Float.parseFloat(pf.getProperty("msfragger.fragment_mass_tolerance")) * 1000) + " ");
      }

      writer.write("--full-scan-rt-filter=ms2_ids ");
      writer.write("--full-scan-rt-filter-tolerance=2 ");
      writer.write("--instrument-min-mz=50 ");
      writer.write("--instrument-max-mz=2000 ");
      writer.write("--full-scan-precursor-isotopes=Count ");

      if (dataType.contentEquals("DIA")) {
        writer.write("--full-scan-isolation-scheme=\"Results only\" ");
      }

      for (String s : lcmsFiles) {
        writer.write("--import-file=" + s + " ");
      }
      writer.write("--import-search-exclude-library-sources ");

      Files.walk(wd).filter(p -> p.getFileName().toString().contentEquals("protein.fas")).forEach(p -> {
        try {
          writer.write("--import-fasta=" + p.toAbsolutePath() + " ");
        } catch (IOException ex) {
          throw new RuntimeException(ex);
        }
      });
      writer.write("--associate-proteins-shared-peptides=DuplicatedBetweenProteins ");

      writer.close();

      List<String> cmd = new ArrayList<>();
      cmd.add(skylinePath);
      cmd.add("--timestamp");
      cmd.add("--dir=" + wd.toAbsolutePath());
      cmd.add("--batch-commands=" + pp.toAbsolutePath());

      Process skylineProcess = null;
      try {
        System.out.println("Running command: " + String.join(" ", cmd));
        ProcessBuilder pb = new ProcessBuilder(cmd);
        ProcessBuilderInfo pbi = new PbiBuilder().setPb(pb).setName(pb.toString()).setFnStdOut(null).setFnStdErr(null).setParallelGroup(null).create();
        ProcessResult pr = new ProcessResult(pbi);
        skylineProcess = pr.start();

        redirectOutputToConsole(skylineProcess.getInputStream());

        int exitValue = skylineProcess.waitFor();
        if (exitValue != 0) {
          String errStr = pr.appendErr(pr.pollStdErr());
          System.err.println("Process " + pb + " returned non zero value. Message:\n" + (errStr == null ? "" : errStr));
        } else {
          System.out.println("DONE! The Skyline files locate in " + wd.toAbsolutePath());
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

  private static void sub(String s) throws Exception {
    List<Path> classpathJars = FragpipeLocations.checkToolsMissing(Seq.of(s));
    if (classpathJars != null && !classpathJars.isEmpty()) {
      Version version = validate(classpathJars.get(0).toAbsolutePath().toString());
      if (version != null) {
        if (s.contentEquals("SkylineRunner.exe")) {
          skylineVersion = new DefaultArtifactVersion(version.strippedVersion);
          skylineRunnerPath = classpathJars.get(0).toAbsolutePath().toString();
        } else if (s.contentEquals("SkylineDailyRunner.exe")) {
          skylineDailyVersion = new DefaultArtifactVersion(version.strippedVersion);
          skylineDailyRunnerPath = classpathJars.get(0).toAbsolutePath().toString();
        } else {
          throw new RuntimeException("Unknown Skyline runner: " + s);
        }
      }
    }
  }

  public static String sub2(String s) throws Exception {
    List<Path> classpathJars = FragpipeLocations.checkToolsMissing(Seq.of(s));
    if (classpathJars != null && !classpathJars.isEmpty()) {
      Version version = validate(classpathJars.get(0).toAbsolutePath().toString());
      if (version != null) {
        return version.strippedVersion;
      }
    }
    return null;
  }

  public static Version validate(String path) throws ValidationException, UnexpectedException {
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
