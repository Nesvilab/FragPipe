package com.dmtavt.fragpipe.tools.msfragger;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.api.Bus;
import com.dmtavt.fragpipe.exceptions.UnexpectedException;
import com.dmtavt.fragpipe.exceptions.ValidationException;
import com.dmtavt.fragpipe.messages.MessageMsfraggerUpdateAvailable;
import com.dmtavt.fragpipe.messages.NoteConfigMsfragger;
import com.github.chhh.utils.StringUtils;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.commons.lang3.JavaVersion;
import org.apache.commons.lang3.SystemUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.dmtavt.fragpipe.api.VersionFetcher;
import com.dmtavt.fragpipe.params.fragger.MsfraggerVersionFetcherGithub;
import com.dmtavt.fragpipe.params.fragger.MsfraggerVersionFetcherLocal;
import com.dmtavt.fragpipe.params.fragger.MsfraggerVersionFetcherServer;

public class Msfragger {

  private static final Logger log = LoggerFactory.getLogger(Msfragger.class);

  public static Version version(Path jar) throws ValidationException {
    // only validate Fragger version if the current Java version is 1.8 or higher
    Version test;
    if (!SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_8)) {
      // we can't test fragger binary verison when java version is less than 1.8
      throw new ValidationException("MSFragger requires Java 8+, can't check version without it.");
    }
    // get the vesrion reported by the current executable
    test = testJar(jar.toString());
    if (!test.isVersionParsed) {
      throw new ValidationException("Could not get version info with given jar: " + jar.toString());
    }
    return test;
  }


  public static void checkUpdates(NoteConfigMsfragger m) {
    final MsfraggerVerCmp vc = new MsfraggerVerCmp();
    final String verLocal = m.version;
    Thread t = new Thread(() -> {

      MsfraggerVersionFetcherServer vfServer = new MsfraggerVersionFetcherServer();
      MsfraggerVersionFetcherGithub vfGithub = new MsfraggerVersionFetcherGithub();
//      MsfraggerVersionFetcherServer vfServer = null;
//      MsfraggerVersionFetcherGithub vfGithub = null;
      MsfraggerVersionFetcherLocal vfLocal = new MsfraggerVersionFetcherLocal();
      List<VersionFetcher> verFetchers = Arrays.asList(vfServer, vfGithub, vfLocal);
      for (final VersionFetcher vf : verFetchers) {
        if (vf == null) {
          continue;
        }
        try {
          final String verUpdated = vf.fetchVersion();
          if (StringUtils.isNullOrWhitespace(verUpdated)) {
            continue;
          }
          // we got a non-empty version from some version fetcher
          if (vc.compare(verLocal, verUpdated) >= 0) {
            continue; // our version is same or newer
          }
          // local version is older than the fetched version
          String url = vf.getDownloadUrl();
          final String manualDownloadUrl = !StringUtils.isNullOrWhitespace(url) ? url : vfLocal.getDownloadUrl();
          Bus.post(new MessageMsfraggerUpdateAvailable(verUpdated, manualDownloadUrl));
          break;
        } catch (Exception ex) {
          // no biggie
          log.warn("Something happened while checking for MSFragger updates", ex);
        }
      }
    });
    t.start();
  }

  public static Version testJar(String jarPath) {
    String verStr = null;
    boolean isVersionParsed = false;
    try {
      ProcessBuilder pb = new ProcessBuilder(Fragpipe.getBinJava(), "-jar", jarPath);
      List<Pattern> regexs = Arrays.asList(MsfraggerVerCmp.regexOldScheme1,
          MsfraggerVerCmp.regexNewScheme1);
      pb.redirectErrorStream(true);
      Process pr = pb.start();
      try (BufferedReader in = new BufferedReader(new InputStreamReader(pr.getInputStream()))) {
        String line;
        while ((line = in.readLine()) != null) {
          for (Pattern re : regexs) {
            Matcher m = re.matcher(line);
            if (m.find()) {
              isVersionParsed = true;
              verStr = m.group(2);
            }
          }
        }
        pr.waitFor();
      }
    } catch (IOException | InterruptedException e) {
      throw new IllegalStateException("Error while creating a java process for MSFragger test.");
    }
    return new Version(isVersionParsed, verStr);
  }

  public static void validateJar(String path) throws ValidationException, UnexpectedException {
    try {
      Path p;
      try {
        p = Paths.get(path);
      } catch (InvalidPathException e) {
        throw new ValidationException("Path is not well formed", e);
      }
      if (!Files.exists(p)) {
        throw new ValidationException("Path not exists");
      }
      if (!path.toLowerCase().endsWith(".jar")) {
        throw new ValidationException("Path not a jar file");
      }
      if (!validateMsfraggerJarContents(p)) {
        throw new ValidationException("Not an MSFragger jar file");
      }
    } catch (Exception e) {
      if (e instanceof ValidationException) {
        throw e;
      }
      throw new UnexpectedException("Invalid MSFragger jar path", e);
    }
  }

  private static boolean validateMsfraggerJarContents(Path p) {
    final boolean[] found = {false};
    try (FileSystem fs = FileSystems.newFileSystem(p, ClassLoader.getSystemClassLoader())) {
      for (Path root : fs.getRootDirectories()) {
        Files.walkFileTree(root, new SimpleFileVisitor<Path>() {
          final Pattern regex = Pattern.compile("msfragger.*\\.jar", Pattern.CASE_INSENSITIVE);

          @Override
          public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) {
            String fileName = file.getFileName().toString();
            if ("MSFragger.class".equalsIgnoreCase(fileName)) {
              found[0] = true;
              return FileVisitResult.TERMINATE;
            } else if (regex.matcher(fileName).find()) {
              found[0] = true;
              return FileVisitResult.TERMINATE;
            }
            return FileVisitResult.CONTINUE;
          }
        });
      }
    } catch (IOException ex) {
      log.warn("Error while checking MSFragger jar contents", ex);
    }

    return found[0];
  }

  public static class Version {

    final public boolean isVersionParsed;
    final public String version;

    public Version(boolean isVersionParsed, String version) {
      this.isVersionParsed = isVersionParsed;
      this.version = version;
    }
  }
}
