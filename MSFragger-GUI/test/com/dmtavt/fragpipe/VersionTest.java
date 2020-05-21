package com.dmtavt.fragpipe;

import com.dmtavt.fragpipe.params.ThisAppProps;
import com.github.chhh.utils.IOUtils;
import com.github.chhh.utils.PathUtils;
import com.github.chhh.utils.StringUtils;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import org.junit.Assert;
import org.junit.Assume;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class VersionTest {
  private static final Logger log = LoggerFactory.getLogger(VersionTest.class);

  @Test
  public void checkVersionIsSameInGradleBuildAndBundle() throws IOException {

    Assume.assumeFalse(Version.isDevBuild());

    Path path = Paths.get(".").toAbsolutePath().normalize().toAbsolutePath();
    log.info("Cur path: {}", path);
    Path dir = PathUtils.existing(path.toString());
    Assert.assertNotNull("Could not determine current directory for testing", dir);
    Path build = PathUtils.existing(dir.resolve("build.gradle").toString());
    Assert.assertNotNull("Could not find [build.gradle]", build);
    List<String> lines = Files.readAllLines(build);
    Pattern re = Pattern.compile("^\\s*version\\s*=\\s*(.+)");
    List<String> l = lines.stream().map(re::matcher).filter(Matcher::find)
        .map(m -> m.group(1)).collect(Collectors.toList());
    Assert.assertNotEquals("No version found in build.gradle", 0, l.size());
    Assert.assertFalse("More than 1 version found in build.gradle", l.size() > 1);
    final String verGradle = l.get(0).trim().replaceAll("['\"]", "");
    final String verBundle = Version.version();
    log.info("Version in build.gradle: {}. Version in bundle: {}, Equal: {}", verGradle, verBundle, verBundle.equals(verGradle));
    Assert.assertEquals("Version mismatch between build.gradle and Bundle.properties", verGradle, verBundle);
  }
}
