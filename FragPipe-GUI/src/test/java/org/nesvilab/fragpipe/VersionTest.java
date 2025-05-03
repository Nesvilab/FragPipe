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

package org.nesvilab.fragpipe;

import org.nesvilab.utils.PathUtils;
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
    Pattern re = Pattern.compile("^\\s*version\\s*=\\s*'([0-9.]+(-build.+)?)'");
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
