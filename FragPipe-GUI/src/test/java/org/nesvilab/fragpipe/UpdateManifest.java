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

import org.nesvilab.utils.FileListing;
import org.nesvilab.utils.PathUtils;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.regex.Pattern;
import org.jooq.lambda.Seq;
import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class UpdateManifest {
  private static final Logger log = LoggerFactory.getLogger(UpdateManifest.class);

  @Test
  public void createManifest() {
    Path dir = Paths.get(".").toAbsolutePath().normalize();
    Path tools = PathUtils.existing(dir.resolve("../tools").toString());
    Assert.assertNotNull(tools);
    Pattern re = Pattern.compile("^.{3,}(\\d+(?:\\.\\d)+).*");
    FileListing listing = new FileListing(tools, re);
    listing.setIncludeDirectories(false);
    listing.setRecursive(true);
    List<Path> files = listing.findFiles();
    log.info("Found files:\n\t{}", Seq.seq(files).toString("\n\t"));
  }

  
}
