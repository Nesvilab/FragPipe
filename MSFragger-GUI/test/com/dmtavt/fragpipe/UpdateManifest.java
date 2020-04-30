package com.dmtavt.fragpipe;

import com.github.chhh.utils.FileListing;
import com.github.chhh.utils.PathUtils;
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
    Path tools = PathUtils.existing(dir.resolve("tools").toString());
    Assert.assertNotNull(tools);
    Pattern re = Pattern.compile("^.{3,}(\\d+(?:\\.\\d)+).*");
    FileListing listing = new FileListing(tools, re);
    listing.setIncludeDirectories(false);
    listing.setRecursive(true);
    List<Path> files = listing.findFiles();
    log.info("Found files:\n\t{}", Seq.seq(files).toString("\n\t"));
  }

  
}
