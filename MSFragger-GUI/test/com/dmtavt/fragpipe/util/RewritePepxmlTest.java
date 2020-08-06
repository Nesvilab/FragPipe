package com.dmtavt.fragpipe.util;

import static org.junit.Assert.*;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.junit.Ignore;
import org.junit.Test;

public class RewritePepxmlTest {

  @Test //@Ignore
  public void testPepxmlRewrite() throws IOException {
    Path dir = Paths.get("D:\\ms-data\\full-runs\\ids");
    String fn = "interact-20171007_LUMOS_f01.pep.xml";
    Path path = dir.resolve(fn);

    //RewritePepxml.rewriteRawPath(path, "D:\\fake\\path.mzXML");
    RewritePepxml.rewriteRawPath(path, null);
  }

}