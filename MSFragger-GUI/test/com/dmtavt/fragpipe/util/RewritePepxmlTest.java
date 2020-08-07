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
    String fn = "interact-20171007_LUMOS_f01-copy.pep.xml";
    Path path = dir.resolve(fn);

    // <msms_run_summary base_name="C:\Users\chhh\Documents\fragpipe-tests\test-60-tmti-closed-search/01CPTAC_CCRCC_W_JHU_20171007_LUMOS_f01" raw_data_type="mzML" raw_data="mzML">
    // <msms_run_summary base_name="C:\Users\chhh\Documents\fragpipe-tests-aaaaaaaaaaaaaaaa\test-60-tmti-closed-search/01CPTAC_CCRCC_W_JHU_20171007_LUMOS_f02" raw_data_type="mzML" raw_data="mzML">

    //RewritePepxml.rewriteRawPath(path, (String[])null);
    RewritePepxml.rewriteRawPath(path, false, new String[]{
        "c:\\ms-data\\01CPTAC_CCRCC_W_JHU_20171007_LUMOS_f01.mzXML",
        "d:/ms-data/01CPTAC_CCRCC_W_JHU_20171007_LUMOS_f02.mzML"});
  }

}