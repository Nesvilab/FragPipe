package com.dmtavt.fragpipe;

import com.dmtavt.fragpipe.params.ThisAppProps;
import org.jsoup.internal.StringUtil;
import org.junit.Assert;
import org.junit.Test;

public class BundleTest {

  @Test
  public void TestAnnouncementIsEmptyForRelease() {
    if (!Version.isDevBuild()) {
      String announce = ThisAppProps.getLocalProperties().getProperty(Version.PROP_ANNOUNCE);
      Assert.assertTrue(
          String.format("Bundle property [%s] must be empty for release versions, but had value [%s]", Version.PROP_ANNOUNCE, announce),
          StringUtil.isBlank(announce));
    }
  }
}
