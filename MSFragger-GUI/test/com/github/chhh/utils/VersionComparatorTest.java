package com.github.chhh.utils;

import com.github.chhh.utils.VersionComparator;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

public class VersionComparatorTest {
  VersionComparator vc;

  @Before
  public void setup() {
    vc = new VersionComparator();
  }

  @Test @Ignore
  public void versionIsGreaterRc() {
    int compare = vc.compare("13.0-RC11", "13.0-RC2");
    Assert.assertTrue(compare > 0);
  }


  @Test
  public void versionIsGreater() {
    int compare = vc.compare("9.0", "9.0-RC1");
    Assert.assertTrue(compare > 0);

    compare = vc.compare("9.0", "9.0-b01");
    Assert.assertTrue(compare > 0);

    compare = vc.compare("9.0.0", "9.0.RC1");
    Assert.assertTrue(compare > 0);
  }

  @Test
  public void versionIsSmaller() {
    int compare = vc.compare("9.0", "9.0.RC1");
    Assert.assertTrue(compare < 0);

    compare = vc.compare("9.0", "9.0.b01");
    Assert.assertTrue(compare < 0);
  }

  @Test
  public void versionsEqual() {
    int compare = vc.compare("9.0", "9.0.0");
    Assert.assertTrue(compare == 0);
  }
}
