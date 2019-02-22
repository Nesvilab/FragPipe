package umich.msfragger.util;

import static org.junit.Assert.*;

import org.hamcrest.BaseMatcher;
import org.hamcrest.Description;
import org.hamcrest.Matcher;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class VersionComparatorTest {
  VersionComparator vc;

  @Before
  public void setup() {
    vc = new VersionComparator();
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
