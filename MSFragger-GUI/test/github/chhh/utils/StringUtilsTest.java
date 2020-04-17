package github.chhh.utils;

import com.github.chhh.utils.StringUtils;
import java.util.List;
import org.junit.Assert;
import org.junit.Test;

public class StringUtilsTest {

  @Test
  public void testSimple() {
    String cmd = "command param1 param2";
    List<String> parts = StringUtils.splitCommandLine(cmd);
    Assert.assertEquals(3, parts.size());
  }

  @Test
  public void testFlagsAndParams() {
    String cmd = "command --flag-1 --flag-2 --param-3 param-3-value";
    List<String> parts = StringUtils.splitCommandLine(cmd);
    Assert.assertEquals(5, parts.size());
  }

  @Test
  public void testPathWithDoubleQuotes() {
    String cmd = "command --flag-1 --flag-2 --param-3 param-3-value -i \"c:/some/path/to/file\"";
    List<String> parts = StringUtils.splitCommandLine(cmd);
    Assert.assertEquals(7, parts.size());
  }

  @Test
  public void testDoubleQuotesAndParamAfter() {
    String cmd = "command --flag-1 --flag-2 --param-3 param-3-value -i \"c:/some/path/to/file\" --help";
    List<String> parts = StringUtils.splitCommandLine(cmd);
    Assert.assertEquals(8, parts.size());
  }

  @Test
  public void testSingleQuotesAndParamAfter() {
    String cmd = "command --flag-1 --flag-2 --param-3 param-3-value -i 'c:/some/path/to/file' --help";
    List<String> parts = StringUtils.splitCommandLine(cmd);
    Assert.assertEquals(8, parts.size());
  }

  @Test
  public void testDoubleQuotePathWithSpaces() {
    String cmd = "command --flag-1 --flag-2 --param-3 param-3-value -i \"c:/some/path with spaces/to/file\" --help";
    List<String> parts = StringUtils.splitCommandLine(cmd);
    Assert.assertEquals(8, parts.size());
  }

  @Test
  public void testSingleQuotePathWithSpaces() {
    String cmd = "command --flag-1 --flag-2 --param-3 param-3-value -i 'c:/some/path with spaces/to/file' --help";
    List<String> parts = StringUtils.splitCommandLine(cmd);
    Assert.assertEquals(8, parts.size());
  }
}
