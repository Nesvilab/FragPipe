package umich.msfragger.params.fragger;

import java.util.Comparator;
import java.util.regex.Pattern;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import umich.msfragger.util.VersionComparator;

public class MsfraggerVersionComparator implements Comparator<String> {
  private static final Logger log = LoggerFactory.getLogger(MsfraggerVersionComparator.class);
  static Pattern regexOldScheme1 = Pattern.compile("(MSFragger-([\\d]{4,}[^\\s]*))", Pattern.CASE_INSENSITIVE);
  static Pattern regexOldScheme2 = Pattern.compile("([\\d]{4,}[^\\s]*)", Pattern.CASE_INSENSITIVE);
  static Pattern regexNewScheme1 = Pattern.compile("(MSFragger-(\\d+\\.\\d+[^\\s]*))", Pattern.CASE_INSENSITIVE);
  static Pattern regexNewScheme2 = Pattern.compile("(\\d+\\.\\d+[^\\s]*)", Pattern.CASE_INSENSITIVE);
  static VersionComparator cmp = new VersionComparator();

  public static boolean isOldScheme(String ver) {
    return regexOldScheme2.matcher(ver).find();
  }

  public static boolean isNewScheme(String ver) {
    return regexNewScheme2.matcher(ver).find();
  }

  @Override
  public int compare(String v1, String v2) {
    boolean isV1old = isOldScheme(v1);
    boolean isV1new = isNewScheme(v1);
    if (isV1new && isV1old) {
      log.error("v1 was determined to be both old and new scheme, something is wrong");
    }
    boolean isV2old = isOldScheme(v2);
    boolean isV2new = isNewScheme(v2);
    if (isV2new && isV2old) {
      log.error("v2 was determined to be both old and new scheme, something is wrong");
    }
    if (isV1old && isV2new) {
      return -1;
    } else if (isV1new && isV2old) {
      return +1;
    }
    return cmp.compare(v1, v2);
  }
}
