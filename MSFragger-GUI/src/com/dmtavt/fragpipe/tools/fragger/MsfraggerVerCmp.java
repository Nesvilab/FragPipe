package com.dmtavt.fragpipe.tools.fragger;

import java.util.Comparator;
import java.util.regex.Pattern;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.github.chhh.utils.VersionComparator;

public class MsfraggerVerCmp implements Comparator<String> {
  private static final Logger log = LoggerFactory.getLogger(MsfraggerVerCmp.class);
  public static Pattern regexOldScheme1 = Pattern.compile("(MSFragger-([\\d]{4,}[a-z0-9.-]*))", Pattern.CASE_INSENSITIVE);
  public static Pattern regexOldScheme2 = Pattern.compile("([\\d]{4,}[a-z0-9.-]*)");
  public static Pattern regexNewScheme1 = Pattern.compile("(MSFragger-(\\d+\\.\\d+[a-z0-9.-]*))", Pattern.CASE_INSENSITIVE);
  public static Pattern regexNewScheme2 = Pattern.compile("(\\d+\\.\\d+[a-z0-9.-]*)");
  static final VersionComparator VER_CMP = new VersionComparator();
  private static final MsfraggerVerCmp INSTANCE = new MsfraggerVerCmp();

  public static boolean isOldScheme(String ver) {
    return regexOldScheme2.matcher(ver).find();
  }

  public static boolean isNewScheme(String ver) {
    return regexNewScheme2.matcher(ver).find();
  }

  public static MsfraggerVerCmp get() {
    return INSTANCE;
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
    return VER_CMP.compare(v1, v2);
  }
}
