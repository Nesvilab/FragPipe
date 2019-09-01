package umich.msfragger.params.fragger;

import java.util.regex.Pattern;

public class MsfraggerVersionComparer {

  public static int compare(String v1, String v2) {
    Pattern regexOldScheme = Pattern.compile("(MSFragger-([\\d]{4,}[^\\s]*))", Pattern.CASE_INSENSITIVE);
    Pattern regexNewScheme = Pattern.compile("(MSFragger-(\\d+\\.\\d+[^\\s]*))", Pattern.CASE_INSENSITIVE);

    // TODO: implement the new comparison
  }
}
