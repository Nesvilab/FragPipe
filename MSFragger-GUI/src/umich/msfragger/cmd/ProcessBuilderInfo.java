package umich.msfragger.cmd;

import java.nio.file.Paths;
import umich.msfragger.util.StringUtils;

public class ProcessBuilderInfo {
  public final ProcessBuilder pb;
  public final String name;
  public final String fnStdOut;
  public final String fnStdErr;

  public ProcessBuilderInfo(ProcessBuilder pb, String name, String fnStdOut,
      String fnStdErr) {
    this.pb = pb;
    this.name = name;
    final boolean redirectErrorStream =
        !StringUtils.isNullOrWhitespace(fnStdOut) && Paths.get(fnStdOut)
            .equals(Paths.get(fnStdErr));
    this.fnStdOut = fnStdOut;
    this.fnStdErr = redirectErrorStream ? "" : fnStdErr;
    pb.redirectErrorStream(redirectErrorStream);
  }
}
