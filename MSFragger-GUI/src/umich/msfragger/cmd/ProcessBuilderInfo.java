package umich.msfragger.cmd;

public class ProcessBuilderInfo {
  public final ProcessBuilder pb;
  public final String name;
  public final String fnStdOut;
  public final String fnStdErr;

  public ProcessBuilderInfo(ProcessBuilder pb, String name, String fnStdOut,
      String fnStdErr) {
    this.pb = pb;
    this.name = name;
    this.fnStdOut = fnStdOut;
    this.fnStdErr = fnStdErr;
  }
}
