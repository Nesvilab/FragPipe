package umich.msfragger.cmd;

public class ProcessBuilderInfo {
  public final ProcessBuilder pb;
  public final String name;

  public ProcessBuilderInfo(ProcessBuilder pb, String name) {
    this.pb = pb;
    this.name = name;
  }
}
