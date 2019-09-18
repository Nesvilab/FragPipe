package umich.msfragger.cmd;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public class PbiBuilder {

  private ProcessBuilder pb;
  private String name;
  private String fnStdOut;
  private String fnStdErr;
  private String parallelGroup;

  public PbiBuilder setPb(ProcessBuilder pb) {
    this.pb = pb;
    return this;
  }

  public PbiBuilder setName(String name) {
    this.name = name;
    return this;
  }

  public PbiBuilder setFnStdOut(String fnStdOut) {
    this.fnStdOut = fnStdOut;
    return this;
  }

  public PbiBuilder setFnStdErr(String fnStdErr) {
    this.fnStdErr = fnStdErr;
    return this;
  }

  public PbiBuilder setParallelGroup(String parallelGroup) {
    this.parallelGroup = parallelGroup;
    return this;
  }

  public ProcessBuilderInfo create() {
    return new ProcessBuilderInfo(pb, name, fnStdOut, fnStdErr, parallelGroup);
  }

  public static List<ProcessBuilderInfo> from (List<ProcessBuilder> pbs) {
    return pbs.stream().map(pb -> new PbiBuilder().setPb(pb).create()).collect(Collectors.toList());
  }

  public static ProcessBuilderInfo from (ProcessBuilder pb) {
    return new PbiBuilder().setPb(pb).create();
  }
}
