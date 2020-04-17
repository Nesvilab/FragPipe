package com.dmtavt.fragpipe.process;

public class ProcessDescription {
  public final String name;
  public final String workDir;
  public final String command;

  public ProcessDescription(String name, String workDir, String command) {
    this.name = name;
    this.workDir = workDir;
    this.command = command;
  }

  public static class Builder {

    private String name;
    private String workDir;
    private String command;

    public Builder setName(String name) {
      this.name = name;
      return this;
    }

    public Builder setWorkDir(String workDir) {
      this.workDir = workDir;
      return this;
    }

    public Builder setCommand(String command) {
      this.command = command;
      return this;
    }

    public ProcessDescription create() {
      return new ProcessDescription(name, workDir, command);
    }
  }
}
