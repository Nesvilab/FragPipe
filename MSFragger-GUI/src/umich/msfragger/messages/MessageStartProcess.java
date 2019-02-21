package umich.msfragger.messages;

import umich.msfragger.cmd.ProcessBuilderInfo;

public class MessageStartProcess {
  public final ProcessBuilderInfo pbi;

  public MessageStartProcess(ProcessBuilderInfo pbi) {
    this.pbi = pbi;
  }
}
