package umich.msfragger.messages;

import java.util.List;
import umich.msfragger.cmd.ProcessBuilderInfo;

public class MessageStartProcess {
  public final List<ProcessBuilderInfo> pbis;

  public MessageStartProcess(List<ProcessBuilderInfo> pbi) {
    this.pbis = pbi;
  }
}
