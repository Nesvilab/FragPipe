package umich.msfragger.messages;

import java.util.List;
import umich.msfragger.gui.RunnableDescription;

public class MessageStartProcesses {
  public final List<RunnableDescription> runDescs;

  public MessageStartProcesses(List<RunnableDescription> pbi) {
    this.runDescs = pbi;
  }
}
