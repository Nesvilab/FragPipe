package umich.msfragger.messages;

import umich.msfragger.gui.ProcessResult;

public class MessageProcessStarted {
  public final ProcessResult pr;

  public MessageProcessStarted(ProcessResult pr) {
    this.pr = pr;
  }
}
