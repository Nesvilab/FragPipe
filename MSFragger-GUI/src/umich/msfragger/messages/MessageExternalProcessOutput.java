package umich.msfragger.messages;

public class MessageExternalProcessOutput {
  public final boolean isError;
  public final String output;
  public final String procName;

  public MessageExternalProcessOutput(boolean isError, String output, String procName) {
    this.isError = isError;
    this.output = output;
    this.procName = procName;
  }
}
