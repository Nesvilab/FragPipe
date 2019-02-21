package umich.msfragger.messages;

public class MessageExternalProcessOutput {
  public final boolean isError;
  public final String output;

  public MessageExternalProcessOutput(boolean isError, String output) {
    this.isError = isError;
    this.output = output;
  }
}
