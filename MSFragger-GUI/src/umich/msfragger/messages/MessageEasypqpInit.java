package umich.msfragger.messages;

public class MessageEasypqpInit {
  public final boolean isPythonOk;
  public final boolean isEasypqpPackageInstalled;
  public final String message;

  public MessageEasypqpInit(boolean isPythonOk, boolean isEasypqpPackageInstalled,
      String message) {
    this.isPythonOk = isPythonOk;
    this.isEasypqpPackageInstalled = isEasypqpPackageInstalled;
    this.message = message;
  }
}
