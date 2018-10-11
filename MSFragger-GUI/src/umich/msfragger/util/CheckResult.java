package umich.msfragger.util;

public class CheckResult {
  public final boolean isSuccess;
  public final String message;

  public CheckResult(boolean isSuccess, String message) {
    this.isSuccess = isSuccess;
    this.message = message;
  }
}
