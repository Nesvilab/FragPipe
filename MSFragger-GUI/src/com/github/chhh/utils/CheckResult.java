package com.github.chhh.utils;

public class CheckResult {
  public final boolean isSuccess;
  public final String category;
  public final String message;

  public CheckResult(boolean isSuccess, String category, String message) {
    this.isSuccess = isSuccess;
    this.category = category;
    this.message = message;
  }

  public CheckResult(boolean isSuccess, String message) {
    this(isSuccess, null, message);
  }

  public static CheckResult newFail(String category, String message) {
    return new CheckResult(false, category, message);
  }

  public static CheckResult newSuccess(String category, String message) {
    return new CheckResult(true, category, message);
  }

  public final boolean ok() {
    return isSuccess;
  }
}
