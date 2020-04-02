package com.github.chhh.utils;

public class CheckResult {
  public final boolean isSuccess;
  public final String category;
  public final String message;
  public static final String INIT_MSG = "init default";

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

  public static CheckResult newFail(String category) {
    return new CheckResult(false, category, INIT_MSG);
  }

  public static CheckResult newSuccess(String category, String message) {
    return new CheckResult(true, category, message);
  }

  public final boolean ok() {
    return isSuccess;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }

    CheckResult that = (CheckResult) o;

    if (isSuccess != that.isSuccess) {
      return false;
    }
    if (category != null ? !category.equals(that.category) : that.category != null) {
      return false;
    }
    return message != null ? message.equals(that.message) : that.message == null;
  }

  @Override
  public int hashCode() {
    int result = (isSuccess ? 1 : 0);
    result = 31 * result + (category != null ? category.hashCode() : 0);
    result = 31 * result + (message != null ? message.hashCode() : 0);
    return result;
  }
}
