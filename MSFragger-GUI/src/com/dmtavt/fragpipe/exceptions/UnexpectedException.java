package com.dmtavt.fragpipe.exceptions;

public class UnexpectedException extends Exception {

  public UnexpectedException() {
  }

  public UnexpectedException(String message) {
    super(message);
  }

  public UnexpectedException(String message, Throwable cause) {
    super(message, cause);
  }

  public UnexpectedException(Throwable cause) {
    super(cause);
  }

  public UnexpectedException(String message, Throwable cause, boolean enableSuppression,
      boolean writableStackTrace) {
    super(message, cause, enableSuppression, writableStackTrace);
  }
}
