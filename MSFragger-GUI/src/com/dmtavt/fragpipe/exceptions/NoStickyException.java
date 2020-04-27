package com.dmtavt.fragpipe.exceptions;

public class NoStickyException extends Throwable {

  /**
   * Constructs a new throwable with the specified detail message.  The cause is not initialized, and
   * may subsequently be initialized by a call to {@link #initCause}.
   *
   * <p>The {@link #fillInStackTrace()} method is called to initialize
   * the stack trace data in the newly created throwable.
   *
   * @param message the detail message. The detail message is saved for later retrieval by the {@link
   *                #getMessage()} method.
   */
  public NoStickyException(String message) {
    super(message);
  }
}
