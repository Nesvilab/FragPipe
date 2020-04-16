package com.dmtavt.fragpipe.exceptions;

public class NoSuchElementInModelException extends RuntimeException {
  public final String uiElemName;
  public final String missingElement;
  public final Class<?> clazz;

  public NoSuchElementInModelException(String uiElemName, String missingElement,
      Class<?> clazz) {
    this.uiElemName = uiElemName;
    this.missingElement = missingElement;
    this.clazz = clazz;
  }
}
