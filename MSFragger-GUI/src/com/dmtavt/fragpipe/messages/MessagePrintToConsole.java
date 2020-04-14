package com.dmtavt.fragpipe.messages;

public class MessagePrintToConsole {

  public final String text;
  public final boolean addNewline;

  public MessagePrintToConsole(String text) {
    this(text, true);
  }

  public MessagePrintToConsole(String text, boolean addNewline) {
    this.text = text;
    this.addNewline = addNewline;
  }


}
