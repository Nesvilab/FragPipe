package umich.msfragger.messages;

import java.awt.Color;

public class MessageAppendToConsole {
  public final String text;
  public final Color color;

  public MessageAppendToConsole(String text, Color color) {
    this.text = text;
    this.color = color;
  }
}
