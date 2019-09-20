package com.github.chhh.utils.swing;

import javax.swing.JTextField;
import javax.swing.text.Document;

public class UiText extends JTextField implements StringRepresentable, GhostedTextComponent {
  private String ghostText = null;

  public UiText() {
    super();
  }

  public UiText(String text) {
    super(text);
  }

  public UiText(String text, String ghostText) {
    super(text);
    this.ghostText = ghostText;
  }

  public UiText(int columns) {
    super(columns);
  }

  public UiText(String text, int columns) {
    super(text, columns);
  }

  public UiText(Document doc, String text, int columns) {
    super(doc, text, columns);
  }

  @Override
  public String asString() {
    return getText().trim();
  }

  @Override
  public void fromString(String s) {
    setText(s);
  }

  @Override
  public void setGhostText(String text) {
    this.ghostText = text;
  }

  @Override
  public String getGhostText() {
    return this.ghostText;
  }

  @Override
  public String getNonGhostText() {
    final String t = getText();
    if (ghostText == null) {
      return t;
    }
    return ghostText.equals(t) ? "" : t;
  }
}
