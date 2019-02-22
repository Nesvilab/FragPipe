package com.github.chhh.utils.swing;

import java.text.Format;
import javax.swing.JFormattedTextField;

public class UiFormattedText extends JFormattedTextField implements StringRepresentable {

  public UiFormattedText() {
  }

  public UiFormattedText(Format format) {
    super(format);
  }

  public UiFormattedText(AbstractFormatter formatter) {
    super(formatter);
  }

  public UiFormattedText(AbstractFormatterFactory factory) {
    super(factory);
  }

  public UiFormattedText(AbstractFormatterFactory factory, Object currentValue) {
    super(factory, currentValue);
  }

  @Override
  public String asString() {
    return getText().trim();
  }

  @Override
  public void fromString(String s) {
    setText(s);
  }
}
