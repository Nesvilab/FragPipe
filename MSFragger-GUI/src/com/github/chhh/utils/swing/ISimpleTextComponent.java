package com.github.chhh.utils.swing;

import javax.swing.JLabel;
import javax.swing.text.JTextComponent;

public interface ISimpleTextComponent {
  String getText();
  void setText(String text);

  public static ISimpleTextComponent from(final JLabel comp) {
    return new ISimpleTextComponent() {
      @Override
      public String getText() {
        return comp.getText();
      }

      @Override
      public void setText(String text) {
        comp.setText(text);
      }
    };
  }

  public static ISimpleTextComponent from(final JTextComponent comp) {
    return new ISimpleTextComponent() {
      @Override
      public String getText() {
        return comp.getText();
      }

      @Override
      public void setText(String text) {
        comp.setText(text);
      }
    };
  }
}
