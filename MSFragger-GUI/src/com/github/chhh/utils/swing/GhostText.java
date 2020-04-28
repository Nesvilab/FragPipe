package com.github.chhh.utils.swing;

import com.github.chhh.utils.StringUtils;
import java.awt.Color;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Objects;
import javax.swing.JTextField;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

/**
 * From: http://stackoverflow.com/questions/10506789/how-to-display-faint-gray-ghost-text-in-a-jtextfield
 */
public class GhostText implements FocusListener, DocumentListener, PropertyChangeListener {

  private final JTextField textfield;
  private Color ghostTextColor;
  private Color normalTextColor;
  private final String ghostText;
  private final Object lock = new Object();

  public static final Color LIGHT_GREY = Color.LIGHT_GRAY;

  public GhostText(final JTextField textfield, String ghostText, Color ghostTextColor) {
    this.textfield = textfield;
    this.ghostText = ghostText;
    this.ghostTextColor = ghostTextColor;
    this.normalTextColor = textfield.getForeground();
    textfield.addFocusListener(this);
    registerListeners();
    updateColorToNormal();
    if (!this.textfield.hasFocus()) {
      focusLost(null);
    }
  }

//  public GhostText(final UiText textfield, Color ghostTextColor) {
//    this.textfield = textfield;
//    this.ghostText = textfield.getGhostText();
//    this.ghostTextColor = ghostTextColor;
//    this.normalTextColor = textfield.getForeground();
//    textfield.addFocusListener(this);
//    registerListeners();
//    updateColor();
//    if (!this.textfield.hasFocus()) {
//      focusLost(null);
//    }
//  }

  public static void register(JTextField textfield, String ghostText, Color ghostTextColor) {
    new GhostText(textfield, ghostText, ghostTextColor);
  }

  public static void register(UiText uiText) {
    register(uiText, Color.LIGHT_GRAY);
  }

  public static void register(UiText uiText, Color ghostTextColor) {
    new GhostText(uiText, uiText.getGhostText(), ghostTextColor);
  }

  public void delete() {
    unregisterListeners();
    textfield.removeFocusListener(this);
  }

  private void registerListeners() {
    textfield.getDocument().addDocumentListener(this);
    textfield.addPropertyChangeListener("foreground", this);
  }

  private void unregisterListeners() {
    textfield.getDocument().removeDocumentListener(this);
    textfield.removePropertyChangeListener("foreground", this);
  }

  @Override
  public void focusGained(FocusEvent e) {
    if (isEmpty()) {
      unregisterListeners();
      updateText("", normalTextColor);
      registerListeners();
    }

  }

  @Override
  public void focusLost(FocusEvent e) {
    if (isEmpty() || isShowingGhostText()) {
      unregisterListeners();
      updateText(ghostText, ghostTextColor);
      registerListeners();
    }
  }

  @Override
  public void propertyChange(PropertyChangeEvent evt) {
    //!"foreground".equalsIgnoreCase(evt.getPropertyName())
//    if (!Objects.equals(evt.getNewValue(), evt.getOldValue())) {
//      updateColorToNormal();
//    }
  }

  @Override
  public void changedUpdate(DocumentEvent e) {
    updateColorToNormal();
  }

  @Override
  public void insertUpdate(DocumentEvent e) {
    changedUpdate(e);
  }

  @Override
  public void removeUpdate(DocumentEvent e) {
    changedUpdate(e);
  }

  private boolean isEmpty() {
    return StringUtils.isBlank(textfield.getText()) || (ghostText!=null && ghostText.equals(textfield.getText()));
  }

  private boolean isShowingGhostText() {
    return ghostText != null && ghostText.equalsIgnoreCase(textfield.getText());
  }

  private void updateText(String text, Color color) {
    if (text != null && !text.equals(textfield.getText())) {
      textfield.setText(text);
    }
    if (color != null && !color.equals(textfield.getForeground())) {
      textfield.setForeground(color);
    }
  }

  private void updateColorToNormal() {
    synchronized (lock) {
      if (!normalTextColor.equals(textfield.getForeground())) {
        textfield.setForeground(normalTextColor);
      }
    }
  }
}

