package com.github.chhh.utils.swing;

import com.github.chhh.utils.StringUtils;
import java.awt.Color;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.concurrent.atomic.AtomicBoolean;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * From: http://stackoverflow.com/questions/10506789/how-to-display-faint-gray-ghost-text-in-a-jtextfield
 */
public class GhostText implements FocusListener, DocumentListener, PropertyChangeListener {
  private static final Logger log = LoggerFactory.getLogger(GhostText.class);

  private final JTextField textfield;
  public static final Color LIGHT_GREY = Color.LIGHT_GRAY;
  private Color ghostTextColor = LIGHT_GREY;
  private Color normalTextColor;
  private final String ghostText;
  private final Object lock = new Object();
  private AtomicBoolean isInsideCall = new AtomicBoolean(false);


  public GhostText(final JTextField textfield, String ghostText) {
    this.textfield = textfield;
    this.ghostText = ghostText;
    this.normalTextColor = textfield.getForeground();
    updateColor();
    textfield.addFocusListener(this);
    registerListeners();
    if (!this.textfield.hasFocus()) {
      focusLost(null);
    }
  }

  public static void register(JTextField textfield, String ghostText) {
    SwingUtilities.invokeLater(() -> new GhostText(textfield, ghostText));
  }

  public static void register(UiText uiText) {
    SwingUtilities.invokeLater(() -> new GhostText(uiText, uiText.getGhostText()));
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
    if (isEmpty()) {
      unregisterListeners();
      updateText(ghostText, ghostTextColor);
      registerListeners();
    }
  }

  @Override
  public void propertyChange(PropertyChangeEvent evt) {
    synchronized (lock) {
      if (!isInsideCall.get()) {
        updateColor();
      }
    }
  }

  @Override
  public void changedUpdate(DocumentEvent e) {
    updateColor();
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
    String t = textfield.getText();
    return StringUtils.isBlank(t) || t.equals(ghostText);
  }

  private boolean isShowingGhostText() {
    return ghostText != null && ghostText.equalsIgnoreCase(textfield.getText());
  }

  private void updateText(String text, Color color) {
    synchronized (lock) {
      isInsideCall.set(true);
      textfield.setForeground(color);
      textfield.setText(text);
      isInsideCall.set(false);
    }
  }

  private void updateColor() {
    synchronized (lock) {
      isInsideCall.set(true);
      Color cCur = textfield.getForeground();
      Color cNew = isShowingGhostText() ? ghostTextColor : normalTextColor;
      if (!cNew.equals(cCur)) {
        textfield.setForeground(cNew);
      }
      isInsideCall.set(false);
    }
  }
}

