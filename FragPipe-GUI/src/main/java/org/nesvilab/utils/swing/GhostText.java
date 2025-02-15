/*
 * This file is part of FragPipe.
 *
 * FragPipe is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * FragPipe is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with FragPipe. If not, see <https://www.gnu.org/licenses/>.
 */

package org.nesvilab.utils.swing;

import org.nesvilab.utils.StringUtils;
import java.awt.Color;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Arrays;
import java.util.List;
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
  private List<String> propsToListen = Arrays.asList("foreground", "text");


  public GhostText(final JTextField textfield, String ghostText) {
    this.textfield = textfield;
    this.ghostText = ghostText;
    this.normalTextColor = textfield.getForeground();
    updateColor();
    for (FocusListener e : textfield.getFocusListeners())
      if (e instanceof GhostText)
        return;
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
    synchronized (lock) {
      textfield.getDocument().addDocumentListener(this);
      for (String prop : propsToListen) {
        textfield.addPropertyChangeListener(prop, this);
      }
    }
  }

  private void unregisterListeners() {
    synchronized (lock) {
      textfield.getDocument().removeDocumentListener(this);
      for (String prop : propsToListen) {
        textfield.removePropertyChangeListener(prop, this);
      }
    }
  }

  @Override
  public void focusGained(FocusEvent e) {
    synchronized (lock) {
      if (isEmpty()) {
        unregisterListeners();
        updateText("", normalTextColor);
        registerListeners();
      }
    }
  }

  @Override
  public void focusLost(FocusEvent e) {
    synchronized (lock) {
      if (isEmpty()) {
        unregisterListeners();
        updateText(ghostText, ghostTextColor);
        registerListeners();
      }
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
    synchronized (lock) {
      String t = textfield.getText();
      return StringUtils.isBlank(t) || t.equals(ghostText);
    }
  }

  private boolean isShowingGhostText() {
    synchronized (lock) {
      return ghostText != null && ghostText.equalsIgnoreCase(textfield.getText());
    }
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

