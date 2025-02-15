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

import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.LoggerContext;
import ch.qos.logback.classic.PatternLayout;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.AppenderBase;
import java.awt.Color;
import javax.swing.JTextPane;
import javax.swing.SwingUtilities;
import javax.swing.text.AbstractDocument;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.Element;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @see ch.qos.logback.core.AppenderBase
 */
public class LogbackJTextPaneAppender extends AppenderBase<ILoggingEvent> {
  private static final Logger log = LoggerFactory.getLogger(LogbackJTextPaneAppender.class);

  private PatternLayout patternLayout;
  private JTextPane textPane;

  private static SimpleAttributeSet ERROR_ATT, WARN_ATT, INFO_ATT, DEBUG_ATT, TRACE_ATT, OTHER_ATT;

  static {
    ERROR_ATT = new SimpleAttributeSet();
    ERROR_ATT.addAttribute(StyleConstants.CharacterConstants.Bold, Boolean.TRUE);
    ERROR_ATT.addAttribute(StyleConstants.CharacterConstants.Italic, Boolean.FALSE);
    ERROR_ATT.addAttribute(StyleConstants.CharacterConstants.Foreground, new Color(153, 0, 0));

    WARN_ATT = new SimpleAttributeSet();
    WARN_ATT.addAttribute(StyleConstants.CharacterConstants.Bold, Boolean.FALSE);
    WARN_ATT.addAttribute(StyleConstants.CharacterConstants.Italic, Boolean.FALSE);
    WARN_ATT.addAttribute(StyleConstants.CharacterConstants.Foreground, new Color(153, 76, 0));

    INFO_ATT = new SimpleAttributeSet();
    INFO_ATT.addAttribute(StyleConstants.CharacterConstants.Bold, Boolean.FALSE);
    INFO_ATT.addAttribute(StyleConstants.CharacterConstants.Italic, Boolean.FALSE);
    INFO_ATT.addAttribute(StyleConstants.CharacterConstants.Foreground, new Color(0, 0, 153));

    DEBUG_ATT = new SimpleAttributeSet();
    DEBUG_ATT.addAttribute(StyleConstants.CharacterConstants.Bold, Boolean.FALSE);
    DEBUG_ATT.addAttribute(StyleConstants.CharacterConstants.Italic, Boolean.TRUE);
    DEBUG_ATT.addAttribute(StyleConstants.CharacterConstants.Foreground, new Color(64, 64, 64));

    TRACE_ATT = new SimpleAttributeSet();
    TRACE_ATT.addAttribute(StyleConstants.CharacterConstants.Bold, Boolean.FALSE);
    TRACE_ATT.addAttribute(StyleConstants.CharacterConstants.Italic, Boolean.TRUE);
    TRACE_ATT.addAttribute(StyleConstants.CharacterConstants.Foreground, new Color(153, 0, 76));

    OTHER_ATT = new SimpleAttributeSet();
    OTHER_ATT.addAttribute(StyleConstants.CharacterConstants.Bold, Boolean.FALSE);
    OTHER_ATT.addAttribute(StyleConstants.CharacterConstants.Italic, Boolean.TRUE);
    OTHER_ATT.addAttribute(StyleConstants.CharacterConstants.Foreground, new Color(0, 0, 0));
  }

  public void setTextPane(JTextPane textPane) {
    this.textPane = textPane;
  }

  public void setPatternLayout(PatternLayout patternLayout) {
    this.patternLayout = patternLayout;
  }

  @Override
  public void start() {
    if (patternLayout == null)
      patternLayout = new PatternLayout();

    LoggerContext lc = (LoggerContext) LoggerFactory.getILoggerFactory();
    setContext(lc);
    //patternLayout.setContext(getContext());
    patternLayout.setContext(lc);
    //patternLayout.setPattern("%d{HH:mm:ss.SSS} [%thread] %-5level %logger{36} - %msg%n");
    patternLayout.setPattern("%d{HH:mm:ss} %-5level: %msg%n");
    patternLayout.start();

    lc.getLogger("ROOT").addAppender(this);

    super.start();
  }

  @Override
  protected void append(ILoggingEvent event) {

    String formattedMsg = patternLayout.doLayout(event);

    SwingUtilities.invokeLater(() -> {
      try {
        int limit = 1000;
        int lineNum = 200;
        if (textPane.getDocument().getDefaultRootElement().getElementCount() > limit) {
          int end = getLineEndOffset(textPane, lineNum);
          replaceRange(textPane, null, 0, end);
        }

        if (event.getLevel() == Level.ERROR)
          textPane.getDocument().insertString(textPane.getDocument().getLength(), formattedMsg, ERROR_ATT);
        else if (event.getLevel() == Level.WARN)
          textPane.getDocument().insertString(textPane.getDocument().getLength(), formattedMsg, WARN_ATT);
        else if (event.getLevel() == Level.INFO)
          textPane.getDocument().insertString(textPane.getDocument().getLength(), formattedMsg, INFO_ATT);
        else if (event.getLevel() == Level.DEBUG)
          textPane.getDocument().insertString(textPane.getDocument().getLength(), formattedMsg, DEBUG_ATT);
        else if (event.getLevel() == Level.TRACE)
          textPane.getDocument().insertString(textPane.getDocument().getLength(), formattedMsg, TRACE_ATT);
        else
          textPane.getDocument().insertString(textPane.getDocument().getLength(), formattedMsg, OTHER_ATT);

      } catch (BadLocationException e) {
      }

      textPane.setCaretPosition(textPane.getDocument().getLength());
    });
  }

  /**
   * Num lines in text pane.
   */
  private int getLineCount(JTextPane textPane) {
    return textPane.getDocument().getDefaultRootElement().getElementCount();
  }

  /**
   * @param textPane de onde quero o offset
   * @param line the line &gt;= 0
   * @return the offset &gt;= 0
   * @throws BadLocationException Thrown if the line is
   * less than zero or greater or equal to the number of
   * lines contained in the document (as reported by
   * getLineCount)
   */
  private int getLineEndOffset(JTextPane textPane, int line) throws BadLocationException {
    int lineCount = getLineCount(textPane);
    if (line < 0) {
      throw new BadLocationException("Negative line", -1);
    } else if (line >= lineCount) {
      throw new BadLocationException("No such line", textPane.getDocument().getLength()+1);
    } else {
      Element map = textPane.getDocument().getDefaultRootElement();
      Element lineElem = map.getElement(line);
      int endOffset = lineElem.getEndOffset();
      // hide the implicit break at the end of the document
      return ((line == lineCount - 1) ? (endOffset - 1) : endOffset);
    }
  }

  /**
   * Replaces text from the indicated start to end position with the
   * new text specified.  Does nothing if the model is null.  Simply
   * does a delete if the new string is null or empty.<br>
   *
   * @param textPane de onde quero substituir o texto
   * @param str the text to use as the replacement
   * @param start the start position &gt;= 0
   * @param end the end position &gt;= start
   * @exception IllegalArgumentException if part of the range is an invalid position in the model
   */
  private void replaceRange(JTextPane textPane, String str, int start, int end) throws IllegalArgumentException {
    if (end < start) {
      throw new IllegalArgumentException("end before start");
    }
    Document doc = textPane.getDocument();
    if (doc != null) {
      try {
        if (doc instanceof AbstractDocument) {
          ((AbstractDocument)doc).replace(start, end - start, str, null);
        }
        else {
          doc.remove(start, end - start);
          doc.insertString(start, str, null);
        }
      } catch (BadLocationException e) {
        throw new IllegalArgumentException(e.getMessage());
      }
    }
  }
}

