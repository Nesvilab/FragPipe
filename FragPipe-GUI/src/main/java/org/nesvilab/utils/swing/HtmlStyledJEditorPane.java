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
import org.nesvilab.utils.SwingUtils;
import java.net.URISyntaxException;
import javax.swing.JEditorPane;
import javax.swing.JLabel;
import javax.swing.event.HyperlinkEvent;
import org.jsoup.Jsoup;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class HtmlStyledJEditorPane extends JEditorPane {
  private static final Logger log = LoggerFactory.getLogger(HtmlStyledJEditorPane.class);
  private final Object lock = new Object();

  public HtmlStyledJEditorPane() {
    this(true, "");
  }

  public HtmlStyledJEditorPane(boolean handleHyperlinks) {
    this(handleHyperlinks, null);
  }

  public HtmlStyledJEditorPane(String text) {
    this(true, text);
  }

  public HtmlStyledJEditorPane(boolean handleHyperlinks, String text) {
    super();
    init();

    if (handleHyperlinks) {
      this.addHyperlinkListener(e -> {
        if (e.getEventType().equals(HyperlinkEvent.EventType.ACTIVATED)) {
          try {
            SwingUtils.openBrowserOrThrow(e.getURL().toURI());
          } catch (URISyntaxException ex) {
            throw new IllegalStateException("Incorrect url/uri", ex);
          }
        }
      });
    }

    if (StringUtils.isNotBlank(text)) {
      this.setText(text);
    }
  }

  private void init() {
    setContentTextHtml();
    setBackground(new JLabel().getBackground());
  }

  private void setContentTextHtml() {
    if (!"text/html".equalsIgnoreCase(getContentType())) {
      setContentType("text/html");
    }
  }

  @Override
  public void setText(String t) {
  /* Do not use SwingUtilities.invokeLater here, may cause deadlock.
    #javax.swing.JEditorPane.setText calls #javax.swing.text.html.HTMLDocument.HTMLReader#adjustEndElement
    which obtains a write lock to an instance of #javax.swing.text.html.HTMLDocument
    followed by a lock on a static java.awt.Component$AWTTreeLock object

    java.awt.Container.getPreferredSize locks on a static java.awt.Component$AWTTreeLock object
    followed by a read lock to an instance of #javax.swing.text.html.HTMLDocument
   */
    if (t == null) {
      log.error("Called setText with null");
      super.setText(null);
      return;
    }
    String body = SwingUtils.tryExtractHtmlBody(t);
    if (body == null) {
      log.error("Body was computed to null");
    }

    String wrap = SwingUtils.wrapInStyledHtml(body);
    if (wrap == null) {
      log.error("Wrapped text evaluated to null");
    }

    try {
      super.setText(null);
      setContentTextHtml();
      super.setText(wrap);
    } catch (NullPointerException e) {
      log.error("NPE happened when setting wrapped text");
    } catch (RuntimeException re) {
      if (re.getMessage().contains("insert new content into body")) {
        log.error("{}\n{}", re.getMessage(), wrap);
        log.error("Trying to use the 'body': {}", body);
        super.setText(body);
      } else {
        throw re;
      }
    }
  }

  /** Text less main HTML (html, head, body) but with inner HTML tags like div or p. */
  @Override
  public String getText() {
    return super.getText();
  }

  /** Text less all HTML tags (html, head, body, div, p, etc). */
  public String getTextLessHtml() {
    return Jsoup.parse(getTextFull()).text();
  }

  /** Includes all HTML formatting stuff. */
  public String getTextFull() {
    return super.getText();
  }
}
