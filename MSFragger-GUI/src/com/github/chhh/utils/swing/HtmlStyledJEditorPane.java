package com.github.chhh.utils.swing;

import com.github.chhh.utils.SwingUtils;
import java.net.URISyntaxException;
import javax.swing.JEditorPane;
import javax.swing.JLabel;
import javax.swing.SwingUtilities;
import javax.swing.event.HyperlinkEvent;
import org.jsoup.Jsoup;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class HtmlStyledJEditorPane extends JEditorPane {
  private static final Logger log = LoggerFactory.getLogger(HtmlStyledJEditorPane.class);
  final boolean handleHyperlinks;
  private final Object lock = new Object();

  public HtmlStyledJEditorPane() {
    super();
    handleHyperlinks = true;
    init();
  }

  public HtmlStyledJEditorPane(boolean handleHyperlinks) {
    super();
    this.handleHyperlinks = handleHyperlinks;
    init();
  }

  public HtmlStyledJEditorPane(String text) {
    super();
    this.handleHyperlinks = true;
    init();
    setText(text);
  }

  private void init() {
    setContentType("text/html");
    setBackground(new JLabel().getBackground());

    if (handleHyperlinks) {
      addHyperlinkListener(e -> {
        if (e.getEventType().equals(HyperlinkEvent.EventType.ACTIVATED)) {
          try {
            SwingUtils.openBrowserOrThrow(e.getURL().toURI());
          } catch (URISyntaxException ex) {
            throw new IllegalStateException("Incorrect url/uri", ex);
          }

        }
      });
    }
    setText(SwingUtils.wrapInStyledHtml(""));
  }


  @Override
  public void setText(String t) {
    SwingUtilities.invokeLater(() -> {
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
    });

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
