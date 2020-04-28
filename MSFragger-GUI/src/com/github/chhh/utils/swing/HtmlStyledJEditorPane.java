package com.github.chhh.utils.swing;

import com.github.chhh.utils.SwingUtils;
import java.net.URISyntaxException;
import javax.swing.JEditorPane;
import javax.swing.JLabel;
import javax.swing.event.HyperlinkEvent;
import org.jsoup.Jsoup;

public class HtmlStyledJEditorPane extends JEditorPane {
  final boolean handleHyperlinks;

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
    String body = SwingUtils.tryExtractHtmlBody(t);
    super.setText(SwingUtils.wrapInStyledHtml(body));
  }

  /** Text less main HTML (html, head, body) but with inner HTML tags like div or p. */
  @Override
  public String getText() {
    return SwingUtils.tryExtractHtmlBody(super.getText());
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
