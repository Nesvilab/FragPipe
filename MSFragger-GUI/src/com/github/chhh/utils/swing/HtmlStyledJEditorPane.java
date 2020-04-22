package com.github.chhh.utils.swing;

import com.github.chhh.utils.SwingUtils;
import javax.swing.JEditorPane;
import org.jsoup.Jsoup;

public class HtmlStyledJEditorPane extends JEditorPane {

  public HtmlStyledJEditorPane() {
    super();
    init();
  }

  private void init() {
    setContentType("text/html");
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
