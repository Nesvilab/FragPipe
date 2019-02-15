package com.github.chhh.utils.swing;

import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.DocumentFilter;
import javax.swing.text.PlainDocument;

/**
 * Methods return {@link javax.swing.text.Document} instances that can be used with
 * {@link javax.swing.text.JTextComponent#setDocument(Document)}. These documents filter out
 * specific characters or string being input by the user.
 *
 * @author Dmitry Avtonomov
 */
public class DocumentFilters {

  public static final Pattern RE_DOT = Pattern.compile("\\.");

  private DocumentFilters() {}

  /**
   * Filter that removes all substrings matching the provided regular expression.
   * @param filteredCharsRegex Regular expression for filtering out characters.
   */
  public static PlainDocument getFilter(final String filteredCharsRegex) {
    PlainDocument doc = new PlainDocument();
    final Pattern regex = Pattern.compile(filteredCharsRegex);
    doc.setDocumentFilter(new DocumentFilter() {
      @Override
      public void insertString(DocumentFilter.FilterBypass fb, int off, String str, AttributeSet attr)
          throws BadLocationException {
        fb.insertString(off, regex.matcher(str).replaceAll(""), attr);
      }

      @Override
      public void replace(DocumentFilter.FilterBypass fb, int off, int len, String str, AttributeSet attr)
          throws BadLocationException {
        fb.replace(off, len, regex.matcher(str).replaceAll(""), attr);
      }
    });
    return doc;
  }

  public static PlainDocument getDigitsOnlyFilter() {
    return getFilter("\\D+");
  }

  public static PlainDocument getDigitCommaDotSpaceMinusFitler() {
    return getFilter("[^0-9., -]+");
  }

  public static PlainDocument getDigitsAndDotFilter() {
    PlainDocument doc = new PlainDocument();
    doc.setDocumentFilter(new DocumentFilter() {
      @Override
      public void insertString(DocumentFilter.FilterBypass fb, int off, String str, AttributeSet attr)
          throws BadLocationException {
        fb.insertString(off, str.replaceAll("[^0-9.]", ""), attr);  // remove non-digits and dots
      }

      @Override
      public void replace(DocumentFilter.FilterBypass fb, int off, int len, String str, AttributeSet attr)
          throws BadLocationException {
        String text = fb.getDocument().getText(0, fb.getDocument().getLength());
        Matcher m = RE_DOT.matcher(text);
        StringBuffer sb = new StringBuffer();
        int cnt = 0;
        while(m.find()) {
          cnt++;
          if (cnt > 1)
            m.appendReplacement(sb, "");
        }
        m.appendTail(sb);
        fb.replace(off, len, str.replaceAll("[^0-9.]", ""), attr);  // remove non-digits and dots
      }
    });
    return doc;
  }

  public static PlainDocument getLeaveDigitsDotsCommasFilter() {
    return getFilter("[^0-9.,]");
  }
}
