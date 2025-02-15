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
package org.nesvilab.utils;

import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.DocumentFilter;
import javax.swing.text.PlainDocument;

/**
 *
 * @author Dmitry Avtonomov
 */
public class DocumentFilters {
    
    private DocumentFilters() {}
    
    /**
     * Filter that removes all substrings matching the provided regular expression.
     * @param filteredCharsRegex
     * @return 
     */
    public static PlainDocument getFilter(final String filteredCharsRegex) {
        PlainDocument doc = new PlainDocument();
        doc.setDocumentFilter(new DocumentFilter() {
            @Override
            public void insertString(DocumentFilter.FilterBypass fb, int off, String str, AttributeSet attr)
                    throws BadLocationException {
                fb.insertString(off, str.replaceAll(filteredCharsRegex, ""), attr);
            }

            @Override
            public void replace(DocumentFilter.FilterBypass fb, int off, int len, String str, AttributeSet attr)
                    throws BadLocationException {
                fb.replace(off, len, str.replaceAll(filteredCharsRegex, ""), attr);
            }
        });
        return doc;
    }
    
    public static PlainDocument getDigitsOnlyFilter() {
        PlainDocument doc = new PlainDocument();
        doc.setDocumentFilter(new DocumentFilter() {
            @Override
            public void insertString(DocumentFilter.FilterBypass fb, int off, String str, AttributeSet attr)
                    throws BadLocationException {
                fb.insertString(off, str.replaceAll("\\D++", ""), attr);  // remove non-digits
            }

            @Override
            public void replace(DocumentFilter.FilterBypass fb, int off, int len, String str, AttributeSet attr)
                    throws BadLocationException {
                fb.replace(off, len, str.replaceAll("\\D++", ""), attr);  // remove non-digits
            }
        });
        return doc;
    }
    
    public static PlainDocument getDigitsAndDotFilter() {
        PlainDocument doc = new PlainDocument();
        doc.setDocumentFilter(new DocumentFilter() {
            @Override
            public void insertString(DocumentFilter.FilterBypass fb, int off, String str, AttributeSet attr)
                    throws BadLocationException {
                fb.insertString(off, str.replaceAll("[^0-9\\.]", ""), attr);  // remove non-digits and dots
            }

            @Override
            public void replace(DocumentFilter.FilterBypass fb, int off, int len, String str, AttributeSet attr)
                    throws BadLocationException {
                String text = fb.getDocument().getText(0, fb.getDocument().getLength());
                Pattern dot = Pattern.compile("\\.");
                Matcher m = dot.matcher(text);
                StringBuffer sb = new StringBuffer();
                int cnt = 0;
                while(m.find()) {
                    cnt++;
                    if (cnt > 1)
                        m.appendReplacement(sb, "");
                }
                m.appendTail(sb);
                fb.replace(off, len, str.replaceAll("[^0-9\\.]", ""), attr);  // remove non-digits and dots
            }
        });
        return doc;
    }
    
    public static PlainDocument getDigitsAndDotCommaFilter() {
        PlainDocument doc = new PlainDocument();
        doc.setDocumentFilter(new DocumentFilter() {
            @Override
            public void insertString(DocumentFilter.FilterBypass fb, int off, String str, AttributeSet attr)
                    throws BadLocationException {
                fb.insertString(off, str.replaceAll("[^0-9\\.,]", ""), attr);  // remove non-digits and dots/commas
            }

            @Override
            public void replace(DocumentFilter.FilterBypass fb, int off, int len, String str, AttributeSet attr)
                    throws BadLocationException {
//                String text = fb.getDocument().getText(0, fb.getDocument().getLength());
//                Matcher m = Pattern.compile("[\\.,]").matcher(text);
//                StringBuffer sb = new StringBuffer();
//                int cnt = 0;
//                while(m.find()) {
//                    cnt++;
//                    if (cnt > 1)
//                        m.appendReplacement(sb, "");
//                }
//                m.appendTail(sb);
                fb.replace(off, len, str.replaceAll("[^0-9\\.,]", ""), attr);  // remove non-digits and dots/commas
            }
        });
        return doc;
    }
}