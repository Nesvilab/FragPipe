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

import java.io.IOException;
import javax.swing.JTextPane;
import javax.swing.text.BadLocationException;
import javax.swing.text.StyledDocument;

public class TextConsoleSimple extends JTextPane implements Appendable {

  @Override
  public Appendable append(CharSequence csq) throws IOException {
    //append(csq.toString());
    StyledDocument doc = getStyledDocument();
    try {
      doc.insertString(doc.getLength(), csq.toString(), null);
    } catch (BadLocationException e) {
      e.printStackTrace();
    }
    return this;
  }

  @Override
  public Appendable append(CharSequence csq, int start, int end) throws IOException {
    //append(csq.subSequence(start, end).toString());
    StyledDocument doc = getStyledDocument();
    try {
      doc.insertString(doc.getLength(), csq.subSequence(start, end).toString(), null);
    } catch (BadLocationException e) {
      e.printStackTrace();
    }
    return this;
  }

  @Override
  public Appendable append(char c) throws IOException {
    //append(Character.toString(c));
    StyledDocument doc = getStyledDocument();
    try {
      doc.insertString(doc.getLength(), String.valueOf(c), null);
    } catch (BadLocationException e) {
      e.printStackTrace();
    }
    return this;
  }
}
