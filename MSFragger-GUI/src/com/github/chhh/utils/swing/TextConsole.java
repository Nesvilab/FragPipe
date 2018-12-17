/*
 * Copyright (c) 2016 Dmitry Avtonomov
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.github.chhh.utils.swing;

import java.io.IOException;
import javax.swing.JTextPane;
import javax.swing.text.BadLocationException;
import javax.swing.text.StyledDocument;

/**
 * Created by dmitriya on 2015-09-18.
 */
public class TextConsole extends JTextPane implements Appendable {

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
