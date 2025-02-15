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

package org.nesvilab.fragpipe.messages;

import java.awt.Color;
import org.nesvilab.fragpipe.api.Bus;
import org.nesvilab.utils.swing.TextConsole;

public class MessagePrintToConsole {

  public final Color color;
  public final String text;
  public final boolean addNewline;
  public final TextConsole console;

  private MessagePrintToConsole(Color color, String text, boolean addNewline, TextConsole console) {
    this.color = color;
    this.text = text;
    this.addNewline = addNewline;
    this.console = console;
  }

  public static void toConsole(String text, TextConsole console) {
    toConsole(Color.BLACK, text, true, console);
  }

  public static void toConsole(String text, boolean addNewline, TextConsole console) {
    toConsole(Color.BLACK, text, addNewline, console);
  }

  public static void toConsole(Color color, String text, boolean addNewline, TextConsole console) {
    Bus.post(new MessagePrintToConsole(color,text, addNewline, console));
  }
}
