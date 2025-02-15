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

import org.nesvilab.utils.swing.TextConsole;
import java.nio.file.Path;
import java.util.Collections;
import java.util.List;

public class MessageKillAll {

  public MessageKillAll(REASON reason, List<Path> pathsToDelete, TextConsole console) {
    this.reason = reason;
    this.pathsToDelete = pathsToDelete;
    this.console = console;
  }

  public MessageKillAll(REASON reason, TextConsole console) {
    this.reason = reason;
    this.pathsToDelete = Collections.emptyList();
    this.console = console;
  }

  public enum REASON {NO_REASON, CANT_START_PROCESS, NON_ZERO_RETURN_FROM_PROCESS, USER_ACTION}

  public final REASON reason;
  public final List<Path> pathsToDelete;
  public final TextConsole console;
}
