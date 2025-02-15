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

package org.nesvilab.fragpipe.internal;

import org.nesvilab.fragpipe.cmd.CmdBase;

public class CmdBaseNode {
  public final CmdBase cmdBase;

  public CmdBaseNode(CmdBase cmdBase) {
    this.cmdBase = cmdBase;
  }

  @Override
  public String toString() {
//    return new StringJoiner(", ", CmdBaseNode.class.getSimpleName() + "[", "]")
//        .add("cmdBase=" + cmdBase)
//        .toString();
    return cmdBase == null ? "n/a" : cmdBase.getCmdName();

  }
}
