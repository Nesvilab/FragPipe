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

public class MessageLcmsGroupAction {

  public MessageLcmsGroupAction(Type type) {
    this.type = type;
  }

  public enum Type {
    CONSECUTIVE_EXP, CONSECUTIVE_REP, BY_PARENT_DIR, BY_FILE_NAME, SET_EXP, SET_REP, SET_DDA, SET_DIA, SET_GPF_DIA, SET_DIA_QUANT, SET_DIA_LIB, SET_DDA_PLUS, CLEAR_EXP, CLEAR_REP
  }
  public final Type type;
}
