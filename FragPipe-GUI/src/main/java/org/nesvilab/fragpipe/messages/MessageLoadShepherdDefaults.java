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

import org.nesvilab.fragpipe.api.SearchTypeProp;
import java.util.StringJoiner;

public class MessageLoadShepherdDefaults {

  public final SearchTypeProp type;

  public MessageLoadShepherdDefaults(SearchTypeProp type) {
    this.type = type;
  }

  @Override
  public String toString() {
    return new StringJoiner(", ", MessageLoadShepherdDefaults.class.getSimpleName() + "[",
        "]")
        .add("type=" + type)
        .toString();
  }
}
