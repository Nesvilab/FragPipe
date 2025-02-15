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

import java.nio.file.Path;

public class NoteConfigDatabase implements INoteConfig {

  public final Path path;
  public final int numEntries;
  public final int decoysCnt;
  public final boolean isBigDatabase;
  public final boolean isValid;

  public NoteConfigDatabase(Path path, int numEntries, int decoysCnt, boolean isBigDatabase, boolean isValid) {
    this.path = path;
    this.numEntries = numEntries;
    this.decoysCnt = decoysCnt;
    this.isBigDatabase = isBigDatabase;
    this.isValid = isValid;
  }

  public NoteConfigDatabase() {
    path = null;
    numEntries = -1;
    decoysCnt = -1;
    isBigDatabase = false;
    this.isValid = false;
  }

  @Override
  public boolean isValid() {
    return isValid;
  }
}
