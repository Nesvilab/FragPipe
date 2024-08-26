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

package com.dmtavt.fragpipe.messages;

import com.dmtavt.fragpipe.tools.database.FastaTable.FastaEntry;
import java.util.ArrayList;
import java.util.List;

public class NoteConfigDatabase implements INoteConfig {

  public final List<FastaEntry> fastaEntryList;
  public final boolean isValid;

  public NoteConfigDatabase(List<FastaEntry> fastaEntryList, boolean isValid) {
    this.fastaEntryList = fastaEntryList;
    this.isValid = isValid;
  }

  public NoteConfigDatabase() {
    fastaEntryList = new ArrayList<>(0);
    this.isValid = false;
  }

  public boolean hasBigDatabase() {
    return fastaEntryList.stream().anyMatch(e -> e.isBigDatabase);
  }

  public int getProteinCount() {
    return fastaEntryList.stream().mapToInt(e -> e.proteinCount).sum();
  }

  public int getDecoyCount() {
    return fastaEntryList.stream().mapToInt(e -> e.decoyCount).sum();
  }

  @Override
  public boolean isValid() {
    return isValid;
  }
}
