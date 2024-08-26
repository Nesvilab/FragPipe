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

package com.dmtavt.fragpipe.tools.database;

import com.dmtavt.fragpipe.api.SimpleUniqueTableModel;
import com.dmtavt.fragpipe.api.TableModelColumn;
import com.dmtavt.fragpipe.tools.database.FastaTable.FastaEntry;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class UniqueFastaTableModel extends SimpleUniqueTableModel<FastaEntry> {
  private static final Logger log = LoggerFactory.getLogger(UniqueFastaTableModel.class);

  public UniqueFastaTableModel(List<TableModelColumn<FastaEntry, ?>> cols, int initSize) {
    super(cols, initSize);
  }

  @Override
  public void setValueAt(Object aValue, int rowIndex, int columnIndex) {
    FastaEntry d = data.get(rowIndex);
    int i = data.indexOf(d);
    if (i < 0) {
      throw new IllegalStateException("The object was not found in table model");
    }
    boolean enabled = columnIndex == 0 ? (Boolean) aValue : d.getEnabled();
    String fastaPath = columnIndex == 1 ? (String) aValue : d.getFastaPath();
    boolean hasDecoys = columnIndex == 2 ? (Boolean) aValue : d.hasDecoys;
    int group = columnIndex == 3 ? (Integer) aValue : d.getGroup();

    FastaEntry fnew = new FastaEntry(enabled, fastaPath, hasDecoys, group);
    dataSet(i, fnew);
  }
}
