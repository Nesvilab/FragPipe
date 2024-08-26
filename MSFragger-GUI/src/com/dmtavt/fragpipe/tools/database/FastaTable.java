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

import com.dmtavt.fragpipe.api.Bus;
import com.dmtavt.fragpipe.api.SimpleETable;
import com.dmtavt.fragpipe.api.SimpleUniqueTableModel;
import com.dmtavt.fragpipe.api.TableModelColumn;
import com.dmtavt.fragpipe.messages.MessageDbNewPath;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class FastaTable extends SimpleETable {

  public FastaTable() {
    super(createTableModel());
    setFullyEditable(true);
    fetchModel().addTableModelListener(e -> {
      Bus.post(new MessageDbNewPath(getFastaEntryList()));
    });
  }

  public List<FastaEntry> getFastaEntryList() {
    return fetchModel().dataCopy();
  }

  @SuppressWarnings("unchecked")
  public SimpleUniqueTableModel<FastaEntry> fetchModel() {
    return (SimpleUniqueTableModel<FastaEntry>) getModel();
  }

  private static SimpleUniqueTableModel<FastaEntry> createTableModel() {
    List<TableModelColumn<FastaEntry, ?>> cols = new ArrayList<>();

    TableModelColumn<FastaEntry, Boolean> colEnabled = new TableModelColumn<>("Enabled", Boolean.class, true, FastaEntry::getEnabled);
    TableModelColumn<FastaEntry, String> colFasta = new TableModelColumn<>("FASTA", String.class, true, FastaEntry::getPath);
    TableModelColumn<FastaEntry, Boolean> colHasDecoy = new TableModelColumn<>("Has decoys", Boolean.class, true, FastaEntry::getHasDecoys);
    TableModelColumn<FastaEntry, Integer> colGroup = new TableModelColumn<>("Group", Integer.class, true, FastaEntry::getGroup);

    cols.add(colEnabled);
    cols.add(colFasta);
    cols.add(colHasDecoy);
    cols.add(colGroup);

    return new UniqueFastaTableModel(cols, 0);
  }

  public static class FastaEntry {

    public static final Pattern disallowedFastaPattern = Pattern.compile("[^A-Za-z0-9_:\\\\/.+-]");

    public final boolean enabled;
    public final String path;
    public final boolean hasDecoys;
    public final int group;
    public int proteinCount;
    public int decoyCount;
    public boolean isBigDatabase;

    public FastaEntry(boolean enabled, String path) {
      this(enabled, path, false, 0);
    }

    public FastaEntry(boolean enabled, String path, boolean hasDecoys, int group) {
      this.enabled = enabled;
      this.path = path;
      this.hasDecoys = hasDecoys;
      this.group = group;
    }

    public String checkFastaPath() {
      Matcher matcher = disallowedFastaPattern.matcher(path);
      if (matcher.find()) {
        return "FASTA file path contains disallowed characters: " + matcher.group() + "\nPlease rename them and try again.";
      } else {
        return null;
      }
    }

    public boolean getEnabled() {
      return enabled;
    }

    public String getPath() {
      return path;
    }

    public boolean getHasDecoys() {
      return hasDecoys;
    }

    public int getGroup() {
      return group;
    }

    @Override
    public String toString() {
      return path + "\t" + enabled + "\t" + hasDecoys + "\t" + group;
    }
  }
}
