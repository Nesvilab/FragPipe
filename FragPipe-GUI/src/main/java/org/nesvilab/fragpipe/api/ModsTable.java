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

package org.nesvilab.fragpipe.api;

import org.nesvilab.fragpipe.exceptions.ValidationException;
import org.nesvilab.utils.swing.StringRepresentable;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;
import javax.swing.JTable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.nesvilab.fragpipe.tools.fragger.Mod;

public class ModsTable extends JTable implements StringRepresentable {
  private static final Logger log = LoggerFactory.getLogger(ModsTable.class);
  public static final String MOD_STRING_DELIMITER = "; ";
  public final ModsTableModel model;
  public final Object[] colNames;
  public final Function<List<Mod>, Object[][]> modsToData;

  public ModsTable(ModsTableModel model, Object[] colNames, Function<List<Mod>, Object[][]> modsToData) {
    super(model);
    this.model = model;
    this.colNames = colNames;
    this.modsToData = modsToData;
  }

  public void setData(List<Mod> mods) {
    Object[][] data = modsToData.apply(mods);
    model.setDataVector(data, colNames);
  }

  @Override
  public String asString() {
    return model.getModifications().stream()
        .map(Mod::asString).collect(Collectors.joining(MOD_STRING_DELIMITER));
  }

  @Override
  public void fromString(String s) {
    String[] split = s.split(MOD_STRING_DELIMITER);
    List<Mod> mods = new ArrayList<>();
    for (String chunk : split) {
      try {
        mods.add(Mod.fromString(chunk));
      } catch (ValidationException e) {
        log.warn("Could not deserialize MSFragger mod encoded as string: {}", s);
      }
    }
    Object[][] vec = modsToData.apply(mods);
    model.setRowCount(0);
    model.setRowCount(vec.length);
    model.setDataVector(vec, colNames);
  }
}
