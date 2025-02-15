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

import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class UniqueLcmsFilesTableModel extends SimpleUniqueTableModel<InputLcmsFile> {
  private static final Logger log = LoggerFactory.getLogger(UniqueLcmsFilesTableModel.class);

  public UniqueLcmsFilesTableModel(
      List<TableModelColumn<InputLcmsFile, ?>> cols, int initSize) {
    super(cols, initSize);
  }

  @Override
  public void setValueAt(Object aValue, int rowIndex, int columnIndex) {
    InputLcmsFile orig = data.get(rowIndex);
    int i = data.indexOf(orig);
    if (i < 0) {
      log.error("The object was not found in table model");
      throw new IllegalStateException("The object was not found in table model");
    }
    String exp = columnIndex == 1 ? (String) aValue : orig.getExperiment();
    Integer rep = columnIndex == 2 ? (Integer) aValue : orig.getReplicate();
    String dataType = columnIndex == 3 ? (String) aValue : orig.getDataType();
    InputLcmsFile fnew = new InputLcmsFile(orig.getPath(), exp, rep, dataType);
    dataSet(i, fnew);
  }
}
