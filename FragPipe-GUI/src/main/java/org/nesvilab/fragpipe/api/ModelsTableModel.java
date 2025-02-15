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

import org.nesvilab.fragpipe.tools.msbooster.Model;
import java.util.ArrayList;
import java.util.List;
import java.util.Vector;
import javax.swing.table.DefaultTableModel;

/**
 *
 * @author Dmitry Avtonomov
 */
public class ModelsTableModel extends DefaultTableModel {

  private static final long serialVersionUID = 1L;
  private Class<?>[] classes;
  private boolean[] canEdit;


  public ModelsTableModel(String[] colNames, Class<?> [] classes, boolean [] canEdit, Object[][] data) {
    super(data, colNames);
    this.canEdit = canEdit;
    this.classes = classes;
  }

  public Class<?> getColumnClass(int columnIndex) {
    return classes [columnIndex];
  }

  public boolean isCellEditable(int rowIndex, int columnIndex) {
    return canEdit [columnIndex];
  }

  public List<Model> getModels() {
    List<Model> list = new ArrayList<>(dataVector.size());
    for (Vector vector : dataVector) {
      Vector<?> row = (Vector<?>) vector;
      if (row != null && row.size() == 2 && row.get(0) != null && row.get(1) != null) {
        Boolean enabled = (Boolean) row.get(0);
        String name = (String) row.get(1);
        Model m = new Model(name, enabled);
        list.add(m);
      }
    }
    return list;
  }
}
