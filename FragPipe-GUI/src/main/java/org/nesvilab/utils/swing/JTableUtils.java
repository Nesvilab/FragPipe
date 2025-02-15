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

package org.nesvilab.utils.swing;

import java.beans.BeanInfo;
import java.beans.Introspector;
import java.beans.PropertyDescriptor;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableModel;

public class JTableUtils {
  public static <T> TableModel createTableModelFromBean(Class<T> beanClass, List<T> list) {
    try {
      BeanInfo beanInfo = Introspector.getBeanInfo(beanClass);
      List<String> columns = new ArrayList<>();
      List<Method> getters = new ArrayList<>();

      for (PropertyDescriptor pd : beanInfo.getPropertyDescriptors()) {
        String name = pd.getName();
        if (name.equals("class")) {
          continue;
        }
        name = Character.toUpperCase(name.charAt(0)) + name.substring(1);
        String[] s = name.split("(?=\\p{Upper})");
        String displayName = "";
        for (String s1 : s) {
          displayName += s1 + " ";
        }

        columns.add(displayName);
        Method m = pd.getReadMethod();
        getters.add(m);
      }

      TableModel model = new AbstractTableModel() {
        @Override
        public String getColumnName(int column) {
          return columns.get(column);
        }

        @Override
        public int getRowCount() {
          return list.size();
        }

        @Override
        public int getColumnCount() {
          return columns.size();
        }

        @Override
        public Object getValueAt(int rowIndex, int columnIndex) {
          try {
            return getters.get(columnIndex).invoke(list.get(rowIndex));
          } catch (Exception e) {
            throw new RuntimeException(e);
          }
        }
      };
      return model;

    } catch (Exception e) {
      throw new RuntimeException(e);
    }
  }
}
