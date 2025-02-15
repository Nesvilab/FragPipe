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

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import javax.swing.table.AbstractTableModel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author Dmitry Avtonomov
 */
public class SimpleTableModel<T> extends AbstractTableModel {
    private static final Logger log = LoggerFactory.getLogger(SimpleTableModel.class);

    private static final long serialVersionUID = 2122960171520812299L;

    protected ArrayList<TableModelColumn<T, ?>> cols;
    protected ArrayList<T> data;

    public SimpleTableModel(List<TableModelColumn<T, ?>> cols, int initSize) {
        this.cols = new ArrayList<>(cols.size());
        this.cols.addAll(cols);
        this.data = new ArrayList<>(initSize);
    }

    public int dataSize() {
        return data.size();
    }
    
    public synchronized ArrayList<T> dataCopy() {
        ArrayList<T> copy = new ArrayList<>(data);
        return copy;
    }
    
    public synchronized T dataRemove(int index) {
        T removed = data.remove(index);
        if (removed != null) {
            fireTableRowsDeleted(index, index);
        }
        return removed;
    }

    public T dataGet(int index) {
        return data.get(index);
    }

    @Override
    public void setValueAt(Object aValue, int rowIndex, int columnIndex) {
        super.setValueAt(aValue, rowIndex, columnIndex);
        log.debug("setValueAt() called for row: {}, col: {}", rowIndex, columnIndex);
    }

    public synchronized T dataSet(int index, T element) {
        T previousElement = data.set(index, element);
        if (previousElement != null && !previousElement.equals(element))
            fireTableRowsUpdated(index, index);
        return previousElement;
    }

    public synchronized boolean dataRemoveAll(Collection<?> c) {
        boolean hasChanged = data.removeAll(c);
        if (hasChanged) {
            fireTableDataChanged();
        }
        return hasChanged;
    }

    public synchronized boolean dataAdd(T e) {
        boolean hasChanged = data.add(e);
        if (hasChanged) {
            fireTableRowsInserted(data.size()-1, data.size()-1);
        }
        return hasChanged;
    }

    public synchronized void dataClear() {
        int originalSize = data.size();
        data.clear();
        if (originalSize != data.size())
            fireTableDataChanged();
    }

    public synchronized boolean dataAddAll(Collection<? extends T> c) {
        if (c.isEmpty())
            return false;
        int originalSize = data.size();
        boolean hasChanged = data.addAll(c);
        if (hasChanged)
            fireTableRowsInserted(originalSize, c.size()-1);
        return hasChanged;
    }
    
    @Override
    public boolean isCellEditable(int row, int column) {
        return cols.get(column).isEditable;
    }

    @Override
    public String getColumnName(int column) {
        return cols.get(column).name;
    }
    
    @Override
    public Class<?> getColumnClass(int columnIndex) {
        return cols.get(columnIndex).clazz;
    }

    @Override
    public int getRowCount() {
        return data.size();
    }

    @Override
    public int getColumnCount() {
        return cols.size();
    }

    @Override
    public Object getValueAt(int rowIndex, int columnIndex) {
        T row = data.get(rowIndex);
        return cols.get(columnIndex).converter.convert(row);
    }
}
