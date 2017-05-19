/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package umich.msfragger.gui.api;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import javax.swing.table.AbstractTableModel;

/**
 *
 * @author Dmitry Avtonomov
 */
public class SimpleTableModel<T> extends AbstractTableModel {

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
