/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package umich.msfragger.gui.api;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 *
 * @author Dmitry Avtonomov
 */
public class SimpleUniqueTableModel<T> extends SimpleTableModel<T> {

    private static final long serialVersionUID = 1L;
    
    private final Set<T> dataSet;
    
    public SimpleUniqueTableModel(List<TableModelColumn<T, ?>> cols, int initSize) {
        super(cols, initSize);
        dataSet = new HashSet<>();
    }
    
    @Override
    public synchronized boolean dataAddAll(Collection<? extends T> c) {
        ArrayList<T> unique = new ArrayList<>(c.size());
        for (T t : c) {
            if (t != null && !dataSet.contains(t)) {
                unique.add(t);
                dataSet.add(t);
            }
        }
        return super.dataAddAll(unique);
    }

    @Override
    public synchronized boolean dataAdd(T t) {
        if (t != null && !dataSet.contains(t)) {
            dataSet.add(t);
            return super.dataAdd(t);
        }
        return false;
    }

    @Override
    public synchronized void dataClear() {
        super.dataClear();
        dataSet.clear();
    }

    @Override
    public synchronized boolean dataRemoveAll(Collection<?> c) {
        dataSet.removeAll(c);
        return super.dataRemoveAll(c);
    }

    @Override
    public synchronized T dataSet(int index, T element) {
        T prevElem = super.dataSet(index, element);
        dataSet.remove(prevElem);
        dataSet.add(element);
        return prevElem;
    }

    @Override
    public synchronized T dataRemove(int index) {
        T old = super.dataRemove(index);
        dataSet.remove(old);
        return old;
    }
    
    
}
