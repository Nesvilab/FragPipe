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
