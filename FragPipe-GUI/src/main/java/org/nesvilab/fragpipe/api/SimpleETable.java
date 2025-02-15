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

import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.Iterator;
import javax.swing.JComponent;
import javax.swing.ListSelectionModel;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableModel;
import org.netbeans.swing.etable.ETable;

/**
 * Non-editable ETable. Because it's non-editable, quick search is enabled by default. Just start
 * typing while the table has focus.
 *
 * @author Dmitry Avtonomov
 */
public class SimpleETable extends ETable {

  private static final long serialVersionUID = 1L;

  protected ArrayList<WeakReference<? extends JComponent>> listenersNonEmptyData = new ArrayList<>();
  protected ArrayList<WeakReference<? extends JComponent>> listenersNonEmptySelection = new ArrayList<>();

  public SimpleETable() {
    super();
    init();
    setFullyNonEditable(false);
  }

  public SimpleETable(TableModel dm) {
    super(dm);
    init();
    setFullyNonEditable(false);
  }


  public void fireInitialization() {
    TableModel model = getModel();
    if (model instanceof AbstractTableModel) {
      AbstractTableModel atm = (AbstractTableModel) model;
      atm.fireTableDataChanged();
    }
    getSelectionModel().setSelectionInterval(0, 0);
    getSelectionModel().clearSelection();
  }

  @Override
  public void setModel(TableModel dataModel) {
    TableModel old = getModel();
    super.setModel(dataModel);
    if (dataModel != null && !dataModel.equals(old)) {
      initModelListeners();
    }
  }

  @Override
  public void setSelectionModel(ListSelectionModel newModel) {
    ListSelectionModel old = getSelectionModel();
    super.setSelectionModel(newModel);
    if (newModel != null && !newModel.equals(old)) {
      initSelectionListeners();
    }
  }

  private void initModelListeners() {
    getModel().addTableModelListener(e -> {
      boolean notEmpty = getModel().getRowCount() > 0;
      Iterator<WeakReference<? extends JComponent>> it = listenersNonEmptyData.iterator();
      while (it.hasNext()) {
        WeakReference<? extends JComponent> ref = it.next();
        JComponent comp = ref.get();
        if (comp == null) {
          it.remove();
          continue;
        }
        ref.get().setEnabled(notEmpty);
      }
    });
  }

  private void initSelectionListeners() {
    getSelectionModel().addListSelectionListener(e -> {
      int[] sel = getSelectedRows();
      boolean notEmpty = sel.length > 0;
      Iterator<WeakReference<? extends JComponent>> it = listenersNonEmptySelection.iterator();
      while (it.hasNext()) {
        WeakReference<? extends JComponent> ref = it.next();
        JComponent comp = ref.get();
        if (comp == null) {
          it.remove();
          continue;
        }
        comp.setEnabled(notEmpty);
      }
    });
  }

  private void init() {
    putClientProperty("terminateEditOnFocusLost", true);
    initModelListeners();
    initSelectionListeners();
    setRowMargin(3);
    setRowHeight(getRowHeight() + getRowMargin() * 2);
  }

  public void addComponentsEnabledOnNonEmptyData(JComponent component) {
    listenersNonEmptyData.add(new WeakReference<>(component));
  }

  public void addComponentsEnabledOnNonEmptySelection(JComponent component) {
    listenersNonEmptySelection.add(new WeakReference<>(component));
  }
}
