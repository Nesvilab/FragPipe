/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package umich.msfragger.gui.api;

import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.Iterator;
import javax.swing.JComponent;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableModel;
import org.netbeans.swing.etable.ETable;

/**
 * Non-editable ETable. Because it's non-editable, quick search is enabled by 
 * default. Just start typing while the table has focus.
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
        setFullyNonEditable(true);
    }

    public SimpleETable(TableModel dm) {
        super(dm);
        init();
        setFullyNonEditable(true);
    }

    
    public void fireInitialization() {
        TableModel model = getModel();
        if (model instanceof AbstractTableModel) {
            AbstractTableModel atm = (AbstractTableModel)model;
            atm.fireTableDataChanged();
        }
        getSelectionModel().setSelectionInterval(0, 0);
        getSelectionModel().clearSelection();
    }
    
    @Override
    public void setModel(TableModel dataModel) {
        TableModel old = getModel();
        super.setModel(dataModel);
        if (dataModel != null && !dataModel.equals(old))
            initModelListeners();
    }

    @Override
    public void setSelectionModel(ListSelectionModel newModel) {
        ListSelectionModel old = getSelectionModel();
        super.setSelectionModel(newModel);
        if (newModel != null && !newModel.equals(old))
            initSelectionListeners();
    }

    private void initModelListeners() {
        getModel().addTableModelListener(new TableModelListener() {
            @Override
            public void tableChanged(TableModelEvent e) {
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
            }
        });
    }
    
    private void initSelectionListeners() {
        getSelectionModel().addListSelectionListener(new ListSelectionListener() {
            @Override
            public void valueChanged(ListSelectionEvent e) {
                int[] sel = getSelectedRows();
                boolean notEmpty = sel.length > 0;
                Iterator<WeakReference<? extends JComponent>> it = listenersNonEmptySelection.iterator();
                while(it.hasNext()) {
                    WeakReference<? extends JComponent> ref = it.next();
                    JComponent comp = ref.get();
                    if (comp == null) {
                        it.remove();
                        continue;
                    }
                    comp.setEnabled(notEmpty);
                }
            }
        });
    }
    
    private void init() {
        initModelListeners();
        initSelectionListeners();
    }
    
    public void addComponentsEnabledOnNonEmptyData(JComponent component) {
        listenersNonEmptyData.add(new WeakReference<>(component));
    }
    
    public void addComponentsEnabledOnNonEmptySelection(JComponent component) {
        listenersNonEmptySelection.add(new WeakReference<>(component));
    }
}
