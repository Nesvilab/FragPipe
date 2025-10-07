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

package org.nesvilab.fragpipe.dialogs;

import org.nesvilab.fragpipe.Fragpipe;
import org.nesvilab.fragpipe.api.OffsetsTable;
import org.nesvilab.fragpipe.api.OffsetsTableModel;
import org.nesvilab.fragpipe.cmd.ToolingUtils;
import org.nesvilab.fragpipe.tabs.TabMsfragger;
import org.nesvilab.fragpipe.util.MassOffsetUtils;
import org.nesvilab.utils.swing.renderers.TableCellDoubleRenderer;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.*;
import java.awt.*;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.Collections;
import java.util.List;

import static org.nesvilab.fragpipe.tabs.TabMsfragger.TABLE_OFFSET_COL_NAMES;

public class DetailedOffsetEditDialog extends javax.swing.JDialog {
    private static final Logger log = LoggerFactory.getLogger(DetailedOffsetEditDialog.class);
    private List<MassOffsetUtils.MassOffset> offsets;

    private JPanel p;
    private JButton buttonOK;
    private JButton buttonCancel;
    private OffsetsTableModel model;
    public OffsetsTable table;
    private Frame parent;
    private int dialogResult = JOptionPane.CLOSED_OPTION;
    private static final MassOffsetUtils.MassOffset zeroOffset = new MassOffsetUtils.MassOffset(0.0f, new String[0], new float[0], new float[0], new float[0]);

    public DetailedOffsetEditDialog(java.awt.Frame parent, List<MassOffsetUtils.MassOffset> offsets) {
        super(parent);
        this.offsets = offsets.isEmpty() ? Collections.singletonList(zeroOffset) : offsets;
        this.parent = parent;
        init();
        postInit();
    }

    public OffsetsTableModel getModel() {
        return model;
    }

    @Override
    public void dispose() {
        super.dispose();
    }

    private void postInit() {
        this.setDefaultCloseOperation(DISPOSE_ON_CLOSE);
        this.setLocationRelativeTo(parent);
        pack();
    }

    private void init() {
        Dimension dim = new Dimension(500, 600);
        this.setPreferredSize(dim);
        this.setLayout(new BorderLayout());

        p = new JPanel();
        JScrollPane scroll = new JScrollPane(p, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        this.setContentPane(scroll);

        buttonOK = new JButton("Save offsets");
        buttonCancel = new JButton("Cancel");
        table = createTableOffsets();

        JButton buttonAddRow = new JButton("Add row");
        buttonAddRow.addActionListener(e -> {
            int selectedRow = table.getSelectedRow();
            if (selectedRow != -1) {
                // insert a new row after the selected row
                model.insertRow(selectedRow + 1, new Object[]{"", "", "", "", ""});
            } else {
                model.addRow(new Object[]{"", "", "", "", ""});
            }
        });
        JButton buttonRemoveRow = new JButton("Remove row");
        buttonRemoveRow.addActionListener(e -> {
            int selectedRow = table.getSelectedRow();
            if (selectedRow != -1) {
                model.removeRow(selectedRow);
            } else {
                // if no row is selected, remove the last row
                int rowCount = model.getRowCount();
                if (rowCount > 0) {
                    model.removeRow(rowCount - 1);
                }
            }
        });

        JButton buttonClearTable = new JButton("Clear table");
        buttonClearTable.addActionListener(e -> {
            int rowCount = model.getRowCount();
            while (rowCount > 0) {
                model.removeRow(rowCount - 1);
                rowCount--;
            }
            model.addRow(new Object[]{0.0f, "", "", "", ""});
        });

        MigLayout layout = new MigLayout(new LC().fillX());//.debug());
        p.setLayout(layout);
        p.add(new JScrollPane(table), new CC().grow().spanX().wrap());
        p.add(buttonAddRow, new CC().split().tag("addrow"));
        p.add(buttonRemoveRow, new CC().split().tag("removerow"));
        p.add(buttonClearTable, new CC().split().tag("cleartable"));

        p.add(buttonOK, new CC().tag("ok").split());
        p.add(buttonCancel, new CC().tag("cancel").wrap());

        setContentPane(scroll);
        setModal(true);
        setModalityType(ModalityType.APPLICATION_MODAL);
        setTitle("Edit Detailed Mass Offsets:");
        setIconImages(ToolingUtils.loadIcon());
        getRootPane().setDefaultButton(buttonOK);

        buttonOK.addActionListener(e -> onOK());
        buttonCancel.addActionListener(e -> onCancel());
        // call onCancel() when cross is clicked
        setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
        addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e) {
                onCancel();
            }
        });
        // call onCancel() on ESCAPE
        p.registerKeyboardAction(e -> onCancel(), KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0),
                JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);
    }

    private void onOK() {
        // add your code here
        dialogResult = JOptionPane.OK_OPTION;
        dispose();
    }

    private void onCancel() {
        // add your code here if necessary
        dialogResult = JOptionPane.CANCEL_OPTION;
        dispose();
    }

    public int getDialogResult() {
        return dialogResult;
    }

    private OffsetsTable createTableOffsets() {
        model = new OffsetsTableModel(TABLE_OFFSET_COL_NAMES);
        OffsetsTable t = new OffsetsTable(model, TABLE_OFFSET_COL_NAMES, TabMsfragger::convertOffsetsToTableData);
        t.setData(offsets);

        Fragpipe.rename(t, "table.detailed-offsets", TabMsfragger.TAB_PREFIX);
        t.setToolTipText("<html>Detailed mass offset table.<br/>Mass is required. " +
                "Specify allowed amino acid site(s) if desired, leave blank to allow anywhere.<br/>" +
                "Specify labile ions (diagnostic, peptide remainder, and fragment remainder) if desired, leave blank for nonlabile offsets.");
        t.setDefaultRenderer(Double.class, new TableCellDoubleRenderer());
        t.setFillsViewportHeight(true);

        return t;
    }
}
