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
import org.nesvilab.fragpipe.api.GlycoResiduesTable;
import org.nesvilab.fragpipe.api.GlycoResiduesTableModel;
import org.nesvilab.fragpipe.cmd.ToolingUtils;
import org.nesvilab.utils.swing.renderers.TableCellDoubleRenderer;
import org.nesvilab.utils.swing.renderers.TableCellIntRenderer;
import org.nesvilab.utils.swing.renderers.TableCellIntSpinnerEditor;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import umich.ms.glyco.GlycanResidue;

import javax.swing.*;
import java.awt.*;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.Collections;
import java.util.List;

public class GlycanModEditDialog extends javax.swing.JDialog {
    private static final Logger log = LoggerFactory.getLogger(GlycanResidueEditDialog.class);
    private List<? extends GlycanResidue> initResidues;

    private JPanel p;
    private JButton buttonOK;
    private JButton buttonCancel;
    private GlycoResiduesTableModel model;
    public GlycoResiduesTable table;
    private Frame parent;
    private int dialogResult = JOptionPane.CLOSED_OPTION;
    private static final String[] MOD_TABLE_COL_NAMES = {"Name", "Mass", "Alternate Names (optional)",
            "is labile?", "Y prob +", "Y prob -", "Elemental Composition", "Required Residue(s)", "Intrinsic Charge"};
    public static final String TAB_PREFIX = "glycan-database.";

    public GlycanModEditDialog(java.awt.Frame parent, List<? extends GlycanResidue> initialResidues) {
        super(parent);
        this.initResidues = initialResidues == null ? Collections.emptyList() : initialResidues;
        this.parent = parent;
        init();
        postInit();
    }

    public GlycoResiduesTableModel getModel() {
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
        Dimension dim = new Dimension(800, 600);
        this.setPreferredSize(dim);
        this.setLayout(new BorderLayout());

        p = new JPanel();
        JScrollPane scroll = new JScrollPane(p,
                ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        this.setContentPane(scroll);

        buttonOK = new JButton("Save");
        buttonCancel = new JButton("Cancel");
        table = createTable();

        MigLayout layout = new MigLayout(new LC().fillX());//.debug());
        p.setLayout(layout);
        p.add(new JScrollPane(table), new CC().grow().spanX().wrap());
        p.add(buttonOK, new CC().tag("ok").split());
        p.add(buttonCancel, new CC().tag("cancel").wrap());

        setContentPane(scroll);
        setModal(true);
        setModalityType(ModalityType.APPLICATION_MODAL);
        setTitle("Edit Glycan Modification Definitions:");
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

    private GlycoResiduesTable createTable() {
        Object[][] data = GlycoResiduesTableModel.convertGlycoModsToData(initResidues);
        model = new GlycoResiduesTableModel(
                MOD_TABLE_COL_NAMES,
                new Class<?>[]{String.class, Double.class, String.class, Boolean.class, Double.class, Double.class, String.class, String.class, Integer.class},
                new boolean[]{true, true, true, true, true, true, true, true, true, true, true},
                data);
        final GlycoResiduesTable t = new GlycoResiduesTable(model, MOD_TABLE_COL_NAMES, GlycoResiduesTableModel::convertGlycoModsToData);
        Fragpipe.rename(t, "table.glyco-mods", TAB_PREFIX);

        t.setToolTipText("<html>Edit the glycan modification definitions (also possible by editing the file manually).<br/>\n" +
                "Name: required - the name of the glycan residue in glycan databases that will be loaded<br/>\n" +
                "Mass: required - the monoisotopic mass of the glycan residue<br/>\n" +
                "Alternate Names: optional - other names that the glycan might be called<br/>\n" +
                "is labile?: optional - if the residue is expected to be lost from fragment ions<br/>\n" +
                "Y prob +: required unless labile - empirical score factor for finding Y ions. Default 5<br/>\n" +
                "Y prob -: required unless labile - empirical score factor for missing Y ions. Default 0.5<br/>\n" +
                "Elemental Composition: optional - used for Skyline conversion, not required for searches.<br/>\n" +
                "Required Residue(s): optional - specify glycan residue names (comma separated) to only allow this mod on residue(s)." +
                "Intrinsic Charge: optional - specify modification charge (e.g., sodium adduct would be +1). Used to adjust elemental composition when calculating ion m/z");
        t.setDefaultRenderer(Float.class, new TableCellDoubleRenderer());
        t.setDefaultRenderer(Integer.class, new TableCellIntRenderer());

        // set cell editor for max occurs for var mods
        DefaultCellEditor cellEditorMaxOccurs = new TableCellIntSpinnerEditor(0, 5, 1);
        t.setDefaultEditor(Integer.class, cellEditorMaxOccurs);
        t.setFillsViewportHeight(true);

        return t;
    }

}