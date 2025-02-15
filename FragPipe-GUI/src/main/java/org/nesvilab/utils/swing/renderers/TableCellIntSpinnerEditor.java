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

package org.nesvilab.utils.swing.renderers;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.util.EventObject;

public class TableCellIntSpinnerEditor extends DefaultCellEditor {
    private static final Logger log = LoggerFactory.getLogger(TableCellIntSpinnerEditor.class);
    JSpinner spinner;
    JSpinner.DefaultEditor editor;
    JTextField textField;
    boolean valueSet;
    final int min;
    final int max;

    public TableCellIntSpinnerEditor() {
        this(Integer.MIN_VALUE, Integer.MAX_VALUE, 0);
    }

    /**
     * Presents small up/down arrows next to the number in the text field. Still editable from keyboard.
     * @param min Min value for spinner.
     * @param max Max value for spinner.
     * @param value Must be between min and max.
     */
    public TableCellIntSpinnerEditor(int min, int max, int value) {
        super(new JTextField());
        this.min = min;
        this.max = max;
        spinner = new JSpinner(new SpinnerNumberModel(value, min, max, 1));
        editor = ((JSpinner.DefaultEditor) spinner.getEditor());
        textField = editor.getTextField();
        textField.addFocusListener(new FocusListener() {
            public void focusGained(FocusEvent fe) {
                log.debug("SpinnerEditor got focus");
                SwingUtilities.invokeLater(() -> {
                    if (valueSet) {
                        textField.setCaretPosition(1);
                    }
                });
            }

            public void focusLost(FocusEvent fe) {
            }
        });
        textField.addActionListener(ae -> stopCellEditing());
    }

    // Prepares the spinner component and returns it.
    @Override
    public Component getTableCellEditorComponent(
            JTable table, Object value, boolean isSelected, int row, int column
    ) {
        if (!valueSet) {
            spinner.setValue(value == null ? 1 : value);
        }
        SwingUtilities.invokeLater(() -> textField.requestFocus());
        return spinner;
    }

    @Override
    public boolean isCellEditable(EventObject eo) {
        log.debug("SpinnerEditor isCellEditable called");
        if (eo instanceof KeyEvent) {
            KeyEvent ke = (KeyEvent) eo;
            log.debug("key event: " + ke.getKeyChar());
            textField.setText(String.valueOf(ke.getKeyChar()));
            //textField.select(1,1);
            //textField.setCaretPosition(1);
            //textField.moveCaretPosition(1);
            valueSet = true;
        } else {
            valueSet = false;
        }
        return true;
    }

    // Returns the spinners current value.
    @Override
    public Object getCellEditorValue() {
        return spinner.getValue();
    }

    @Override
    public boolean stopCellEditing() {
        log.debug("SpinnerEditor stopping edit");
        try {
            editor.commitEdit();
            spinner.commitEdit();
        } catch (java.text.ParseException e) {
            JOptionPane.showMessageDialog(null, "Invalid value, discarding.");
        }
        return super.stopCellEditing();
    }
}
