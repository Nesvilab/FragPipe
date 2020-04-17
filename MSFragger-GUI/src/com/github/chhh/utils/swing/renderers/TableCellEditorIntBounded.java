package com.github.chhh.utils.swing.renderers;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.border.LineBorder;
import java.awt.*;

public class TableCellEditorIntBounded extends DefaultCellEditor {
    private static final Logger log = LoggerFactory.getLogger(TableCellEditorIntBounded.class);

    private final int min;
    private final int max;
    private static final Border RED = new LineBorder(Color.red);
    private static final Border BLACK = new LineBorder(Color.black);
    private JTextField textField;

    public TableCellEditorIntBounded(JTextField textField, int min, int max) {
        super(textField);
        this.textField = textField;
        this.textField.setHorizontalAlignment(JTextField.RIGHT);
        this.max = max;
        this.min = min;
    }

    @Override
    public boolean stopCellEditing() {
        try {
            int v = Integer.parseInt(textField.getText());
            if (v < min || v > max) {
                throw new NumberFormatException();
            }
        } catch (NumberFormatException e) {
            textField.setBorder(RED);
            return false;
        }
        return super.stopCellEditing();
    }

    @Override
    public Component getTableCellEditorComponent(JTable table,
                                                 Object value, boolean isSelected, int row, int column) {
        textField.setBorder(BLACK);
        return super.getTableCellEditorComponent(
                table, value, isSelected, row, column);
    }

    private void showTextfieldError() {
        JTextField textField = (JTextField) getComponent();
        textField.setBorder(new LineBorder(Color.red));
        textField.selectAll();
        textField.requestFocusInWindow();
    }
}
