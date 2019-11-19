package umich.msfragger.gui.renderers;

import javax.swing.*;
import javax.swing.table.DefaultTableCellRenderer;
import java.awt.*;
import java.text.DecimalFormat;

public class TableCellIntRenderer extends DefaultTableCellRenderer {

    private static final long serialVersionUID = 1L;

    private DecimalFormat df0;

    public TableCellIntRenderer() {
        super();
        setHorizontalAlignment(SwingConstants.LEADING);
        df0 = new DecimalFormat("0");
    }

    @Override
    public Component getTableCellRendererComponent(
            JTable table, Object value, boolean isSelected,
            boolean hasFocus, int row, int column) {

        // First format the cell value as required
        if (value != null) {
            if (value instanceof Integer) {
                value = df0.format(value);
            }
        }

        // And pass it on to parent class
        Component renderer = super.getTableCellRendererComponent(
                table, value, isSelected, hasFocus, row, column);
        return renderer;
    }
}