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