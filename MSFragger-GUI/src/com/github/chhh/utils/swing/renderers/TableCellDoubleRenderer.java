/* 
 * Copyright (C) 2018 Dmitry Avtonomov
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package com.github.chhh.utils.swing.renderers;

import java.awt.Component;
import java.text.DecimalFormat;
import javax.swing.JTable;
import javax.swing.SwingConstants;
import javax.swing.table.DefaultTableCellRenderer;

/**
 *
 * @author Dmitry Avtonomov
 */
public class TableCellDoubleRenderer extends DefaultTableCellRenderer {

    private static final long serialVersionUID = 1L;

    private DecimalFormat df0;

    public TableCellDoubleRenderer() {
        super();
        setHorizontalAlignment(SwingConstants.LEADING);
        df0 = new DecimalFormat("#.##########");
    }

    @Override
    public Component getTableCellRendererComponent(
            JTable table, Object value, boolean isSelected,
            boolean hasFocus, int row, int column) {

        // First format the cell value as required
        if (value != null) {
            if (value instanceof Double || value instanceof Float) {
                value = df0.format((Double)value);
            }
        }

        // And pass it on to parent class
        Component renderer = super.getTableCellRendererComponent(
                table, value, isSelected, hasFocus, row, column);
        return renderer;
    }
}
