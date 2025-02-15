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

import java.awt.Component;
import javax.swing.JTable;
import javax.swing.SwingConstants;
import javax.swing.table.DefaultTableCellRenderer;

/**
 *
 * @author Dmitry Avtonomov
 */
public class TableCellDoubleRenderer extends DefaultTableCellRenderer {

    private static final long serialVersionUID = 1L;

    public TableCellDoubleRenderer() {
        super();
        setHorizontalAlignment(SwingConstants.LEADING);
    }

    @Override
    public Component getTableCellRendererComponent(
            JTable table, Object value, boolean isSelected,
            boolean hasFocus, int row, int column) {

        // And pass it on to parent class
        Component renderer = super.getTableCellRendererComponent(
                table, value, isSelected, hasFocus, row, column);
        return renderer;
    }
}
