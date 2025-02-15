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

/**
 *
 * @author Dmitry Avtonomov
 */
public class TableModelColumn<I, O> {
    public final String name;
    public final Class<O> clazz;
    public final boolean isEditable;
    public final DataConverter<I, O> converter;

    public TableModelColumn(String name, Class<O> clazz, boolean isEditable, DataConverter<I, O> converter) {
        this.name = name;
        this.clazz = clazz;
        this.isEditable = isEditable;
        this.converter = converter;
    }
}
