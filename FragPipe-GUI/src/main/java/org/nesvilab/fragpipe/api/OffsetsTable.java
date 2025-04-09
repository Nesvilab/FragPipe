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

import org.nesvilab.fragpipe.util.MassOffsetUtils;
import org.nesvilab.fragpipe.util.MassOffsetUtils.MassOffset;
import org.nesvilab.utils.swing.StringRepresentable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.*;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

public class OffsetsTable extends JTable implements StringRepresentable {
    private static final Logger log = LoggerFactory.getLogger(OffsetsTable.class);
    public final OffsetsTableModel model;
    public final Object[] colNames;
    public final Function<List<MassOffset>, Object[][]> modsToData;

    public OffsetsTable(OffsetsTableModel model, Object[] colNames, Function<List<MassOffset>, Object[][]> modsToData) {
        super(model);
        this.model = model;
        this.colNames = colNames;
        this.modsToData = modsToData;
    }

    public void setData(List<MassOffset> offsets) {
        Object[][] data = modsToData.apply(offsets);
        model.setDataVector(data, colNames);
    }

    @Override
    public String asString() {
        return model.getOffsets().stream().map(MassOffset::toString).collect(Collectors.joining(MassOffsetUtils.DELIMITER));
    }

    @Override
    public void fromString(String s) {
        String[] split = s.split(MassOffsetUtils.DELIMITER);
        List<MassOffset> offsets = new ArrayList<>();
        for (String chunk : split) {
            offsets.add(new MassOffset(chunk));
        }
        Object[][] vec = modsToData.apply(offsets);
        model.setRowCount(0);
        model.setRowCount(vec.length);
        model.setDataVector(vec, colNames);
    }
}
