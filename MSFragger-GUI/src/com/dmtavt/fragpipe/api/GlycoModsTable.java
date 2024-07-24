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

package com.dmtavt.fragpipe.api;

import umich.ms.glyco.GlycanMod;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.*;
import java.util.List;
import java.util.function.Function;

public class GlycoModsTable extends JTable {
    private static final Logger log = LoggerFactory.getLogger(ModsTable.class);
    public final GlycoModsTableModel model;
    public final Object[] colNames;
    public final Function<List<GlycanMod>, Object[][]> modsToData;

    public GlycoModsTable(GlycoModsTableModel model, Object[] colNames, Function<List<GlycanMod>, Object[][]> modsToData) {
        super(model);
        this.model = model;
        this.colNames = colNames;
        this.modsToData = modsToData;
    }

    public void setData(List<GlycanMod> mods) {
        Object[][] data = modsToData.apply(mods);
        model.setDataVector(data, colNames);
    }

}
