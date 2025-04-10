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

import org.nesvilab.fragpipe.util.BatchRun;
import javax.swing.*;
import java.util.List;
import java.util.function.Function;

public class BatchTable extends JTable {
    public final BatchTableModel model;
    public final Object[] colNames;
    public final Function<List<BatchRun>, Object[][]> runsToData;

    public BatchTable(BatchTableModel model, Object[] colNames, Function<List<BatchRun>, Object[][]> runsToData) {
        super(model);
        this.model = model;
        this.colNames = colNames;
        this.runsToData = runsToData;
    }

    public void setData(List<BatchRun> runs) {
        Object[][] data = runsToData.apply(runs);
        model.setDataVector(data, colNames);
    }

}
