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
import javax.swing.table.DefaultTableModel;
import java.util.ArrayList;
import java.util.List;

public class BatchTableModel extends DefaultTableModel {
    private static final int COL_NAME = 0;
    private static final int COL_WORKFLOW = 1;
    private static final int COL_MANIFEST = 2;
    private static final int COL_OUTPUT = 3;
    private static final int COL_TOOLS = 4;
    private static final int COL_FASTA = 5;
    private static final int COL_RAM = 6;
    private static final int COL_THREADS = 7;

    public BatchTableModel(String[] colNames) {
        super(colNames, 0);
    }

    // convert table data to BatchRuns
    public List<BatchRun> getRuns() {
        List<BatchRun> runs = new ArrayList<>();
        for (int i = 0; i < getRowCount(); i++) {
            BatchRun run = new BatchRun(
                    getValueAt(i, COL_NAME).toString(),
                    getValueAt(i, COL_WORKFLOW).toString().replace("\"", ""),
                    getValueAt(i, COL_MANIFEST).toString().replace("\"", ""),
                    getValueAt(i, COL_OUTPUT).toString().replace("\"", ""),
                    getValueAt(i, COL_TOOLS).toString().replace("\"", ""),
                    getValueAt(i, COL_FASTA).toString(),
                    Integer.parseInt(getValueAt(i, COL_RAM).toString()),
                    Integer.parseInt(getValueAt(i, COL_THREADS).toString()));
            runs.add(run);
        }
        return runs;
    }

}
