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


import java.util.ArrayList;
import java.util.List;
import java.util.Vector;
import javax.swing.table.DefaultTableModel;
import org.nesvilab.fragpipe.tools.glyco.GlycoMassLoader;
import org.nesvilab.utils.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import umich.ms.glyco.GlycanMod;
import umich.ms.glyco.GlycanParser;
import umich.ms.glyco.GlycanResidue;

public class GlycoModsTableModel extends DefaultTableModel {
    private static final Logger log = LoggerFactory.getLogger(GlycoModsTableModel.class);

    private static final long serialVersionUID = 1L;
    private Class<?>[] classes;
    private String[] colNames;
    private boolean [] canEdit;

    private static final int COL_ENABLED = 0;
    private static final int COL_NAME = 1;
    private static final int COL_MASS = 2;
    private static final int COL_SITES = 3;
    private static final int COL_VARIABLE = 4;
    private static final int COL_MAX_OCCURS = 5;

    /**
     *
     * @param colNames  Names of columns.
     * @param classes  Types of columns.
     * @param canEdit  Which columns are editable.
     * @param data  Can be null.
     */
    public GlycoModsTableModel(String[] colNames, Class<?> [] classes, boolean [] canEdit, Object[][] data) {
        super(data, colNames);
        this.colNames = colNames;
        this.canEdit = canEdit;
        this.classes = classes;
    }

    public Class<?> getColumnClass(int columnIndex) {
        return classes [columnIndex];
    }

    public boolean isCellEditable(int rowIndex, int columnIndex) {
        return canEdit [columnIndex];
    }

    public List<GlycanMod> getModifications(GlycoMassLoader glycoLoader) {
        ArrayList<GlycanMod> list = new ArrayList<>();
        for (Vector<?> row : dataVector) {
            if (row != null && row.size() > 2 && row.get(COL_ENABLED) != null && row.get(COL_NAME) != null) {
                String name = (String) row.get(COL_NAME);
                String[] sites = ((String) row.get(COL_SITES)).trim().split(",");
                Boolean enabled = (Boolean) row.get(COL_ENABLED);
                Boolean isVar = (Boolean) row.get(COL_VARIABLE);
                int maxOccurrences = -1;
                if (row.size() > COL_MAX_OCCURS && row.get(COL_MASS) != null) {
                    Object maxOccursCol = row.get(COL_MAX_OCCURS);
                    if (maxOccursCol != null) {
                        maxOccurrences = (Integer) row.get(COL_MAX_OCCURS);
                    } else {
                        maxOccurrences = 0;
                    }
                }

                if (!StringUtils.isNullOrWhitespace(name)) {
                    // match name to definitions database
                    GlycanMod mod = glycoLoader.glycanMods.get(name);
                    mod.isEnabled = enabled;
                    mod.isVariable = isVar;
                    mod.maxAllowed = maxOccurrences;
                    List<GlycanResidue> requiredRes = new ArrayList<>();
                    for (String resStr : sites) {
                        if (!StringUtils.isNullOrWhitespace(resStr)) {
                            GlycanResidue res = GlycanParser.findResidueName(resStr, glycoLoader.glycanResidues);
                            if (res != null) {
                                requiredRes.add(res);
                            } else {
                                log.error(String.format("Required glycan residue %s in glycan mod %s could not be parsed! Make sure it is the name of a glycan residue in the residues table.", resStr, name));
                            }
                        }
                    }
                    mod.requiredRes = requiredRes;
                    list.add(mod);
                }
            }
        }
        return list;
    }

    public String getMBGresidues() {
        ArrayList<String> residues = new ArrayList<>();
        for (Vector<?> row : dataVector) {
            if ((Boolean) row.get(COL_ENABLED)) {
                residues.add(row.get(COL_NAME) + "(1)");
            }
        }
        return StringUtils.join(residues, ",");
    }
}