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
import java.util.HashMap;
import java.util.List;
import java.util.Vector;
import javax.swing.table.DefaultTableModel;
import org.nesvilab.utils.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import umich.ms.glyco.GlycanMod;
import umich.ms.glyco.GlycanResidue;
import umich.ms.util.ElementalComposition;

public class GlycoResiduesTableModel extends DefaultTableModel {
    private static final Logger log = LoggerFactory.getLogger(GlycoResiduesTableModel.class);

    private static final long serialVersionUID = 1L;
    private Class<?>[] classes;
    private String[] colNames;
    private boolean [] canEdit;

    private static final int COL_NAME = 0;
    private static final int COL_MASS = 1;
    private static final int COL_ALTNAMES = 2;
    private static final int COL_LABILE = 3;
    private static final int COL_PROB1 = 4;
    private static final int COL_PROB2 = 5;
    private static final int COL_ELEMENTAL = 6;
    private static final int COL_REQUIRED_RES = 7;
    private static final int COL_INTRINSIC_CHARGE = 8;

    private static final int NUM_COLS = 7;
    private static final int NUM_MOD_COLS = 9;

    /**
     *
     * @param colNames  Names of columns.
     * @param classes  Types of columns.
     * @param canEdit  Which columns are editable.
     * @param data  Can be null.
     */
    public GlycoResiduesTableModel(String[] colNames, Class<?> [] classes, boolean [] canEdit, Object[][] data) {
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

    public ArrayList<GlycanResidue> getResidues() {
        int printIndex = 0;
        ArrayList<GlycanResidue> list = new ArrayList<>();
        for (Vector<?> row : dataVector) {
            GlycanResidue res = parseResidueDefinition(row, printIndex);
            if (res != null) {
                printIndex++;
                list.add(res);
            }
        }
        return list;
    }

    public ArrayList<GlycanMod> getMods(HashMap<String, GlycanResidue> glycanResidues) {
        int printIndex = glycanResidues.size();
        ArrayList<GlycanMod> list = new ArrayList<>(dataVector.size());
        for (Vector<?> row : dataVector) {
            GlycanResidue res = parseResidueDefinition(row, printIndex);
            String requiredRes = row.get(COL_REQUIRED_RES) != null ? (String) row.get(COL_REQUIRED_RES) : "";
            int intrinsicCharge = row.get(COL_INTRINSIC_CHARGE) != null ? (Integer) row.get(COL_INTRINSIC_CHARGE) : 0;
            if (res != null) {
                printIndex++;
                GlycanMod mod = new GlycanMod(res, requiredRes, glycanResidues, intrinsicCharge);
                list.add(mod);
            }
        }
        return list;
    }

    private GlycanResidue parseResidueDefinition(Vector<?> row, int printIndex) {
        if (row != null && row.size() > 2 && row.get(COL_NAME) != null && row.get(COL_MASS) != null) {
            Double mass;
            if (row.get(COL_MASS) instanceof Double) {
                mass = (Double) row.get(COL_MASS);
            } else {
                throw new RuntimeException(row.get(COL_MASS) + " is not Double.");
            }
            String name = (String) row.get(COL_NAME);

            String[] altNames;
            if (row.get(COL_ALTNAMES) != null) {
                altNames = ((String) row.get(COL_ALTNAMES)).split(",");
            } else {
                altNames = new String[0];
            }
            boolean isLabile = row.get(COL_LABILE) != null ? (Boolean) row.get(COL_LABILE) : false;
            double yProbPlus, yProbMinus;
            if (isLabile) {
                yProbPlus = -1;
                yProbMinus = -1;
            } else {
                if (row.get(COL_PROB1) != null && row.get(COL_PROB2) != null) {
                    yProbPlus = (Double) row.get(COL_PROB1);
                    yProbMinus = (Double) row.get(COL_PROB2);
                } else {
                    yProbPlus = -1;
                    yProbMinus = -1;
                }
            }
            ElementalComposition elementalComp;
            Object rowComp = row.get(COL_ELEMENTAL);
            if (rowComp != null) {
                try {
                    elementalComp = (ElementalComposition) rowComp;
                } catch (ClassCastException ex) {
                    elementalComp = new ElementalComposition("");   // empty cell returns an empty String
                }
            } else {
                elementalComp = new ElementalComposition("");
            }

            if (!StringUtils.isNullOrWhitespace(name) && mass != null) {
                return new GlycanResidue(name, mass, altNames, yProbPlus, yProbMinus, isLabile, elementalComp, printIndex);
            }
        }
        return null;
    }

    // for glycan residues definitions table
    public static Object[][] convertGlycoResiduesToData(List<? extends GlycanResidue> residues) {
        Object[][] data = new Object[residues.size() + 3][NUM_COLS];
        for (int i = 0; i < data.length; i++) {
            for (int j = 0; j < NUM_COLS; j++) {
                data[i][j] = null;
            }
        }
        if (residues.size() > data.length) {
            throw new IllegalStateException("loaded residues list length is longer than currently supported");
        }

        for (int i = 0; i < residues.size(); i++) {
            GlycanResidue m = residues.get(i);
            data[i][0] = m.name;
            data[i][1] = m.mass;
            data[i][2] = String.join(",", m.alternateNames);
            data[i][3] = m.isLabile;
            data[i][4] = m.yProbPlus == -1 ? null : m.yProbPlus;    // show an empty box for labile residues with prob = -1
            data[i][5] = m.yProbMinus == -1 ? null : m.yProbMinus;
            data[i][6] = m.elementalComp;
        }
        return data;
    }

    // for glycan mods definitions table
    public static Object[][] convertGlycoModsToData(List<? extends GlycanResidue> mods) {
        Object[][] data = new Object[mods.size() + 3][NUM_MOD_COLS];
        for (int i = 0; i < data.length; i++) {
            for (int j = 0; j < NUM_MOD_COLS; j++) {
                data[i][j] = null;
            }
        }
        if (mods.size() > data.length) {
            throw new IllegalStateException("loaded residues list length is longer than currently supported");
        }

        for (int i = 0; i < mods.size(); i++) {
            GlycanMod m = (GlycanMod) mods.get(i);
            data[i][0] = m.name;
            data[i][1] = m.mass;
            data[i][2] = String.join(",", m.alternateNames);
            data[i][3] = m.isLabile;
            data[i][4] = m.yProbPlus == -1 ? null : m.yProbPlus;    // show an empty box for labile residues with prob = -1
            data[i][5] = m.yProbMinus == -1 ? null : m.yProbMinus;
            data[i][6] = m.elementalComp;
            data[i][7] = m.getRequiredResStr();
            data[i][8] = m.intrinsicCharge == 0 ? null : m.intrinsicCharge;
        }
        return data;
    }
}