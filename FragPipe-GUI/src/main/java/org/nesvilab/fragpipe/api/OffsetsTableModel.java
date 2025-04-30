package org.nesvilab.fragpipe.api;

import org.nesvilab.fragpipe.util.MassOffsetUtils;
import org.nesvilab.fragpipe.util.MassOffsetUtils.MassOffset;

import javax.swing.table.DefaultTableModel;
import java.util.ArrayList;
import java.util.List;

public class OffsetsTableModel extends DefaultTableModel {
    private static final int COL_MASS = 0;
    private static final int COL_SITES = 1;
    private static final int COL_DIAGNOSTIC_IONS = 2;
    private static final int COL_PEPTIDE_REMAINDERS = 3;
    private static final int COL_FRAGMENT_REMAINDERS = 4;

    public OffsetsTableModel(String[] colNames) {
        super(colNames, 0);
    }

    // convert table data to MassOffset objects
    public List<MassOffset> getOffsets() {
        List<MassOffset> offsets = new ArrayList<>();
        boolean foundZero = false;
        for (int i = 0; i < getRowCount(); i++) {
            float mass;
            try {
                mass = Float.parseFloat(String.valueOf(getValueAt(i, COL_MASS)));
            } catch (NumberFormatException e) {
                continue; // skip invalid mass values
            }
            if (mass == 0) {
                if (foundZero) {
                    continue; // skip duplicate zero mass offsets (due to empty extra rows in the table)
                }
                foundZero = true;
            }
            String[] sites = MassOffsetUtils.getSites((String) getValueAt(i, COL_SITES)).toArray(new String[0]);
            float[] diagnosticIons = parseFloats(String.valueOf(getValueAt(i, COL_DIAGNOSTIC_IONS)));
            float[] peptideRemainders = parseFloats(String.valueOf(getValueAt(i, COL_PEPTIDE_REMAINDERS)));
            float[] fragmentRemainders = parseFloats(String.valueOf(getValueAt(i, COL_FRAGMENT_REMAINDERS)));
            offsets.add(new MassOffset(mass, sites, diagnosticIons, peptideRemainders, fragmentRemainders));
        }
        return offsets;
    }

    private float[] parseFloats(String str) {
        if (str == null || str.isEmpty()) {
            return new float[0];
        }
        String[] parts = str.split("[,\\s;/]+");
        float[] values = new float[parts.length];
        for (int i = 0; i < parts.length; i++) {
            values[i] = Float.parseFloat(parts[i].trim());
        }
        return values;
    }



}
