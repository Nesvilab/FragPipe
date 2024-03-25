package com.dmtavt.fragpipe.api;

import umich.ms.glyco.GlycanResidue;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.*;
import java.util.List;
import java.util.function.Function;

public class GlycoResiduesTable extends JTable {
    private static final Logger log = LoggerFactory.getLogger(ModsTable.class);
    public final GlycoResiduesTableModel model;
    public final Object[] colNames;
    public final Function<List<? extends GlycanResidue>, Object[][]> modsToData;

    public GlycoResiduesTable(GlycoResiduesTableModel model, Object[] colNames, Function<List<? extends GlycanResidue>, Object[][]> modsToData) {
        super(model);
        this.model = model;
        this.colNames = colNames;
        this.modsToData = modsToData;
    }

    public void setData(List<GlycanResidue> residues) {
        Object[][] data = modsToData.apply(residues);
        model.setDataVector(data, colNames);
    }
}