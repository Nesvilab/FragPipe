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
