/*
 * Copyright 2017 Dmitry Avtonomov.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package umich.msfragger.gui;

import java.util.ArrayList;
import java.util.List;
import java.util.Vector;
import javax.swing.table.DefaultTableModel;
import umich.msfragger.params.fragger.Mod;
import umich.msfragger.util.StringUtils;

/**
 *
 * @author Dmitry Avtonomov
 */
public class ModificationsTableModel extends DefaultTableModel {
    
    private static final long serialVersionUID = 1L;
    private Class<?>[] classes;
    private String[] colNames;
    private boolean[]canEdit;
    /** [0] - which col is Enabled, [1] - which col is Site Name, [2] - which col is mass delta. */
    private int[] modMapping;
    private static final int COL_ENABLED = 0;
    private static final int COL_SITES = 1;
    private static final int COL_DELTA = 2;
    
    

    /**
     * 
     * @param colNames  Names of columns.
     * @param classes  Types of columns.
     * @param canEdit  Which columns are editable.
     * @param data  Can be null.
     */
    public ModificationsTableModel(String[] colNames, Class<?> [] classes, boolean [] canEdit, 
            int[] modMapping, Object[][] data) {
        super(data, colNames);
        this.colNames = colNames;
        this.canEdit = canEdit;
        this.classes = classes;
        this.modMapping = modMapping;
    }

    public Class getColumnClass(int columnIndex) {
        return classes [columnIndex];
    }

    public boolean isCellEditable(int rowIndex, int columnIndex) {
        return canEdit [columnIndex];
    }
    
    public List<Mod> getModifications() {
        ArrayList<Mod> list = new ArrayList<>(dataVector.size());
        for (int i = 0; i < dataVector.size(); i++) {
            Vector row = (Vector)dataVector.get(i);
            if (row != null) {
                Double delta = (Double)row.get(COL_DELTA);
                String sites = (String)row.get(COL_SITES);
                Boolean enabled = (Boolean)row.get(COL_ENABLED);
                if (!StringUtils.isNullOrWhitespace(sites) && delta != null) {
                    Mod m = new Mod(delta, sites, enabled);
                    list.add(m);
                }
            } else {
                // row is null? strange
            }
            
        }
        return list;
    }
}
