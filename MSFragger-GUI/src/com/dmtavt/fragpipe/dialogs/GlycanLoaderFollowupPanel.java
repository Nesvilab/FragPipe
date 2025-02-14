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

package com.dmtavt.fragpipe.dialogs;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.api.GlycoModsTable;
import com.dmtavt.fragpipe.api.GlycoModsTableModel;
import com.dmtavt.fragpipe.tabs.TabMsfragger;
import com.dmtavt.fragpipe.tools.fragger.MsfraggerParams;
import umich.ms.glyco.GlycanMod;
import com.dmtavt.fragpipe.tools.glyco.GlycoMassLoader;
import com.github.chhh.utils.swing.*;
import com.github.chhh.utils.swing.renderers.TableCellDoubleRenderer;
import com.github.chhh.utils.swing.renderers.TableCellIntRenderer;
import com.github.chhh.utils.swing.renderers.TableCellIntSpinnerEditor;
import net.miginfocom.layout.CC;

import javax.swing.*;
import java.awt.*;
import java.text.DecimalFormat;
import java.util.List;

public class GlycanLoaderFollowupPanel extends JPanel {
    private UiSpinnerDouble maxOffsetMass;
    private UiSpinnerDouble minOffsetMass;
    private UiSpinnerInt maxOffsetCombos;
    private UiSpinnerInt maxModsCombos;
    private UiCheck filterMass;
    private MigUtils mu;
    public GlycoModsTable tableGlycoMods;
    private final GlycoMassLoader glycoLoader;

    private static final String[] TABLE_MODS_COL_NAMES = {"Enabled", "Name", "Mass", "Site(s)", "is variable?", "Max occurrences"};
    public static final String TAB_PREFIX = "glycan-database.";

    public GlycanLoaderFollowupPanel(GlycoMassLoader loader) {
        this.glycoLoader = loader;
        init();
    }

    private void init() {
        JPanel container = new JPanel();
        container.setLayout(new BoxLayout(container, BoxLayout.Y_AXIS));
        mu = MigUtils.get();

        maxOffsetCombos = new UiSpinnerInt(1, 1, 100, 1, 2);
        String maxOffsetsTooltip = "Generates combinations of provided glycans up to the max number (if >1)." +
                "\nUse to search for multiple glycans per peptide." +
                "\nNOT recommended for N-glycans." +
                "\nWARNING: modifications must be labile for >1 offset per peptide to be found!";
        FormEntry feMaxCombos = Fragpipe.feNoCache(maxOffsetCombos, "Max Combinations of Glycans", Fragpipe.PROP_NOCACHE)
                .label("Max Combinations of Glycans").tooltip(maxOffsetsTooltip).create();

        maxModsCombos = new UiSpinnerInt(1, 1, 100, 1, 2);
        String maxModsTooltip = "Allow up to this many modifications to be combined (if mods are enabled)." +
                "\nNote: modifications are applied after glycan combinations are calculated.";
        FormEntry feMaxModsCombos = Fragpipe.feNoCache(maxModsCombos, "Max Combinations of Glycan Mods", Fragpipe.PROP_NOCACHE)
                .label("Max Combinations of Glycan Mods").tooltip(maxModsTooltip).create();

        minOffsetMass = new UiSpinnerDouble(100, -10000, 10000, 1,0, new DecimalFormat("0") );
        maxOffsetMass = new UiSpinnerDouble(3000, -10000, 10000, 1,0, new DecimalFormat("0") );
        String maxMassTooltip = "Filter loaded glycans and combinations to the specified mass range (if enabled).";
        FormEntry feMinMass = Fragpipe.feNoCache(minOffsetMass, "Min Mass", Fragpipe.PROP_NOCACHE)
                .label("Min Mass").tooltip(maxMassTooltip).create();
        FormEntry feMaxMass = Fragpipe.feNoCache(maxOffsetMass, "Max Mass", Fragpipe.PROP_NOCACHE)
                .label("Max Mass").tooltip(maxMassTooltip).create();
        filterMass = UiUtils.createUiCheck("Filter Glycans by Mass", false);

        JPanel glycoModsPanel = createPanelGlycoMods();
        container.add(glycoModsPanel);

        JPanel entries = mu.newPanel("Glycan combinations and filtering:", true);
        mu.add(entries, feMaxCombos.label()).split();
        mu.add(entries, feMaxCombos.comp).wrap();
        mu.add(entries, feMaxModsCombos.label()).split();
        mu.add(entries, feMaxModsCombos.comp).wrap();
        mu.add(entries, filterMass).split();
        mu.add(entries, feMinMass.label()).split();
        mu.add(entries, feMinMass.comp).split();
        mu.add(entries, feMaxMass.label()).split();
        mu.add(entries, feMaxMass.comp).wrap();
        container.add(entries);

        JScrollPane scroll = new JScrollPane(container);
        this.setLayout(new BorderLayout());
        this.add(scroll, BorderLayout.CENTER);
    }

    private JPanel createPanelGlycoMods() {
        JPanel p = mu.newPanel("Define Glycan Modifications (optional)", true);

        tableGlycoMods = createTableGlycoMods();
        SwingUtilities.invokeLater(() -> {
            TabMsfragger.setJTableColSize(tableGlycoMods, 0, 20, 150, 50);
        });
        JScrollPane varModsScroll = new JScrollPane(tableGlycoMods, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        p.add(varModsScroll, new CC().minHeight("100px").maxHeight("150px").spanX().wrap());

        return p;
    }

    private GlycoModsTable createTableGlycoMods() {
        Object[][] data = convertGlycoModsToData(glycoLoader.glycanModDefinitions);
        GlycoModsTableModel m = new GlycoModsTableModel(
                TABLE_MODS_COL_NAMES,
                new Class<?>[]{Boolean.class, String.class, Float.class, String.class, Boolean.class, Integer.class},
                new boolean[]{true, false, false, true, true, true},
                data);
        final GlycoModsTable t = new GlycoModsTable(m, TABLE_MODS_COL_NAMES, GlycanLoaderFollowupPanel::convertGlycoModsToData);
        Fragpipe.rename(t, "table.glyco-mods", TAB_PREFIX);

        t.setToolTipText("");
        t.setDefaultRenderer(Float.class, new TableCellDoubleRenderer());
        t.setDefaultRenderer(Integer.class, new TableCellIntRenderer());

        // set cell editor for max occurs for var mods
        DefaultCellEditor cellEditorMaxOccurs = new TableCellIntSpinnerEditor(0, 5, 1);
        t.setDefaultEditor(Integer.class, cellEditorMaxOccurs);
        t.setFillsViewportHeight(true);

        return t;
    }

    private static Object[][] convertGlycoModsToData(List<GlycanMod> mods) {
        Object[][] data = new Object[MsfraggerParams.VAR_MOD_COUNT_MAX][TABLE_MODS_COL_NAMES.length];
        for (int i = 0; i < data.length; i++) {
            data[i][0] = false;
            for (int j = 1; j < TABLE_MODS_COL_NAMES.length; j++) {
                data[i][j] = null;
            }
        }
        if (mods.size() > data.length) {
            throw new IllegalStateException("loaded mods list length is longer than currently supported");
        }

        for (int i = 0; i < mods.size(); i++) {
            GlycanMod m = mods.get(i);
            String[] reqResStrs = new String[m.requiredRes.size()];
            for (int k = 0; k < m.requiredRes.size(); k++) {
                reqResStrs[k] = m.requiredRes.get(k).name;
            }
            data[i][0] = m.isEnabled;
            data[i][1] = m.name;
            data[i][2] = m.mass;
            data[i][3] = String.join(",", reqResStrs);
            data[i][4] = m.isVariable;
            data[i][5] = m.maxAllowed;
        }
        return data;
    }

    // Check if any glycan mods have been enabled in the table
    public boolean isGlycanModsChanged() {
        List<GlycanMod> tableMods = tableGlycoMods.model.getModifications(glycoLoader);
        for (GlycanMod mod: tableMods) {
            if (mod.isEnabled) {
                return true;
            }
        }
        return false;
    }

    public int getMaxCombos() {
        return maxOffsetCombos.getActualValue();
    }
    public int getMaxModCombos() {
        return maxModsCombos.getActualValue();
    }
    public boolean useMassFilter() { return filterMass.isSelected(); }
    public double getMaxMass() {
        return maxOffsetMass.getActualValue();
    }
    public double getMinMass() {
        return minOffsetMass.getActualValue();
    }
}
