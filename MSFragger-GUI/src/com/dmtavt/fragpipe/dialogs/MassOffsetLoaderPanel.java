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
import com.github.chhh.utils.swing.*;

import javax.swing.*;
import java.awt.*;
import java.text.DecimalFormat;

public class MassOffsetLoaderPanel extends JPanel {
    private UiCheck saveToMSFragger;
    private UiCheck saveToPTMShepherd;
    private UiCheck saveToOPair;

    private UiSpinnerDouble maxOffsetMass;
    private UiSpinnerDouble minOffsetMass;
    private UiSpinnerInt maxOffsetCombos;
    private UiCheck filterMass;

    public MassOffsetLoaderPanel(boolean msfraggerOnly) {
        init(msfraggerOnly);
    }

    private void init(boolean msfraggerOnly) {
        JPanel container = new JPanel();
        container.setLayout(new BoxLayout(container, BoxLayout.Y_AXIS));

        saveToMSFragger = UiUtils.createUiCheck("Load Glycans to MSFragger mass offset list", true);
        saveToPTMShepherd = UiUtils.createUiCheck("Load Glycans to PTM-Shepherd Composition Assignment", true);
        saveToOPair = UiUtils.createUiCheck("Load Glycans to O-Pair Localization", true);

        maxOffsetCombos = new UiSpinnerInt(1, 1, 100, 1, 2);
        String maxOffsetsTooltip = "Generates combinations of provided mass offsets up to the max number (if >1)." +
                "\nUse to search for multiple mass offsets per peptide." +
                "\nWARNING: modifications must be labile for >1 offset per peptide to be found!";
        FormEntry feMaxCombos = Fragpipe.feNoCache(maxOffsetCombos, "Max Combinations of Offsets", Fragpipe.PROP_NOCACHE)
                .label("Max Combinations of Offsets").tooltip(maxOffsetsTooltip).create();

        minOffsetMass = new UiSpinnerDouble(100, -10000, 10000, 1,0, new DecimalFormat("0") );
        maxOffsetMass = new UiSpinnerDouble(3000, -10000, 10000, 1,0, new DecimalFormat("0") );
        String maxMassTooltip = "Filter loaded offsets and combinations to the specified mass range (if enabled).";
        FormEntry feMinMass = Fragpipe.feNoCache(minOffsetMass, "Min Mass", Fragpipe.PROP_NOCACHE)
                .label("Min Mass").tooltip(maxMassTooltip).create();
        FormEntry feMaxMass = Fragpipe.feNoCache(maxOffsetMass, "Max Mass", Fragpipe.PROP_NOCACHE)
                .label("Max Mass").tooltip(maxMassTooltip).create();
        filterMass = UiUtils.createUiCheck("Filter Combinations by Mass", false);

        final MigUtils mu = MigUtils.get();

        if (!msfraggerOnly) {
            JPanel saveOptions = mu.newPanel("Choose Where to Load Glycans", true);
            mu.add(saveOptions, saveToMSFragger).wrap();
            mu.add(saveOptions, saveToPTMShepherd).wrap();
            mu.add(saveOptions, saveToOPair).wrap();
            container.add(saveOptions);
        }

        JPanel entries = mu.newPanel("MSFragger: Multiple Offsets per Peptide Options", true);
        mu.add(entries, feMaxCombos.label()).split();
        mu.add(entries, feMaxCombos.comp).wrap();
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

    public int getMaxCombos() {
        return maxOffsetCombos.getActualValue();
    }
    public boolean useMassFilter() { return filterMass.isSelected(); }
    public double getMaxMass() {
        return maxOffsetMass.getActualValue();
    }
    public double getMinMass() {
        return minOffsetMass.getActualValue();
    }
    public boolean isSaveToMSFragger() {
        return saveToMSFragger.isSelected();
    }
    public boolean isSaveToPTMShepherd() {
        return saveToPTMShepherd.isSelected();
    }
    public boolean isSaveToOPair() {
        return saveToOPair.isSelected();
    }
}
