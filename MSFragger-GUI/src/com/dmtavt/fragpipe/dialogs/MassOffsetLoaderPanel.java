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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with FragPipe.  If not, see <https://www.gnu.org/licenses/>.
 */

package com.dmtavt.fragpipe.dialogs;

import com.dmtavt.fragpipe.Fragpipe;
import com.github.chhh.utils.swing.*;

import javax.swing.*;
import java.awt.*;
import java.text.DecimalFormat;

public class MassOffsetLoaderPanel extends JPanel {
    private UiSpinnerDouble maxOffsetMass;
    private UiSpinnerInt maxOffsetCombos;
    private UiCheck filterMaxMass;

    public MassOffsetLoaderPanel() {
        initMore();
    }

    private void initMore() {
        JPanel container = new JPanel();
        container.setLayout(new BoxLayout(container, BoxLayout.Y_AXIS));

        maxOffsetCombos = new UiSpinnerInt(1, 1, 100, 1, 2);
        String maxOffsetsTooltip = "Generates combinations of provided mass offsets up to the max number (if >1)." +
                "\nUse to search for multiple mass offsets per peptide." +
                "\nWARNING: modifications must be labile for >1 offset per peptide to be found!";
        FormEntry feMaxCombos = Fragpipe.feNoCache(maxOffsetCombos, "Max Combinations of Offsets", Fragpipe.PROP_NOCACHE)
                .label("Max Combinations of Offsets").tooltip(maxOffsetsTooltip).create();

        maxOffsetMass = new UiSpinnerDouble(3000, -10000, 100000, 1,0, new DecimalFormat("0") );
        String maxMassTooltip = "Filter loaded offsets and combinations to a maximum mass value (if enabled).";
        FormEntry feMaxMass = Fragpipe.feNoCache(maxOffsetMass, "Max Mass", Fragpipe.PROP_NOCACHE)
                .label("Max Mass").tooltip(maxMassTooltip).create();
        filterMaxMass = UiUtils.createUiCheck("Filter Combinations by Max Mass", false);

        final MigUtils mu = MigUtils.get();
        JPanel entries = mu.newPanel("Mass Offset Loading Options", true);
        mu.add(entries, feMaxCombos.label()).split();
        mu.add(entries, feMaxCombos.comp).wrap();
        mu.add(entries, filterMaxMass).split();
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
    public double getMaxMass() {
        return maxOffsetMass.getActualValue();
    }
    public boolean useMassFilter() {
        return filterMaxMass.isSelected();
    }
}
