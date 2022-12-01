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

package com.dmtavt.fragpipe.tabs;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.tools.opair.OPairPanel;
import com.dmtavt.fragpipe.tools.ptmshepherd.PTMSGlycanAssignPanel;
import com.dmtavt.fragpipe.util.GlycoMassLoader;
import com.github.chhh.utils.swing.*;
import com.dmtavt.fragpipe.api.Bus;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.util.List;


public class TabGlyco extends JPanelWithEnablement {
    private static MigUtils mu = MigUtils.get();
    private OPairPanel panelOPair;
    private PTMSGlycanAssignPanel panelGlycanAssign;
    private JPanel panelLoadGlycans;
    private static final Logger log = LoggerFactory.getLogger(TabGlyco.class);
    private UiText textLoadGlycans;

    public TabGlyco() {
        init();
        initMore();
    }

    private void init() {
        mu.layout(this).fillX();

        panelLoadGlycans = createPanelLoadGlycans();
        panelGlycanAssign = new PTMSGlycanAssignPanel();
        panelOPair = new OPairPanel();

        mu.add(this, panelLoadGlycans).spanX().growX().wrap();
        mu.add(this, panelGlycanAssign).spanX().growX().wrap();
        mu.add(this, panelOPair).spanX().growX().wrap();
    }

    private void initMore() {
        Bus.registerQuietly(this);
        Bus.postSticky(this);
    }

    private JPanel createPanelLoadGlycans() {
        JPanel p = mu.newPanel("Glycan Database Options", true);

        JButton btnLoadGlycanMasses = new JButton("Load Glycan Database");
        btnLoadGlycanMasses.addActionListener(this::actionBtnLoadMassOffsets);
        btnLoadGlycanMasses.setToolTipText("Load mass offsets from a file. Supported formats: Byonic, pGlyco, text (.csv, .tsv, .txt, .glyc files)");
        textLoadGlycans = new UiText();

        mu.add(p, btnLoadGlycanMasses).spanX().split().wrap();
        mu.add(p, textLoadGlycans).spanX().growX().wrap();
        return p;
    }

    private void actionBtnLoadMassOffsets(ActionEvent actionEvent) {
        GlycoMassLoader loader = new GlycoMassLoader(false);
        List<String> massStrings = loader.mainLoadOffsets(this);

        if (massStrings.size() > 0) {
            if (loader.optionsPanel.isSaveToMSFragger()) {
                String offsetsText = String.join(" ", massStrings);
                TabMsfragger fraggerTab = Fragpipe.getStickyStrict(TabMsfragger.class);
                fraggerTab.setMassOffsets(offsetsText);
                textLoadGlycans.setText(String.format("Loaded %d unique glycan masses from file", massStrings.size() - 1));
                log.info(String.format("[Glyco Tab load glycans button] Loaded %d unique mass offsets from file", massStrings.size() - 1));
            }
            if (loader.optionsPanel.isSaveToPTMShepherd()) {
                panelGlycanAssign.setGlycanDatabase(loader.glycoFilePath);
            }
            if (loader.optionsPanel.isSaveToOPair()) {
                panelOPair.setGlycanDatabase(loader.glycoFilePath);
                panelOPair.setMaxGlycans(loader.optionsPanel.getMaxCombos());   // also sync O-Pair setting for max combox
            }
        } else {
            textLoadGlycans.setText("No glycans loaded");
            log.info("[Glyco Tab load glycans button] no glycans loaded");
        }
    }
}

