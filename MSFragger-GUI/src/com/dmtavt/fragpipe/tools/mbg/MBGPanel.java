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

package com.dmtavt.fragpipe.tools.mbg;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.dialogs.GlycanResidueEditDialog;
import com.dmtavt.fragpipe.dialogs.MBGchooseResiduesDialog;
import com.dmtavt.fragpipe.tools.glyco.GlycoMassLoader;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.swing.FileChooserUtils;
import com.github.chhh.utils.swing.FormEntry;
import com.github.chhh.utils.swing.JPanelBase;
import com.github.chhh.utils.swing.MigUtils;
import com.github.chhh.utils.swing.UiCheck;
import com.github.chhh.utils.swing.UiSpinnerDouble;
import com.github.chhh.utils.swing.UiSpinnerInt;
import com.github.chhh.utils.swing.UiText;
import com.github.chhh.utils.swing.UiUtils;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.nio.file.Path;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;
import javax.swing.*;
import javax.swing.filechooser.FileFilter;
import javax.swing.filechooser.FileNameExtensionFilter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import umich.ms.glyco.GlycanResidue;

import static com.dmtavt.fragpipe.tabs.TabGlyco.stopJTableEditing;

public class MBGPanel extends JPanelBase {
    private static final Logger log = LoggerFactory.getLogger(com.dmtavt.fragpipe.tools.opair.OPairPanel.class);
    private static final String PREFIX = "mbg.";
    private JPanel pTop;
    private JPanel pContent;
    private static final MigUtils mu = MigUtils.get();
    private UiCheck checkRun;

    private static final String PROP_minPSMs = "min_psms";
    private static final String PROP_minGlycans = "min_glycans";
    private static final String PROP_maxGlycanQ = "max_glycan_q";
    private static final String PROP_residues_to_add = "residues_to_add";
    private static final String PROP_MBG_FDR = "fdr";

    private UiSpinnerDouble uiSpinnerMaxQ;
    private UiSpinnerDouble uiSpinnerFDR;
    private UiSpinnerInt uiSpinnerMinPSMs;
    private UiSpinnerInt uiSpinnerMinGlycans;
    private UiText uiTextResiduesToAdd;

    private GlycoMassLoader glycoLoader;

    public MBGPanel(GlycoMassLoader glycoLoader) {
        super();
        this.glycoLoader = glycoLoader;
    }

    @Override
    protected ItemSelectable getRunCheckbox() {
        return checkRun;
    }

    @Override
    protected Component getEnablementToggleComponent() {
        return pContent;
    }

    @Override
    protected String getComponentNamePrefix() {
        return PREFIX;
    }

    @Override
    protected void init() {
        mu.layout(this, mu.lcFillXNoInsetsTopBottom());
        mu.border(this, "MS1 Glycoform Inference");

        pTop = createPanelTop();

        pContent = mu.newPanel(null, mu.lcFillXNoInsetsTopBottom());

        uiSpinnerMaxQ = UiSpinnerDouble.builder(0.01, 0, 1.0, 0.01)
                .setFormat(new DecimalFormat("0.00#")).setCols(3).create();
        uiSpinnerMaxQ.setToolTipText("Maximum glycan q-value (from composition assignment) to consider a given glycoPSM as a starting point for glycoform inference");
        FormEntry feMaxQ = new FormEntry(PROP_maxGlycanQ, "Max Glycan q-value",
                uiSpinnerMaxQ);

        uiSpinnerFDR = UiSpinnerDouble.builder(0.01, 0, 1.0, 0.01)
                .setFormat(new DecimalFormat("0.00#")).setCols(4).create();
        uiSpinnerFDR.setToolTipText("FDR for matching between glycans");
        FormEntry feFDR = new FormEntry(PROP_MBG_FDR, "MBG FDR", uiSpinnerFDR);

        uiSpinnerMinPSMs = new UiSpinnerInt(5, 1, 10000, 1);
        uiSpinnerMinPSMs.setToolTipText("Minimum number of glycoPSMs for a given glycosite to perform glycan inference at that site");
        FormEntry feMinPSMs = new FormEntry(PROP_minPSMs, "Min PSMs", uiSpinnerMinPSMs);

        uiSpinnerMinGlycans = new UiSpinnerInt(2, 1, 10000, 1);
        uiSpinnerMinGlycans.setToolTipText("Minimum number of glycans observed at a given glycosite to perform glycan inference at that site");
        FormEntry feMinGlycans = new FormEntry(PROP_minGlycans, "Min Glycans", uiSpinnerMinGlycans);

        uiTextResiduesToAdd = UiUtils.uiTextBuilder().create();
        uiTextResiduesToAdd.setPreferredSize(new Dimension(200, 25));
        FormEntry feResiduesToAdd = mu.feb(PROP_residues_to_add, uiTextResiduesToAdd)
                .label("Residues to Add:").tooltip("Choose which glycan residues/mods are considered for MBG matching").create();

        mu.add(pContent, feMaxQ.label(), mu.ccL()).split(8);
        mu.add(pContent, feMaxQ.comp).split();
        mu.add(pContent, feMinPSMs.label(), mu.ccR());
        mu.add(pContent, feMinPSMs.comp);
        mu.add(pContent, feMinGlycans.label(), mu.ccR());
        mu.add(pContent, feMinGlycans.comp);
        mu.add(pContent, feFDR.label(), mu.ccR());
        mu.add(pContent, feFDR.comp).wrap();

        JButton btnChooseMBGresidues = new JButton("Pick Residues");
        btnChooseMBGresidues.addActionListener(this::actionBtnChooseMBGresidues);
        btnChooseMBGresidues.setToolTipText("Select glycan residues and/or mods from the reference tables to use.");

        mu.add(pContent, btnChooseMBGresidues).split(3);
        mu.add(pContent, feResiduesToAdd.label(), mu.ccR());
        mu.add(pContent, feResiduesToAdd.comp).wrap();

        mu.add(this, pTop).growX().wrap();
        mu.add(this, pContent).growX().wrap();
    }

    private JPanel createPanelTop() {

        JPanel p = mu.newPanel(null, mu.lcFillXNoInsetsTopBottom());

        checkRun = new UiCheck("Run Glycoform Inference", null, false);
        checkRun.setName("run-mbg");
        JLabel info = new JLabel("<html>MBG: Glycoform inference.");

        mu.add(p, checkRun).split();
        return p;
    }

    /**
     * Open the table editor to modify the glycan residue definitions file
     * @param actionEvent
     */
    private void actionBtnChooseMBGresidues(ActionEvent actionEvent) {
        MBGchooseResiduesDialog tableDialog = new MBGchooseResiduesDialog(SwingUtils.findParentFrame(this), glycoLoader);

        tableDialog.setVisible(true);
        if (tableDialog.getDialogResult() != JOptionPane.OK_OPTION) {
            return;
        }

        // get new data
        stopJTableEditing(tableDialog.tableGlycoMods);
        String enabledResidues = tableDialog.tableGlycoMods.model.getMBGresidues();
        uiTextResiduesToAdd.setText(enabledResidues);
    }

    public boolean isRun() {
        return SwingUtils.isEnabledAndChecked(checkRun);
    }

    @Override
    public void initMore() {

        updateEnabledStatus(this, true);
        super.initMore();
    }

    public MBGParams getMBGParams() {
        MBGParams params = new MBGParams();
        params.setMaxGlycanQ(uiSpinnerMaxQ.getActualValue());
        params.setMinPSMs(uiSpinnerMinPSMs.getActualValue());
        params.setIntGlycans(uiSpinnerMinGlycans.getActualValue());
        params.setResiduesToAdd(uiTextResiduesToAdd.getNonGhostText());
        return params;
    }

}
