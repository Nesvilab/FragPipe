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
import java.awt.Component;
import java.awt.ItemSelectable;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.filechooser.FileFilter;
import javax.swing.filechooser.FileNameExtensionFilter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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
    private static final String PROP_glycanDB = "glycan_db";

    private UiSpinnerDouble uiSpinnerMaxQ;
    private UiSpinnerInt uiSpinnerMinPSMs;
    private UiSpinnerInt uiSpinnerMinGlycans;
    private UiText uiTextGlycanDBFile;

    public MBGPanel() {
        super();
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

        uiSpinnerMinPSMs = new UiSpinnerInt(5, 1, 10000, 1);
        uiSpinnerMinPSMs.setToolTipText("Minimum number of glycoPSMs for a given glycosite to perform glycan inference at that site");
        FormEntry feMinPSMs = new FormEntry(PROP_minPSMs, "Min PSMs", uiSpinnerMinPSMs);

        uiSpinnerMinGlycans = new UiSpinnerInt(2, 1, 10000, 1);
        uiSpinnerMinGlycans.setToolTipText("Minimum number of glycans observed at a given glycosite to perform glycan inference at that site");
        FormEntry feMinGlycans = new FormEntry(PROP_minGlycans, "Min Glycans", uiSpinnerMinGlycans);

        String tooltipGlycanDBFile = "Glycan database file (.txt). Will use an internal default glycan list if not provided.";
        uiTextGlycanDBFile = UiUtils.uiTextBuilder().cols(85).create();
        List<FileFilter> glycFilters = new ArrayList<>();
        FileFilter filter = new FileNameExtensionFilter("Glycan Database file (txt)", "txt");
        glycFilters.add(filter);
        FormEntry feGlycanDBFile = mu.feb(PROP_glycanDB, uiTextGlycanDBFile)
                .label("Glycan Database").tooltip(tooltipGlycanDBFile).create();
        JButton btnBrosweGlycanDBFile = feGlycanDBFile.browseButton("Browse", tooltipGlycanDBFile,
                () -> FileChooserUtils.builder("Select custom glycan database file")
                        .approveButton("Select").mode(FileChooserUtils.FcMode.FILES_ONLY).acceptAll(false).multi(false).filters(glycFilters)
                        .paths(Stream.of(Fragpipe.propsVarGet(PROP_glycanDB))).create(),
                paths -> {
                    if (paths != null && !paths.isEmpty()) {
                        String path = paths.get(0).toString();
                        Fragpipe.propsVarSet(PROP_glycanDB, path);
                        uiTextGlycanDBFile.setText(path);
                    }
                });

        mu.add(pContent, feMaxQ.label(), mu.ccR());
        mu.add(pContent, feMaxQ.comp);
        mu.add(pContent, feMinPSMs.label(), mu.ccR());
        mu.add(pContent, feMinPSMs.comp);
        mu.add(pContent, feMinGlycans.label(), mu.ccR());
        mu.add(pContent, feMinGlycans.comp).wrap();

        mu.add(pContent, feGlycanDBFile.label()).split().spanX();
        mu.add(pContent, btnBrosweGlycanDBFile);
        mu.add(pContent, feGlycanDBFile.comp).wrap();

        mu.add(this, pTop).growX().wrap();
        mu.add(this, pContent).growX().wrap();
    }

    private JPanel createPanelTop() {

        JPanel p = mu.newPanel(null, mu.lcFillXNoInsetsTopBottom());

        checkRun = new UiCheck("Run Glycoform Inference", null, false);
        checkRun.setName("run-mbg");
        JLabel info = new JLabel("<html>MS1 glycoform inference.");

        mu.add(p, checkRun).split();
        return p;
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
        params.setGlycanDB(uiTextGlycanDBFile.getNonGhostText());
        return params;
    }

    public void setGlycanDatabase(String path) {
        uiTextGlycanDBFile.setText(path);
    }
}
