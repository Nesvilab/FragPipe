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

package com.dmtavt.fragpipe.tools.opair;

import com.dmtavt.fragpipe.Fragpipe;
import com.github.chhh.utils.swing.*;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.swing.JPanelBase;

import java.awt.Component;
import java.awt.ItemSelectable;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;
import javax.swing.*;
import javax.swing.filechooser.FileFilter;
import javax.swing.filechooser.FileNameExtensionFilter;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class OPairPanel extends JPanelBase {
    private static final Logger log = LoggerFactory.getLogger(com.dmtavt.fragpipe.tools.opair.OPairPanel.class);
    private static final String PREFIX = "opair.";
    private JPanel pTop;
    private JPanel pContent;
    private static final MigUtils mu = MigUtils.get();
    private UiCheck checkRun;
    private UiText uiTextOGlycanDBFile;

    private static final String PROP_ms2TolPPM = "ms2_tol";
    private static final String PROP_ms1TolPPM = "ms1_tol";
    private static final String PROP_maxGlycan = "max_glycans";
    private static final String PROP_glycoDB = "glyco_db";

    private UiSpinnerDouble uiSpinnerMS2Tol;
    private UiSpinnerDouble uiSpinnerMS1Tol;
    private UiSpinnerInt uiSpinnerMaxGlycans;

    private OPairParams params;

    public OPairPanel() {
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

    public void setRunStatus(boolean status) {
        checkRun.setSelected(status);
    }

    @Override
    protected void init() {
        mu.layout(this, mu.lcFillXNoInsetsTopBottom());
        mu.border(this, "O-glycan Localization with O-Pair");

        pTop = createPanelTop();

        pContent = mu.newPanel(null, mu.lcFillXNoInsetsTopBottom());

        uiSpinnerMS2Tol = UiSpinnerDouble.builder(20.0, 0.1, 1000.0, 1)
                .setFormat(new DecimalFormat("0.#")).setCols(5).create();
        FormEntry feMS2SpectraTol = new FormEntry(PROP_ms2TolPPM, "Fragment mass tolerance (PPM)",
                uiSpinnerMS2Tol);
        uiSpinnerMS1Tol = UiSpinnerDouble.builder(20.0, 0.1, 1000.0, 1)
                .setFormat(new DecimalFormat("0.#")).setCols(5).create();
        FormEntry feMS1SpectraTol = new FormEntry(PROP_ms1TolPPM, "Fragment mass tolerance (PPM)",
                uiSpinnerMS1Tol);

        uiSpinnerMaxGlycans = new UiSpinnerInt(2, 1, 10, 1, 1);
        FormEntry feMaxGlycans = new FormEntry(PROP_maxGlycan, "Max Glycans", uiSpinnerMaxGlycans,
                "Maximum number of glycans per peptide. Increasing this value greatly increases search time");

        String tooltipGlycanDBFile = "Custom glycan database file (.glyc). Will use internal default O-glycan list if not provided.";
        uiTextOGlycanDBFile = UiUtils.uiTextBuilder().create();
        List<FileFilter> glycFilters = new ArrayList<>();
        FileFilter filter = new FileNameExtensionFilter("Glycan Database file (.glyc, txt, csv, tsv)", "glyc", "txt", "csv", "tsv");
        glycFilters.add(filter);
        FormEntry feGlycanDBFile = mu.feb(PROP_glycoDB, uiTextOGlycanDBFile)
                .label("O-Glycan Database").tooltip(tooltipGlycanDBFile).create();
        JButton btnBrosweGlycanDBFile = feGlycanDBFile.browseButton("Browse", tooltipGlycanDBFile,
                () -> FileChooserUtils.builder("Select custom glycan database file")
                        .approveButton("Select").mode(FileChooserUtils.FcMode.FILES_ONLY).acceptAll(false).multi(false).filters(glycFilters)
                        .paths(Stream.of(Fragpipe.propsVarGet(PROP_glycoDB))).create(),
                paths -> {
                    if (paths != null && !paths.isEmpty()) {
                        String path = paths.get(0).toString();
                        Fragpipe.propsVarSet(PROP_glycoDB, path);
                        uiTextOGlycanDBFile.setText(path);
                    }
                });

        mu.add(pContent, feMS2SpectraTol.label(), mu.ccR());
        mu.add(pContent, feMS2SpectraTol.comp).split().wrap();
        mu.add(pContent, feMS1SpectraTol.label(), mu.ccR());
        mu.add(pContent, feMS1SpectraTol.comp).split().wrap();

        mu.add(pContent, feMaxGlycans.label(), mu.ccR());
        mu.add(pContent, feMaxGlycans.comp).split();
        mu.add(pContent, feGlycanDBFile.label(), mu.ccR());
        mu.add(pContent, btnBrosweGlycanDBFile, mu.ccR()).split();
        mu.add(pContent, feGlycanDBFile.comp).split().growX().spanX().pushX().wrap();

        mu.add(this, pTop).growX().wrap();
        mu.add(this, pContent).growX().wrap();
    }

    private JPanel createPanelTop() {

        JPanel p = mu.newPanel(null, mu.lcFillXNoInsetsTopBottom());

        checkRun = new UiCheck("Run OPair", null, false);
        checkRun.setName("run-opair");
        JLabel info = new JLabel("<html>O-glycan localization with O-Pair. Requires <b>paired scan data</b>.");

        mu.add(p, checkRun).split();
        mu.add(p, info).gapLeft("80px").wrap();
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

    public OPairParams getOPairParams() {
        OPairParams params = new OPairParams();
        params.setProductPPMtol(uiSpinnerMS2Tol.getActualValue());
        params.setPrecursorPPMtol(uiSpinnerMS1Tol.getActualValue());
        params.setMaxNumGlycans(uiSpinnerMaxGlycans.getActualValue());
        params.setOglycanDB(uiTextOGlycanDBFile.getNonGhostText());
        return params;
    }

    public void setParams(OPairParams params) {
        this.params = params;
    }
}
