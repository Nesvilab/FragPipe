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
import com.dmtavt.fragpipe.tools.enums.ActivationTypes;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.swing.FileChooserUtils;
import com.github.chhh.utils.swing.FormEntry;
import com.github.chhh.utils.swing.JPanelBase;
import com.github.chhh.utils.swing.MigUtils;
import com.github.chhh.utils.swing.UiCheck;
import com.github.chhh.utils.swing.UiCombo;
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
    private static final String PROP_minIsotope = "min_isotope_error";
    private static final String PROP_maxIsotope = "max_isotope_error";
    private static final String PROP_glycoDB = "glyco_db";
    private static final String PROP_reverseOrder = "reverse_scan_order";
    private static final String PROP_singleScanType = "single_scan_type";
    private static final String PROP_activation1 = "activation1";
    private static final String PROP_activation2 = "activation2";

    private UiSpinnerDouble uiSpinnerMS2Tol;
    private UiSpinnerDouble uiSpinnerMS1Tol;
    private UiSpinnerInt uiSpinnerMaxGlycans;
    private UiSpinnerInt uiSpinnerMinIsotope;
    private UiSpinnerInt uiSpinnerMaxIsotope;
    private UiCombo uiComboActivation1;
    private UiCombo uiComboActivation2;
    private UiCheck uiCheckReverseScanOrder;
    private UiCheck uiCheckSingleScanType;

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

    @Override
    protected void init() {
        mu.layout(this, mu.lcFillXNoInsetsTopBottom());
        mu.border(this, "O-glycan Localization with O-Pair");

        pTop = createPanelTop();

        pContent = mu.newPanel(null, mu.lcFillXNoInsetsTopBottom());

        uiSpinnerMS2Tol = UiSpinnerDouble.builder(20.0, 1, 1000.0, 1)
                .setFormat(new DecimalFormat("0.#")).setCols(5).create();
        FormEntry feMS2SpectraTol = new FormEntry(PROP_ms2TolPPM, "Fragment mass tolerance (PPM)",
                uiSpinnerMS2Tol);
        uiSpinnerMS1Tol = UiSpinnerDouble.builder(20.0, 1, 1000.0, 1)
                .setFormat(new DecimalFormat("0.#")).setCols(5).create();
        FormEntry feMS1SpectraTol = new FormEntry(PROP_ms1TolPPM, "Precursor mass tolerance (PPM)",
                uiSpinnerMS1Tol);

        uiSpinnerMaxGlycans = new UiSpinnerInt(2, 1, 10, 1, 1);
        FormEntry feMaxGlycans = new FormEntry(PROP_maxGlycan, "Max Glycans", uiSpinnerMaxGlycans,
                "Maximum number of glycans per peptide. Increasing this value greatly increases search time");

        // To make sure that max isotope >= min isotope
        uiSpinnerMinIsotope = new UiSpinnerInt(0, -10, 0, 1, 1);
        FormEntry feMinIsotope = new FormEntry(PROP_minIsotope, "Min Isotope Error", uiSpinnerMinIsotope,
                "Precursor isotope error range lower bound");
        uiSpinnerMaxIsotope = new UiSpinnerInt(2, 0, 10, 1, 1);
        FormEntry feMaxIsotope = new FormEntry(PROP_maxIsotope, "Max Isotope Error", uiSpinnerMaxIsotope,
                "Precursor isotope error range upper bound");

        uiComboActivation1 = UiUtils.createUiCombo(ActivationTypes.values());
        FormEntry feActivation1 = new FormEntry(PROP_activation1, "First activation type (parent scan)", uiComboActivation1);
        uiComboActivation2 = UiUtils.createUiCombo(ActivationTypes.values());
        FormEntry feActivation2 = new FormEntry(PROP_activation2, "Second activation type (child scan)", uiComboActivation2);

        uiCheckReverseScanOrder = UiUtils.createUiCheck("Reverse paired scan order", false);
        uiCheckReverseScanOrder.setName(PROP_reverseOrder);
        uiCheckReverseScanOrder.setToolTipText("Use if localization scan type (e.g., ETD) comes before search scan type (e.g., HCD)");
        uiCheckSingleScanType = UiUtils.createUiCheck("Single scan type", false);
        uiCheckSingleScanType.setName(PROP_singleScanType);
        uiCheckSingleScanType.setToolTipText("Use if only one scan type (must be hybrid activation)");

        String tooltipGlycanDBFile = "Glycan database file in Byonic or pGlyco formats (.txt or .pdb). Will use internal default O-glycan list if not provided.";
        uiTextOGlycanDBFile = UiUtils.uiTextBuilder().create();
        List<FileFilter> glycFilters = new ArrayList<>();
        FileFilter filter = new FileNameExtensionFilter("Glycan Database file (glyc, txt, csv, tsv, pdb)", "glyc", "txt", "csv", "tsv", "pdb");
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

        mu.add(pContent, feActivation1.label(), mu.ccR());
        mu.add(pContent, feActivation1.comp);
        mu.add(pContent, feActivation2.label(), mu.ccR());
        mu.add(pContent, feActivation2.comp);
        mu.add(pContent, uiCheckReverseScanOrder);
        mu.add(pContent, uiCheckSingleScanType).pushX().wrap();

        mu.add(pContent, feMS2SpectraTol.label(), mu.ccR());
        mu.add(pContent, feMS2SpectraTol.comp);
        mu.add(pContent, feMinIsotope.label(), mu.ccR());
        mu.add(pContent, feMinIsotope.comp);
        mu.add(pContent, feMaxIsotope.label(), mu.ccR());
        mu.add(pContent, feMaxIsotope.comp).wrap();

        mu.add(pContent, feMS1SpectraTol.label(), mu.ccR());
        mu.add(pContent, feMS1SpectraTol.comp);
        mu.add(pContent, feMaxGlycans.label(), mu.ccR());
        mu.add(pContent, feMaxGlycans.comp).wrap();

        mu.add(pContent, feGlycanDBFile.label()).split().spanX();
        mu.add(pContent, btnBrosweGlycanDBFile);
        mu.add(pContent, feGlycanDBFile.comp).wrap();

        mu.add(this, pTop).growX().wrap();
        mu.add(this, pContent).growX().wrap();
    }

    private JPanel createPanelTop() {

        JPanel p = mu.newPanel(null, mu.lcFillXNoInsetsTopBottom());

        checkRun = new UiCheck("Run O-Pair", null, false);
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
        params.setMinIsotope(uiSpinnerMinIsotope.getActualValue());
        params.setMaxIsotope(uiSpinnerMaxIsotope.getActualValue());
        params.setOglycanDB(uiTextOGlycanDBFile.getNonGhostText());
        params.setReverseScanOrder(uiCheckReverseScanOrder.isSelected());
        params.setSingleScanType(uiCheckSingleScanType.isSelected());
        params.setActivation1((String) uiComboActivation1.getSelectedItem());
        params.setActivation2((String) uiComboActivation2.getSelectedItem());
        return params;
    }
}
