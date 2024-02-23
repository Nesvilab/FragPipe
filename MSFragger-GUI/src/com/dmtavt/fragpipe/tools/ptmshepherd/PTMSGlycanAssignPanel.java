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

package com.dmtavt.fragpipe.tools.ptmshepherd;

import com.dmtavt.fragpipe.Fragpipe;
import com.github.chhh.utils.MapUtils;
import com.github.chhh.utils.StringUtils;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.swing.FileChooserUtils;
import com.github.chhh.utils.swing.FormEntry;
import com.github.chhh.utils.swing.JPanelBase;
import com.github.chhh.utils.swing.UiCheck;
import com.github.chhh.utils.swing.UiSpinnerDouble;
import com.github.chhh.utils.swing.UiSpinnerInt;
import com.github.chhh.utils.swing.UiText;
import com.github.chhh.utils.swing.UiUtils;
import java.awt.Component;
import java.awt.ItemSelectable;
import java.awt.image.BufferedImage;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Stream;
import javax.imageio.ImageIO;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.filechooser.FileFilter;
import javax.swing.filechooser.FileNameExtensionFilter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class PTMSGlycanAssignPanel extends JPanelBase {
    public static final String PREFIX = "ptmshepherd.";
    private static final Logger log = LoggerFactory.getLogger(PTMSGlycanAssignPanel.class);

    private JPanel pGlycanAssignment;
    private JPanel pGlycoAssignContent;
    private JPanel pGlycoAdvParams;
    private UiCheck checkRunGlycanAssignment;
    private UiCheck uiCheckGlycoAdvParams;
    private UiText uiTextGlycanDBFile;

    private static final String PROP_run_glyco_mode = "run_glyco_mode";
    private static final String PROP_glycan_fdr = "glyco_fdr";
    private static final String PROP_glyco_mass_error_ppm = "glyco_ppm_tol";
    private static final String PROP_glyco_isotope_error_low = "glyco_isotope_min";
    private static final String PROP_glyco_isotope_error_high = "glyco_isotope_max";
    private static final String PROP_adduct_names = "glyco_adducts";
    private static final String PROP_max_adducts = "max_adducts";
    private static final String PROP_neuac_probs = "prob_neuacOx";
    private static final String PROP_neugc_probs = "prob_neugcOx";
    private static final String PROP_fucOx_probs = "prob_dhexOx";
    private static final String PROP_phospho_probs = "prob_phosphoOx";
    private static final String PROP_sulfo_probs = "prob_sulfoOx";
    private static final String PROP_regY_probs = "prob_regY";
    private static final String PROP_fucY_probs = "prob_dhexY";
    private static final String PROP_decoy_type = "decoy_type";
    private static final String PROP_glycan_database = "glycodatabase";
    private static final String PROP_remove_glyco_deltamass = "remove_glycan_delta_mass";
    private static final String PROP_print_decoys = "print_decoys";
    private static final String PROP_nglyco_mode = "n_glyco";

    public PTMSGlycanAssignPanel() {
        super();
    }

    protected void init() {
        mu.layout(this, mu.lcFillXNoInsetsTopBottom());
        mu.border(this, "Glycan Composition Assignment and FDR (using PTM-Shepherd)");

        pGlycanAssignment = createpanelGlycanAssignment();
        mu.add(this, pGlycanAssignment).spanX().growX().wrap();
    }

    @Override
    protected void initMore() {
        super.initMore();

        // enable/disable the Glycan Assignment sub-area specifically when the glycan assignment box is changed
        SwingUtils.setEnablementUpdater(this, pGlycoAssignContent, checkRunGlycanAssignment);
        SwingUtils.setEnablementUpdater(this, uiCheckGlycoAdvParams, checkRunGlycanAssignment);
        // enable/disable advanced params for glycan assignment when the corresponding checkbox is changed
        SwingUtils.setEnablementUpdater(this, pGlycoAdvParams, uiCheckGlycoAdvParams);
    }

    @Override
    protected ItemSelectable getRunCheckbox() {
        return checkRunGlycanAssignment;
    }

    @Override
    protected Component getEnablementToggleComponent() {
        return pGlycoAssignContent;
    }

    @Override
    protected String getComponentNamePrefix() {
        return PREFIX;
    }

    @Override
    public boolean isRun() {
        return SwingUtils.isEnabledAndChecked(checkRunGlycanAssignment);
    }

    // Get params for PTM-S glycan assignment to pass to PTM-S configure method as additional params
    public Map<String, String> getGlycanAssignParams() {
        Map<String, String> map0 = super.toMap();
        Map<String, String> map1 = MapUtils.remapKeys(map0, s -> StringUtils.stripLeading(s, PREFIX));
        return map1;
    }

    public void setGlycanDatabase(String path) {
        uiTextGlycanDBFile.setText(path);
    }

    private JPanel createpanelGlycanAssignment() {
        pGlycanAssignment = mu.newPanel(null, mu.lcFillXNoInsetsTopBottom());

        // glycan assignment params
        pGlycoAssignContent = mu.newPanel(null, mu.lcFillXNoInsetsTopBottom());
        pGlycoAdvParams = mu.newPanel(null, mu.lcFillXNoInsetsTopBottom());

        checkRunGlycanAssignment = UiUtils.createUiCheck("Assign Glycans with FDR", false);
        checkRunGlycanAssignment.setName(PROP_run_glyco_mode);
        checkRunGlycanAssignment.setToolTipText("NOTE: requires PTM-Shepherd! Check the 'Run PTM-Shepherd' box on the PTMs tab to enable this section. Perform glycan assignment and glycan FDR on PSMs reported with a delta mass");
        uiCheckGlycoAdvParams = UiUtils.createUiCheck("Edit Advanced Parameters", false);
        uiCheckGlycoAdvParams.setName("adv_params");
        uiCheckGlycoAdvParams.setToolTipText("Enable/disable the advanced parameter options below");

        UiSpinnerDouble uiSpinnerGlycanFDR = UiSpinnerDouble.builder(0.01, 0, 1.0, 0.01)
                .setFormat(new DecimalFormat("0.00#")).setCols(3).create();
        FormEntry feGlycanFDR = new FormEntry(PROP_glycan_fdr, "Glycan FDR", uiSpinnerGlycanFDR,
                "Glycan assignment FDR. Default 0.01 (1%)\n");

        UiSpinnerDouble uiSpinnerGlycanMassErr = UiSpinnerDouble.builder(50.0, 0.0, 10000.0, 5.0)
                .setFormat(new DecimalFormat("0.#")).setCols(5).create();
        FormEntry feGlycanMassErr = new FormEntry(PROP_glyco_mass_error_ppm, "Glycan mass tolerance (ppm)", uiSpinnerGlycanMassErr,
                "Mass tolerance for finding possible glycan candidates to consider in glycan assignment (ppm).\n");

        FormEntry feGlycanIsotopesLow = new FormEntry(PROP_glyco_isotope_error_low, "Isotope Error Range Min:",
                new UiSpinnerInt(-1, -2, 0, 1, 3),
                "Lowest isotope error to consider. Allowed isotope errors will go from this value to Isotope Error Range Max (inclusive).");
        FormEntry feGlycanIsotopesHigh = new FormEntry(PROP_glyco_isotope_error_high, "Max:",
                new UiSpinnerInt(3, 0, 4, 1, 3),
                "Highest isotope error to consider. Allowed isotope errors will go from Isotope Error Range Min to this value (inclusive).");

        FormEntry feAdductNames = mu.feb(PROP_adduct_names, UiUtils.uiTextBuilder().create())
                .label("Adduct Type(s)")
                .tooltip("Added to possible glycan compositions as noncovalent adducts. "
                        + "Space, comma, or slash separated values accepted. " +
                        "Possible values: NH3, Na, Fe3, Fe2, Al, Ca").create();
        FormEntry feMaxAdducts = new FormEntry(PROP_max_adducts, "Max Adducts",
                new UiSpinnerInt(0, 0, 5, 1, 1),
                "Maximum number of each specified adduct to allow");

        String tooltipGlycanDBFile = "Custom glycan database file (.glyc). Will use internal default N-glycan list if not provided.";
        uiTextGlycanDBFile = UiUtils.uiTextBuilder().create();
        List<FileFilter> glycFilters = new ArrayList<>();
        FileFilter filter = new FileNameExtensionFilter("Glycan Database file (.glyc, txt, csv, tsv)", "glyc", "txt", "csv", "tsv");
        glycFilters.add(filter);
        FormEntry feGlycanDBFile = mu.feb(PROP_glycan_database, uiTextGlycanDBFile)
                .label("Custom Glycan Database").tooltip(tooltipGlycanDBFile).create();
        JButton btnBrosweGlycanDBFile = feGlycanDBFile.browseButton("Browse", tooltipGlycanDBFile,
                () -> FileChooserUtils.builder("Select custom glycan database file")
                        .approveButton("Select").mode(FileChooserUtils.FcMode.FILES_ONLY).acceptAll(false).multi(false).filters(glycFilters)
                        .paths(Stream.of(Fragpipe.propsVarGet(PROP_glycan_database))).create(),
                paths -> {
                    if (paths != null && !paths.isEmpty()) {
                        String path = paths.get(0).toString();
                        Fragpipe.propsVarSet(PROP_glycan_database, path);
                        uiTextGlycanDBFile.setText(path);
                    }
                });

        UiText neuacProbs = UiUtils.uiTextBuilder().create();
        neuacProbs.setColumns(7);
        FormEntry feNeuAcProbs = mu.feb(PROP_neuac_probs, neuacProbs)
                .label("NeuAc Oxonium Ratios")
                .tooltip("Likelihood ratios for NeuAc oxonium ions. Hit ratio, miss ratio, expected intensity, separated by commas. " +
                        "Default 2,0.05,0.2").create();
        UiText neugcProbs = UiUtils.uiTextBuilder().create();
        neugcProbs.setColumns(7);
        FormEntry feNeuGcProbs = mu.feb(PROP_neugc_probs, neugcProbs)
                .label("NeuGc Oxonium Ratios")
                .tooltip("Likelihood ratios for NeuGc oxonium ions. Hit ratio, miss ratio, expected intensity, separated by commas. " +
                        "Default 2,0.05,0.2").create();
        UiText fucProbs = UiUtils.uiTextBuilder().create();
        fucProbs.setColumns(7);
        FormEntry feFucOxProbs = mu.feb(PROP_fucOx_probs, fucProbs)
                .label("Fucose Oxonium Ratios")
                .tooltip("Likelihood ratios for Fucose oxonium ions. Hit ratio, miss ratio, expected intensity, separated by commas. " +
                        "Default 2,0.5,0.1").create();
        UiText phosphoProbs = UiUtils.uiTextBuilder().create();
        phosphoProbs.setColumns(7);
        FormEntry fePhosphoProbs = mu.feb(PROP_phospho_probs, phosphoProbs)
                .label("Phospho Oxonium Ratios")
                .tooltip("Likelihood ratios for Phospho oxonium ions. Hit ratio, miss ratio, expected intensity, separated by commas. " +
                        "Default 2,0.05,0.2").create();
        UiText sulfoProbs = UiUtils.uiTextBuilder().create();
        sulfoProbs.setColumns(7);
        FormEntry feSulfoProbs = mu.feb(PROP_sulfo_probs, sulfoProbs)
                .label("Sulfo Oxonium Ratios")
                .tooltip("Likelihood ratios for Sulfo oxonium ions. Hit ratio, miss ratio, expected intensity, separated by commas. " +
                        "Default 2,0.1,0.1").create();
        UiText regYprobs = UiUtils.uiTextBuilder().create();
        regYprobs.setColumns(4);
        FormEntry feRegYProbs = mu.feb(PROP_regY_probs, regYprobs)
                .label("Y-ion Ratios")
                .tooltip("Likelihood ratios for Y-ions not containing Fucose. Hit ratio, miss ratio, separated by commas. " +
                        "Default 5,0.5").create();
        UiText fucYprobs = UiUtils.uiTextBuilder().create();
        fucYprobs.setColumns(4);
        FormEntry feFucYProbs = mu.feb(PROP_fucY_probs, fucYprobs)
                .label("Fucose Y-ion Ratios")
                .tooltip("Likelihood ratios for for Y-ions containing Fucose. Hit ratio, miss ratio, separated by commas. " +
                        "Default 2,0.5").create();
        FormEntry feDecoyType = new FormEntry(PROP_decoy_type, "Decoy Type",
                new UiSpinnerInt(1, 0, 3, 1, 1),
                "How to generate decoy glycan intact mass.\n " +
                        "0: Random mass shift within +/- 3 Da\n" +
                        "1: Random mass shift within glycan mass error tolerance, random isotope error (DEFAULT)\n" +
                        "2: Random mass shift within glycan mass error tolerance, no isotope error\n" +
                        "3: exact same mass as target");

        FormEntry fePrintGlycoDecoys = mu.feb(PROP_print_decoys, UiUtils.createUiCheck("Print Decoy Glycans", false))
                .tooltip("By default, the best target glycan is printed to the PSM table for PSMs assigned to a decoy glycan (with q-value = 1)\n" +
                        "Check this box to instead print the decoy glycan (identified by 'Decoy_[glycan name])")
                .create();
        FormEntry feRemoveGlycoDeltaMass = mu.feb(PROP_remove_glyco_deltamass, UiUtils.createUiCheck("Remove Glycan Delta Mass", false))
                .tooltip("Removes glycan mass from Delta Mass column in PSM table, even for PSMs that do not pass glycan FDR.\n" +
                        "Required for processing by IonQuant and for PSM table integrity, but prevents re-analysis by PTM-Shepherd.")
                .create();
        FormEntry feNGlycanMode = mu.feb(PROP_nglyco_mode, UiUtils.createUiCheck("N-Glycan Mode", false))
                .tooltip("Sets localization to N-X-S/T sequon if enabled and uses default N-glycan database if custom glycan database is not provided\n. " +
                        "If disabled, localization settings are taken from 'Restrict localization to' parameter above\n" +
                        "and O-glycan default database used.")
                .create();

        mu.add(pGlycoAssignContent, feGlycanFDR.label()).split(2);
        mu.add(pGlycoAssignContent, feGlycanFDR.comp);

        mu.add(pGlycoAssignContent, feNGlycanMode.comp);
        mu.add(pGlycoAssignContent, feGlycanIsotopesLow.label()).split(4);
        mu.add(pGlycoAssignContent, feGlycanIsotopesLow.comp);
        mu.add(pGlycoAssignContent, feGlycanIsotopesHigh.label());
        mu.add(pGlycoAssignContent, feGlycanIsotopesHigh.comp).wrap();

        mu.add(pGlycoAssignContent, feGlycanMassErr.label()).split(2);
        mu.add(pGlycoAssignContent, feGlycanMassErr.comp);
        mu.add(pGlycoAssignContent, feMaxAdducts.label()).split(2);
        mu.add(pGlycoAssignContent, feMaxAdducts.comp);
        mu.add(pGlycoAssignContent, feAdductNames.label()).split();
        mu.add(pGlycoAssignContent, feAdductNames.comp).growX().wrap();

        mu.add(pGlycoAssignContent, feGlycanDBFile.label(), mu.ccL()).split(3).spanX();
        mu.add(pGlycoAssignContent, btnBrosweGlycanDBFile, mu.ccL());
        mu.add(pGlycoAssignContent, feGlycanDBFile.comp).growX().wrap();

        JLabel imageLabel = new JLabel();
        try {
            BufferedImage image = ImageIO.read(Objects.requireNonNull(getClass().getResource("/com/dmtavt/fragpipe/icons/ptm-s_logo.png")));
            imageLabel = new JLabel(new ImageIcon(image));
        } catch (Exception ex) {
            ex.printStackTrace();
        }

        mu.add(pGlycanAssignment, checkRunGlycanAssignment);
        mu.add(pGlycanAssignment, imageLabel, mu.ccR()).wrap();

        mu.add(pGlycanAssignment, pGlycoAssignContent).growX().wrap();

        // advanced params panel
        mu.add(pGlycoAdvParams, feNeuAcProbs.label(), mu.ccR());
        mu.add(pGlycoAdvParams, feNeuAcProbs.comp).split();
        mu.add(pGlycoAdvParams, feNeuGcProbs.label(), mu.ccR());
        mu.add(pGlycoAdvParams, feNeuGcProbs.comp).split();
        mu.add(pGlycoAdvParams, feFucOxProbs.label(), mu.ccR());
        mu.add(pGlycoAdvParams, feFucOxProbs.comp).split();
        mu.add(pGlycoAdvParams, fePhosphoProbs.label(), mu.ccR());
        mu.add(pGlycoAdvParams, fePhosphoProbs.comp).split();
        mu.add(pGlycoAdvParams, feSulfoProbs.label(), mu.ccR());
        mu.add(pGlycoAdvParams, feSulfoProbs.comp).split().spanX().pushX().wrap();

        mu.add(pGlycoAdvParams, feRegYProbs.label(), mu.ccR());
        mu.add(pGlycoAdvParams, feRegYProbs.comp).split();
        mu.add(pGlycoAdvParams, feFucYProbs.label(), mu.ccR());
        mu.add(pGlycoAdvParams, feFucYProbs.comp).split();
        mu.add(pGlycoAdvParams, feDecoyType.label(), mu.ccR());
        mu.add(pGlycoAdvParams, feDecoyType.comp).split();
        mu.add(pGlycoAdvParams, feRemoveGlycoDeltaMass.comp).split();
        mu.add(pGlycoAdvParams, fePrintGlycoDecoys.comp).split().growX().spanX().pushX().wrap();

        mu.add(pGlycanAssignment, uiCheckGlycoAdvParams).split().spanX().wrap();
        mu.add(pGlycanAssignment, pGlycoAdvParams).growX().wrap();

        return pGlycanAssignment;
    }
}
