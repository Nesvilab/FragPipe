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

package org.nesvilab.fragpipe.tools.ptmshepherd;

import org.nesvilab.fragpipe.Fragpipe;
import org.nesvilab.fragpipe.FragpipeLocations;
import org.nesvilab.fragpipe.tabs.TabGlyco;
import org.nesvilab.utils.MapUtils;
import org.nesvilab.utils.StringUtils;
import org.nesvilab.utils.SwingUtils;
import org.nesvilab.utils.swing.FormEntry;
import org.nesvilab.utils.swing.JPanelBase;
import org.nesvilab.utils.swing.UiCheck;
import org.nesvilab.utils.swing.UiSpinnerDouble;
import org.nesvilab.utils.swing.UiSpinnerInt;
import org.nesvilab.utils.swing.UiText;
import org.nesvilab.utils.swing.UiUtils;

import java.awt.*;
import java.awt.image.BufferedImage;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.DecimalFormat;
import java.util.Map;
import java.util.Objects;
import javax.imageio.ImageIO;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JPanel;
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
    private UiText uiTextLDAfeatures;

    private static final String PROP_run_glyco_mode = "run_glyco_mode";
    private static final String PROP_glycan_fdr = "glyco_fdr";
    private static final String PROP_glyco_mass_error_ppm = "glyco_ppm_tol";
    private static final String PROP_glyco_isotope_error_low = "glyco_isotope_min";
    private static final String PROP_glyco_isotope_error_high = "glyco_isotope_max";
    private static final String PROP_decoy_type = "decoy_type";
    private static final String PROP_glycan_database = "glycodatabase";
    private static final String PROP_remove_glyco_deltamass = "remove_glycan_delta_mass";
    private static final String PROP_print_decoys = "print_decoys";
    private static final String PROP_prob_mass = "prob_mass";
    private static final String PROP_print_extended_params = "print_full_glyco_params";
    private static final String PROP_glyco_lda = "glyco_lda";
    private static final String PROP_glyco_lda_features_text = "glyco_lda_features";
    private static final String PROP_glyco_second_pass = "use_glycan_fragment_probs";
    private static final String PROP_shuffle_decoys = "shuffle_decoy_intensities";

    private static final String PROP_nglyco_mode = "n_glyco";

    public PTMSGlycanAssignPanel() {
        super();
    }

    protected void init() {
        mu.layout(this, mu.lcFillXNoInsetsTopBottom());
        mu.border(this, "Glycan composition assignment and FDR (using PTM-Shepherd)");

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

    public boolean needsIonQuant() {
        return isRun() && uiTextLDAfeatures.getNonGhostText().contains("kl");
    }

    // Get params for PTM-S glycan assignment to pass to PTM-S configure method as additional params
    public Map<String, String> getGlycanAssignParams() {
        Map<String, String> map0 = super.toMap();
        Map<String, String> map1 = MapUtils.remapKeys(map0, s -> StringUtils.stripLeading(s, PREFIX));

        // add glycan residue/mods databases to PTM-Shepherd params
        final Path dirTools = FragpipeLocations.get().getDirTools();
        map1.put("glyco_residue_list", Paths.get(dirTools.toString(), TabGlyco.glycanDBfolder, "glycan_residues.txt").toString());
        map1.put("glyco_mod_list", Paths.get(dirTools.toString(), TabGlyco.glycanDBfolder, "glycan_mods.txt").toString());
        map1.put("glyco_oxonium_list", Paths.get(dirTools.toString(), TabGlyco.glycanDBfolder, "oxonium_ion_list.txt").toString());

        return map1;
    }

    public void setGlycanDatabase(String glycanList) {
        uiTextGlycanDBFile.setText(glycanList);
    }

    private JPanel createpanelGlycanAssignment() {
        pGlycanAssignment = mu.newPanel(null, mu.lcFillXNoInsetsTopBottom());

        // glycan assignment params
        pGlycoAssignContent = mu.newPanel(null, mu.lcFillXNoInsetsTopBottom());
        pGlycoAdvParams = mu.newPanel(null, mu.lcFillXNoInsetsTopBottom());

        checkRunGlycanAssignment = UiUtils.createUiCheck("Assign Glycans with FDR", false);
        checkRunGlycanAssignment.setName(PROP_run_glyco_mode);
        checkRunGlycanAssignment.setToolTipText("NOTE: requires PTM-Shepherd! Check the 'Run PTM-Shepherd' box on the PTMs tab to enable this section. Perform glycan assignment and glycan FDR on PSMs reported with a delta mass");
        uiCheckGlycoAdvParams = UiUtils.createUiCheck("Edit advanced parameters", false);
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

        FormEntry feGlycanIsotopesLow = new FormEntry(PROP_glyco_isotope_error_low, "Isotope error range min:",
                new UiSpinnerInt(-1, -2, 0, 1, 3),
                "Lowest isotope error to consider. Allowed isotope errors will go from this value to Isotope error range max (inclusive).");
        FormEntry feGlycanIsotopesHigh = new FormEntry(PROP_glyco_isotope_error_high, "Max:",
                new UiSpinnerInt(3, 0, 4, 1, 3),
                "Highest isotope error to consider. Allowed isotope errors will go from Isotope error range min to this value (inclusive).");

        String tooltipGlycanDBFile = "Custom glycan database. Will use internal default N-glycan list if not provided.";
        uiTextGlycanDBFile = UiUtils.uiTextBuilder().create();
        uiTextGlycanDBFile.setPreferredSize(new Dimension(100, 25));
        FormEntry feGlycanDBFile = mu.feb(PROP_glycan_database, uiTextGlycanDBFile)
                .label("Glycan database").tooltip(tooltipGlycanDBFile).create();

        FormEntry feDecoyType = new FormEntry(PROP_decoy_type, "Decoy type",
                new UiSpinnerInt(1, 0, 3, 1, 1),
                "How to generate decoy glycan intact mass.\n " +
                        "0: Random mass shift within +/- 3 Da\n" +
                        "1: Random mass shift within glycan mass error tolerance, random isotope error (DEFAULT)\n" +
                        "2: Random mass shift within glycan mass error tolerance, no isotope error\n" +
                        "3: exact same mass as target");

        FormEntry fePrintGlycoDecoys = mu.feb(PROP_print_decoys, UiUtils.createUiCheck("Print decoy glycans", false))
                .tooltip("By default, the best target glycan is printed to the PSM table for PSMs assigned to a decoy glycan (with q-value = 1)\n" +
                        "Check this box to instead print the decoy glycan (identified by 'Decoy_[glycan name])")
                .create();
        FormEntry feRemoveGlycoDeltaMass = mu.feb(PROP_remove_glyco_deltamass, UiUtils.createUiCheck("Remove glycan delta mass", false))
                .tooltip("Removes glycan mass from Delta Mass column in PSM table, even for PSMs that do not pass glycan FDR.\n" +
                        "Required for processing by IonQuant and for PSM table integrity, but prevents re-analysis by PTM-Shepherd.")
                .create();
        FormEntry fePrintExtGlycoParams = mu.feb(PROP_print_extended_params, UiUtils.createUiCheck("Print extended params", false))
                .tooltip("Print additional glyco parameter information for debugging.")
                .create();
        UiSpinnerDouble uiSpinnerMassProb = UiSpinnerDouble.builder(0.5, 0.01, 10.0, 0.1)
                .setFormat(new DecimalFormat("0.0#")).setCols(2).create();
        FormEntry feMassProb = new FormEntry(PROP_prob_mass, "Mass score scaling", uiSpinnerMassProb,
                "Empirical scaling factor for scoring mass deviations. Set to 1 for no scaling. Default 0.5");
        FormEntry feNGlycanMode = mu.feb(PROP_nglyco_mode, UiUtils.createUiCheck("N-Glycan mode", false))
                .tooltip("Sets localization to N-X-S/T sequon if enabled and uses default N-glycan database if custom glycan database is not provided\n. " +
                        "If disabled, localization settings are taken from 'Restrict localization to' parameter above\n" +
                        "and O-glycan default database used.")
                .create();

        FormEntry feUseLDA = mu.feb(PROP_glyco_lda, UiUtils.createUiCheck("(BETA) Use LDA", false))
                .create();
        FormEntry feSecondPass = mu.feb(PROP_glyco_second_pass, UiUtils.createUiCheck("2 pass mode", false))
                .create();
        FormEntry feShuffleDecoys = mu.feb(PROP_shuffle_decoys, UiUtils.createUiCheck("Shuffle decoy intensities", false))
                .create();

        uiTextLDAfeatures = UiUtils.uiTextBuilder().create();
        uiTextLDAfeatures.setPreferredSize(new Dimension(50, 25));
        FormEntry feLDAfeatures = mu.feb(PROP_glyco_lda_features_text, uiTextLDAfeatures)
                .label("LDA features").tooltip("Valid values: yscore,oxo,mass,mass2nd,glycanfreq,kl,iso,iso2nd,yprop").create();

        mu.add(pGlycoAssignContent, feGlycanFDR.label()).split(2);
        mu.add(pGlycoAssignContent, feGlycanFDR.comp);

        mu.add(pGlycoAssignContent, feNGlycanMode.comp);
        mu.add(pGlycoAssignContent, feGlycanMassErr.label(), mu.ccR());
        mu.add(pGlycoAssignContent, feGlycanMassErr.comp);
        mu.add(pGlycoAssignContent, feGlycanIsotopesLow.label()).split(5);
        mu.add(pGlycoAssignContent, feGlycanIsotopesLow.comp);
        mu.add(pGlycoAssignContent, feGlycanIsotopesHigh.label());
        mu.add(pGlycoAssignContent, feGlycanIsotopesHigh.comp).wrap();

        mu.add(pGlycoAssignContent, feGlycanDBFile.label(), mu.ccL()).split(3).spanX();
        mu.add(pGlycoAssignContent, feGlycanDBFile.comp).growX().wrap();

        JLabel imageLabel = new JLabel();
        try {
            BufferedImage image = ImageIO.read(Objects.requireNonNull(getClass().getResource("/org/nesvilab/fragpipe/icons/ptm-s_logo.png")));
            imageLabel = new JLabel(new ImageIcon(image));
        } catch (Exception ex) {
            ex.printStackTrace();
        }

        mu.add(pGlycanAssignment, checkRunGlycanAssignment);
        mu.add(pGlycanAssignment, imageLabel, mu.ccR()).wrap();

        mu.add(pGlycanAssignment, pGlycoAssignContent).growX().wrap();

        // advanced params panel
        mu.add(pGlycoAdvParams, feDecoyType.label(), mu.ccL()).split(7);
        mu.add(pGlycoAdvParams, feDecoyType.comp).split();
        mu.add(pGlycoAdvParams, feRemoveGlycoDeltaMass.comp).split();
        mu.add(pGlycoAdvParams, fePrintGlycoDecoys.comp).split();
        mu.add(pGlycoAdvParams, feMassProb.label(), mu.ccR());
        mu.add(pGlycoAdvParams, feMassProb.comp).split();
        mu.add(pGlycoAdvParams, fePrintExtGlycoParams.comp).split().spanX().pushX().wrap();

        mu.add(pGlycoAdvParams, feUseLDA.comp).split();
        mu.add(pGlycoAdvParams, feSecondPass.comp).split();
        mu.add(pGlycoAdvParams, feShuffleDecoys.comp).split();
        mu.add(pGlycoAdvParams, feLDAfeatures.label(), mu.ccL()).split().spanX();
        mu.add(pGlycoAdvParams, feLDAfeatures.comp).growX().wrap();

        mu.add(pGlycanAssignment, uiCheckGlycoAdvParams).split().spanX().wrap();
        mu.add(pGlycanAssignment, pGlycoAdvParams).growX().wrap();

        return pGlycanAssignment;
    }
}
