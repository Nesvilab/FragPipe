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
import org.nesvilab.utils.swing.FileChooserUtils;
import org.nesvilab.utils.swing.FormEntry;
import org.nesvilab.utils.swing.JPanelBase;
import org.nesvilab.utils.swing.UiCheck;
import org.nesvilab.utils.swing.UiRadio;
import org.nesvilab.utils.swing.UiSpinnerDouble;
import org.nesvilab.utils.swing.UiSpinnerInt;
import org.nesvilab.utils.swing.UiText;
import org.nesvilab.utils.swing.UiUtils;

import java.awt.*;
import java.awt.image.BufferedImage;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Stream;
import javax.imageio.ImageIO;
import javax.swing.ButtonGroup;
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
    private JPanel pGlycoTwoPassParams;
    private UiCheck checkRunGlycanAssignment;
    private UiCheck uiCheckGlycoAdvParams;
    private UiCheck checkTwoPassMode;
    private UiCheck checkSkipMS1Scoring;
    private UiRadio radioScoringLDA;
    private UiRadio radioScoringNN;
    private UiCheck checkUseExternalLibrary;
    private UiText uiTextGlycanDBFile;
    private UiText uiTextGlycoLibPath;
    private JButton btnBrowseGlycoLibPath;
    private UiText uiTextAdvancedOptions;

    private static final String PROP_run_glyco_mode = "run_glyco_mode";
    private static final String PROP_glycan_fdr = "glyco_fdr";
    private static final String PROP_glyco_mass_error_ppm = "glyco_ppm_tol";
    private static final String PROP_glyco_isotope_error_low = "glyco_isotope_min";
    private static final String PROP_glyco_isotope_error_high = "glyco_isotope_max";
    private static final String PROP_decoy_type = "decoy_type";
    private static final String PROP_glycan_database = "glycodatabase";
    private static final String PROP_remove_glyco_deltamass = "remove_glycan_delta_mass";
    private static final String PROP_print_decoys = "print_decoys";
    private static final String PROP_print_extended_params = "print_full_glyco_params";
    private static final String PROP_nglyco_mode = "n_glyco";
    private static final String PROP_glyco_two_pass_search = "glyco_two_pass_search";
    private static final String PROP_min_psms_consensus = "min_psms_consensus";
    private static final String PROP_min_y_consensus = "min_y_consensus";
    private static final String PROP_use_external_library = "use_external_library";
    private static final String PROP_glyco_lib_path = "glyco_lib_path";
    private static final String PROP_glyco_update_lib = "glyco_update_lib";
    private static final String PROP_glyco_score_plots = "glyco_score_plots";
    private static final String PROP_skip_ms1_scoring = "glyco_skip_ms1";
    private static final String PROP_glyco_lda_radio = "glyco_lda_radio";   // session state; maps to glyco_LDA in PTM-S config
    private static final String PROP_glyco_nn_radio = "glyco_nn_radio";     // session state; maps to glyco_NN in PTM-S config
    private static final String PROP_advanced_options = "advanced_options";  // UI only; parsed into individual PTM-S params

    public PTMSGlycanAssignPanel() {
        super();
    }

    protected void init() {
        mu.layout(this, mu.lcFillXNoInsetsTopBottom());
        mu.border(this, "Glycan composition assignment and FDR (using PTM-Shepherd)");

        pGlycanAssignment = createpanelGlycanAssignment();
        mu.add(this, pGlycanAssignment).spanX().growX().wrap();

        // Set default glyco library path
        final Path dirTools = FragpipeLocations.get().getDirTools();
        if (dirTools != null) {
            uiTextGlycoLibPath.setText(dirTools.resolve("Glycan_Databases").resolve("base_glyco_lib.glycolib").toString());
        }
    }

    @Override
    protected void initMore() {
        super.initMore();

        // enable/disable the Glycan Assignment sub-area specifically when the glycan assignment box is changed
        SwingUtils.setEnablementUpdater(this, pGlycoAssignContent, checkRunGlycanAssignment);
        SwingUtils.setEnablementUpdater(this, uiCheckGlycoAdvParams, checkRunGlycanAssignment);
        SwingUtils.setEnablementUpdater(this, checkTwoPassMode, checkRunGlycanAssignment);
        // enable/disable 2 pass mode subpanel when the corresponding checkbox is changed
        SwingUtils.setEnablementUpdater(this, pGlycoTwoPassParams, checkTwoPassMode);
        // enable/disable advanced params for glycan assignment when the corresponding checkbox is changed
        SwingUtils.setEnablementUpdater(this, pGlycoAdvParams, uiCheckGlycoAdvParams);
        // enable/disable glyco library path when the external library checkbox is changed
        SwingUtils.setEnablementUpdater(this, uiTextGlycoLibPath, checkUseExternalLibrary);
        SwingUtils.setEnablementUpdater(this, btnBrowseGlycoLibPath, checkUseExternalLibrary);
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
        return !checkSkipMS1Scoring.isSelected();
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

        // Scoring method radio buttons: derive glyco_LDA and glyco_NN from selection
        map1.remove(PROP_glyco_lda_radio);
        map1.remove(PROP_glyco_nn_radio);
        map1.put("glyco_lda", Boolean.toString(radioScoringLDA.isSelected()));
        map1.put("glyco_nn", Boolean.toString(radioScoringNN.isSelected()));

        // Parse free-form advanced options (format: "key=value; key=value; ...")
        map1.remove(PROP_advanced_options);
        String advOpts = uiTextAdvancedOptions.getNonGhostText().trim();
        if (!advOpts.isEmpty()) {
            for (String pair : advOpts.split(";\\s*")) {
                String[] kv = pair.split("=", 2);
                if (kv.length == 2 && !kv[0].trim().isEmpty()) {
                    map1.put(kv[0].trim(), kv[1].trim());
                }
            }
        }

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
        pGlycoTwoPassParams = mu.newPanel("2 pass mode options", mu.lcFillXNoInsetsTopBottom());

        checkRunGlycanAssignment = UiUtils.createUiCheck("Assign Glycan Compositions", false);
        checkRunGlycanAssignment.setName(PROP_run_glyco_mode);
        checkRunGlycanAssignment.setToolTipText("Perform glycan composition assignment and estimate composition-level FDR on PSMs reported with a delta mass.\n " +
                "Replaces delta mass values with assigned glycan compositions for PSMs passing glycan FDR, and removes unassigned PSMs from the output tables.\n");
        uiCheckGlycoAdvParams = UiUtils.createUiCheck("Edit advanced parameters", false);
        uiCheckGlycoAdvParams.setName("adv_params");
        uiCheckGlycoAdvParams.setToolTipText("Enable/disable the advanced parameter options below");

        UiSpinnerDouble uiSpinnerGlycanFDR = UiSpinnerDouble.builder(0.01, 0, 1.0, 0.01)
                .setFormat(new DecimalFormat("0.00#")).setCols(3).create();
        FormEntry feGlycanFDR = new FormEntry(PROP_glycan_fdr, "Glycan FDR", uiSpinnerGlycanFDR,
                "Glycan assignment FDR. Default 0.01 (1%). Set to 1 to disable (report all composition results)\n");

        UiSpinnerDouble uiSpinnerGlycanMassErr = UiSpinnerDouble.builder(20.0, 0.0, 10000.0, 5.0)
                .setFormat(new DecimalFormat("0.#")).setCols(5).create();
        FormEntry feGlycanMassErr = new FormEntry(PROP_glyco_mass_error_ppm, "Glycan mass tolerance (ppm)", uiSpinnerGlycanMassErr,
                "Mass tolerance for finding possible glycan candidates to consider in glycan assignment (ppm).\n");

        FormEntry feGlycanIsotopesLow = new FormEntry(PROP_glyco_isotope_error_low, "Isotope error range min:",
                new UiSpinnerInt(0, -4, 0, 1, 3),
                "Lowest isotope error to consider. Allowed isotope errors will go from this value to Isotope error range max (inclusive).");
        FormEntry feGlycanIsotopesHigh = new FormEntry(PROP_glyco_isotope_error_high, "Max:",
                new UiSpinnerInt(2, 0, 6, 1, 3),
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
        FormEntry feNGlycanMode = mu.feb(PROP_nglyco_mode, UiUtils.createUiCheck("N-Glycan mode", false))
                .tooltip("Sets localization to N-X-S/T sequon if enabled and uses default N-glycan database if custom glycan database is not provided\n. " +
                        "If disabled, localization settings are taken from 'Restrict localization to' parameter above\n" +
                        "and O-glycan default database used.")
                .create();

        // 2 pass mode checkbox and subpanel
        checkTwoPassMode = UiUtils.createUiCheck("2 pass mode", false);
        checkTwoPassMode.setName(PROP_glyco_two_pass_search);
        checkTwoPassMode.setToolTipText("<html>Enable 2-pass glycan assignment.<br>" +
                "In the first pass, high-confidence PSMs are used to build a consensus spectral library of glycan<br>" +
                "fragment ions. In the second pass, this library rescores all PSMs, improving sensitivity and accuracy.</html>");

        FormEntry feMinPSMs = new FormEntry(PROP_min_psms_consensus, "Min PSMs for Library",
                new UiSpinnerInt(10, 1, Integer.MAX_VALUE, 1, 5),
                "<html>Minimum number of PSMs supporting a glycan that must be observed in the first pass<br>" +
                "to include that glycan's fragment ion profile in the spectral library.<br>" +
                "Higher values produce a more reliable library but may exclude rare glycans.</html>");
        FormEntry feMinYIons = new FormEntry(PROP_min_y_consensus, "Min Y ions for Library",
                new UiSpinnerInt(3, 0, Integer.MAX_VALUE, 1, 5),
                "<html>Minimum number of distinct Y (peptide + glycan fragment) ions that must be observed<br>" +
                "for a given PSM in the first pass to include it in the spectral library.<br>" +
                "Higher values require more fragment ion evidence before a PSM is added to the library, increasing library quality.</html>");

        checkSkipMS1Scoring = UiUtils.createUiCheck("Skip MS1 scoring", false);
        checkSkipMS1Scoring.setName(PROP_skip_ms1_scoring);
        checkSkipMS1Scoring.setToolTipText("<html>Skip the MS1 (precursor) intensity scoring component of glycan assignment.<br>" +
                "MS1 scoring requires IonQuant to extract precursor intensities.<br>" +
                "Check this box to run glycan assignment without IonQuant.<br>" +
                "When unchecked, IonQuant must be available and will be added to the PTM-Shepherd classpath.</html>");

        // Scoring method radio buttons (LDA vs NN)
        radioScoringLDA = new UiRadio("LDA", null, false);
        radioScoringLDA.setName(PROP_glyco_lda_radio);
        radioScoringLDA.setToolTipText("<html>Use Linear Discriminant Analysis (LDA) to separate target and decoy glycan<br>" +
                "composition assignments. LDA is a fast, simple approach that works well for<br>" +
                "datasets of all sizes, but may be outperformed by NN in larger datasets.</html>");
        radioScoringNN = new UiRadio("NN", null, true);
        radioScoringNN.setName(PROP_glyco_nn_radio);
        radioScoringNN.setToolTipText("<html>Use a Neural Network (NN) to separate target and decoy glycan composition assignments.<br>" +
                "NN scoring can capture non-linear relationships between features and may improve<br>" +
                "accuracy in large datasets, but requires more data to train effectively so may not be appropriate in small datasets.</html>");
        ButtonGroup scoringGroup = new ButtonGroup();
        scoringGroup.add(radioScoringLDA);
        scoringGroup.add(radioScoringNN);

        checkUseExternalLibrary = UiUtils.createUiCheck("(beta) Use external library", false);
        checkUseExternalLibrary.setName(PROP_use_external_library);
        checkUseExternalLibrary.setToolTipText("<html>Use a pre-built glyco spectral library file instead of building one from first-pass results.<br>" +
                "The library specifies expected fragment ion patterns for each glycan,<br>" +
                "which are used to rescore PSMs in the second pass.</html>");

        uiTextGlycoLibPath = UiUtils.uiTextBuilder().create();
        uiTextGlycoLibPath.setPreferredSize(new Dimension(100, 25));
        String tooltipGlycoLibPath = "Path to an existing glyco spectral library file (.glycolib).\n" +
                "Only used when 'Use external library' is checked.\n" +
                "Defaults to the bundled base library in the Glycan_Databases folder.";
//        uiTextGlycoLibPath.setGhostText("Path to an existing glyco spectral library file (.glycolib).");
        FormEntry feGlycoLibPath = mu.feb(PROP_glyco_lib_path, uiTextGlycoLibPath)
                .label("Glyco Library Path")
                .tooltip(tooltipGlycoLibPath)
                .create();

        List<FileFilter> glycoLibFilters = new ArrayList<>();
        glycoLibFilters.add(new FileNameExtensionFilter("Glyco library file (glycolib)", "glycolib"));
        btnBrowseGlycoLibPath = feGlycoLibPath.browseButton("Browse", null,
                () -> FileChooserUtils.builder("Select glyco library file")
                        .approveButton("Select").mode(FileChooserUtils.FcMode.FILES_ONLY).acceptAll(false).multi(false).filters(glycoLibFilters)
                        .paths(Stream.of(FragpipeLocations.get().getDirTools() != null
                                ? FragpipeLocations.get().getDirTools().resolve("Glycan_Databases").toString()
                                : uiTextGlycoLibPath.getNonGhostText())).create(),
                paths -> {
                    if (paths != null && !paths.isEmpty()) {
                        uiTextGlycoLibPath.setText(paths.get(0).toString());
                    }
                });

        FormEntry feUpdateLib = mu.feb(PROP_glyco_update_lib, UiUtils.createUiCheck("Update library with results", false))
                .tooltip("<html>After the second pass, update the glyco spectral library with fragment ion<br>" +
                        "patterns observed in the current dataset.<br>" +
                        "This allows the library to accumulate evidence across multiple analyses.</html>")
                .create();

        // Advanced Options free-form text field
        uiTextAdvancedOptions = UiUtils.uiTextBuilder().ghost("param_name=value; param_name2=value2").create();
        uiTextAdvancedOptions.setPreferredSize(new Dimension(100, 25));
        uiTextAdvancedOptions.setName(PROP_advanced_options);
        uiTextAdvancedOptions.setToolTipText("<html>Specify additional PTM-Shepherd glycan assignment parameters not exposed in the UI.<br>" +
                "Format: <b>param_name=value; param_name2=value2</b> (semicolon-separated, spaces after semicolon are allowed).<br>" +
                "Parameter names and values are written directly to the PTM-Shepherd config file.<br>" +
                "Values set here will override any corresponding UI controls above.</html>");

        // Advanced params
        FormEntry feGenerateDebugPlots = mu.feb(PROP_glyco_score_plots, UiUtils.createUiCheck("Generate debug plots", false))
                .tooltip("<html>Generate diagnostic score distribution plots for glycan assignment.<br>" +
                        "Useful for troubleshooting assignment quality or tuning scoring parameters.</html>")
                .create();

        // Layout: pGlycoAssignContent
        mu.add(pGlycoAssignContent, feGlycanFDR.label()).split(2);
        mu.add(pGlycoAssignContent, feGlycanFDR.comp);

        mu.add(pGlycoAssignContent, feNGlycanMode.comp);
        mu.add(pGlycoAssignContent, feGlycanMassErr.label(), mu.ccR());
        mu.add(pGlycoAssignContent, feGlycanMassErr.comp);
        mu.add(pGlycoAssignContent, feGlycanIsotopesLow.label()).split(6);
        mu.add(pGlycoAssignContent, feGlycanIsotopesLow.comp);
        mu.add(pGlycoAssignContent, feGlycanIsotopesHigh.label());
        mu.add(pGlycoAssignContent, feGlycanIsotopesHigh.comp);
        mu.add(pGlycoAssignContent, checkSkipMS1Scoring).wrap();

        mu.add(pGlycoAssignContent, feGlycanDBFile.label(), mu.ccL()).split(3).spanX();
        mu.add(pGlycoAssignContent, feGlycanDBFile.comp).growX().wrap();

        // Layout: pGlycoTwoPassParams
        mu.add(pGlycoTwoPassParams, feMinPSMs.label()).split(4);
        mu.add(pGlycoTwoPassParams, feMinPSMs.comp);
        mu.add(pGlycoTwoPassParams, feMinYIons.label());
        mu.add(pGlycoTwoPassParams, feMinYIons.comp).wrap();

        mu.add(pGlycoTwoPassParams, new JLabel("Scoring:")).split(3);
        mu.add(pGlycoTwoPassParams, radioScoringLDA);
        mu.add(pGlycoTwoPassParams, radioScoringNN).wrap();

        mu.add(pGlycoTwoPassParams, checkUseExternalLibrary).split(6);
        mu.add(pGlycoTwoPassParams, feUpdateLib.comp);
        mu.add(pGlycoTwoPassParams, feGlycoLibPath.label());
        mu.add(pGlycoTwoPassParams, btnBrowseGlycoLibPath);
        mu.add(pGlycoTwoPassParams, feGlycoLibPath.comp).growX().wrap();

        // Layout: pGlycoAdvParams
        mu.add(pGlycoAdvParams, feDecoyType.label(), mu.ccL()).split(6);
        mu.add(pGlycoAdvParams, feDecoyType.comp).split();
        mu.add(pGlycoAdvParams, feRemoveGlycoDeltaMass.comp).split();
        mu.add(pGlycoAdvParams, fePrintGlycoDecoys.comp).split();
        mu.add(pGlycoAdvParams, fePrintExtGlycoParams.comp).split();
        mu.add(pGlycoAdvParams, feGenerateDebugPlots.comp).split().spanX().pushX().wrap();

        mu.add(pGlycoAdvParams, new JLabel("Advanced Options:")).split(2);
        mu.add(pGlycoAdvParams, uiTextAdvancedOptions).growX().wrap();

        // Layout: pGlycanAssignment
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

        mu.add(pGlycanAssignment, checkTwoPassMode).split().spanX().wrap();
        mu.add(pGlycanAssignment, pGlycoTwoPassParams).growX().wrap();

        mu.add(pGlycanAssignment, uiCheckGlycoAdvParams).split().spanX().wrap();
        mu.add(pGlycanAssignment, pGlycoAdvParams).growX().wrap();

        return pGlycanAssignment;
    }
}
