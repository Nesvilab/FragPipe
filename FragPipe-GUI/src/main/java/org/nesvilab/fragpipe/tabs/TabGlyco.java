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

package org.nesvilab.fragpipe.tabs;

import static org.nesvilab.fragpipe.Version.PROGRAM_TITLE;
import static org.nesvilab.fragpipe.tabs.TabWorkflow.getSaveFilePath;
import static org.nesvilab.fragpipe.tools.glyco.GlycoMassLoader.PROP_FILECHOOSER_LAST_PATH;

import org.nesvilab.fragpipe.Fragpipe;
import org.nesvilab.fragpipe.FragpipeLocations;
import org.nesvilab.fragpipe.dialogs.GlycanModEditDialog;
import org.nesvilab.fragpipe.dialogs.GlycanResidueEditDialog;
import org.nesvilab.fragpipe.tools.fragger.MsfraggerParams;
import org.nesvilab.fragpipe.tools.mbg.MBGPanel;
import org.nesvilab.utils.swing.*;
import umich.ms.glyco.*;
import org.nesvilab.fragpipe.tools.opair.OPairPanel;
import org.nesvilab.fragpipe.tools.ptmshepherd.PTMSGlycanAssignPanel;
import org.nesvilab.fragpipe.tools.glyco.GlycoMassLoader;
import org.nesvilab.utils.StringUtils;
import org.nesvilab.utils.SwingUtils;
import org.nesvilab.fragpipe.api.Bus;
import org.jooq.lambda.Unchecked;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.*;
import javax.swing.table.TableCellEditor;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.*;
import java.util.List;
import java.util.function.Predicate;


public class TabGlyco extends JPanelWithEnablement {
    private static MigUtils mu = MigUtils.get();
    private OPairPanel panelOPair;
    private PTMSGlycanAssignPanel panelGlycanAssign;
    private JPanel panelLoadGlycans;
    private MBGPanel panelMBG;
    private static final Logger log = LoggerFactory.getLogger(TabGlyco.class);
    private UiText textLoadGlycans;
    private UiCombo uiComboLoadBuiltinGlycans;
    private LinkedHashMap<String, File> glycanDBs;
    public static final String glycanDBfolder = "Glycan_Databases";
    public static final String GLYCAN_RESIDUE_HEADER = "#Name\tMass\tyProb+\tyProb-\tAlternate Names\tElemental Composition";
    public static final String GLYCAN_MOD_HEADER = "#Name\tMass\tyProb+\tyProb-\tAlternate Names\tElemental Composition\tRequired Residue(s)\tIntrinsic Charge";
    public GlycoMassLoader glycanDBloader;
    private Path glycoDBpath;

    public TabGlyco() {
        init();
        initMore();
    }

    private void init() {
        mu.layout(this).fillX();

        final Path dirTools = FragpipeLocations.get().getDirTools();
        glycoDBpath = Paths.get(dirTools.toString(), glycanDBfolder);

        glycanDBloader = new GlycoMassLoader(false);

        panelLoadGlycans = createPanelLoadGlycans();
        panelGlycanAssign = new PTMSGlycanAssignPanel();
        panelOPair = new OPairPanel();
        panelMBG = new MBGPanel(glycanDBloader);

        mu.add(this, panelLoadGlycans).spanX().growX().wrap();
        mu.add(this, panelGlycanAssign).spanX().growX().wrap();
        mu.add(this, panelOPair).spanX().growX().wrap();
        mu.add(this, panelMBG).spanX().growX().wrap();
    }

    private void initMore() {
        Bus.registerQuietly(this);
        Bus.postSticky(this);
    }

    private JPanel createPanelLoadGlycans() {
        JPanel p = mu.newPanel("Glycan database options", true);

        JLabel jLabelLoadGlycanDB = new JLabel("Select Glycan database to load:");
        try {
            glycanDBs = getGlycanDBs();
        } catch (IOException ex) {
            glycanDBs = new LinkedHashMap<>();
            log.error("Failed to load internal glycan databases list on Glyco tab\n" + Arrays.toString(ex.getStackTrace()));
        }
        List<String> glycanDBnames = new ArrayList<>(glycanDBs.keySet());
        glycanDBnames.add(0, "Custom");     // for custom DB loading
        uiComboLoadBuiltinGlycans = UiUtils.createUiCombo(glycanDBnames);
        uiComboLoadBuiltinGlycans.addActionListener(this::actionBtnLoadGlycans);
        textLoadGlycans = new UiText();

        JButton btnSaveGlycanDB = new JButton("Save Glycan Database");
        btnSaveGlycanDB.addActionListener(this::actionBtnSaveGlycanDB);
        btnSaveGlycanDB.setToolTipText("Save the currently loaded glycan database to a file (.tsv)");

        JButton btnEditGlycanResiduesTable = new JButton("Edit Glycan residue definitions");
        btnEditGlycanResiduesTable.addActionListener(this::actionBtnEditGlycanResidues);
        btnEditGlycanResiduesTable.setToolTipText("Edit the internal glycan residue definitions used by all tools.");

        JButton btnEditGlycanModsTable = new JButton("Edit Glycan modification definitions");
        btnEditGlycanModsTable.addActionListener(this::actionBtnEditGlycanMods);
        btnEditGlycanModsTable.setToolTipText("Edit the internal glycan modification definitions. The defined mods will be available when loading a glycan database");

        JButton btnOpenInExplorer = SwingUtils.createButtonOpenInFileManager(this, "Open in file manager", () -> FragpipeLocations.get().getDirTools().resolve("Glycan_Databases"));
        btnOpenInExplorer.setToolTipText("Opens the file location with the internal 'glycan_residues.txt' and 'glycan_mods.txt' \nfiles, which " +
                "control the supported monosaccharides and glycan modifications, respectively, for " + PROGRAM_TITLE + " and all glyco tools.\n" +
                "For details on how to edit these files, please see the glyco tutorial pages at fragpipe.nesvilab.org.");

        mu.add(p, jLabelLoadGlycanDB).split();
        mu.add(p, uiComboLoadBuiltinGlycans).split().wrap();
        mu.add(p, textLoadGlycans).spanX().growX().wrap();
        mu.add(p, btnSaveGlycanDB).split();
        mu.add(p, btnEditGlycanResiduesTable).split();
        mu.add(p, btnEditGlycanModsTable).split();
        mu.add(p, btnOpenInExplorer).wrap();
        return p;
    }

    /**
     * Load a glycan database from the internal list or custom file, then trigger the follow-up method
     * to save the database to various places.
     * @param actionEvent
     */
    private void actionBtnLoadGlycans(ActionEvent actionEvent) {
        if (uiComboLoadBuiltinGlycans.getSelectedItem().toString().equals("Custom")) {
            // load a custom (user-defined) glycan database
            glycanDBloader.loadCustomGlycans(this);
        } else {
            // load the built-in glycan database specified by the UiCombo
            String name = (String) uiComboLoadBuiltinGlycans.getSelectedItem();
            File selectedDB = glycanDBs.get(name);
            if (selectedDB != null) {
                glycanDBloader.loadGlycansFromDatabase(this, selectedDB);
            } else {
                log.info("No database selected, canceling load glycan masses");
                return;
            }
        }
        loadGlycansFollowup();
    }

    private void actionBtnSaveGlycanDB(ActionEvent actionEvent) {
        FileNameEndingFilter filter = new FileNameEndingFilter("Glycan Database (.tsv)", "tsv");

        Path savePath = getSaveFilePath(null, PROP_FILECHOOSER_LAST_PATH, filter, ".tsv", false, this);
        if (savePath == null) {
            return;     // user canceled action
        }
        glycanDBloader.saveGlycanDBtoFile(savePath, this);
    }

    /**
     * Open the table editor to modify the glycan residue definitions file
     * @param actionEvent
     */
    private void actionBtnEditGlycanResidues(ActionEvent actionEvent) {
        GlycanResidueEditDialog tableDialog = new GlycanResidueEditDialog(SwingUtils.findParentFrame(this), glycanDBloader.glycanResidueDefinitions);

        tableDialog.setVisible(true);
        if (tableDialog.getDialogResult() != JOptionPane.OK_OPTION) {
            return;
        }

        // get new data
        stopJTableEditing(tableDialog.table);
        ArrayList<GlycanResidue> updatedResidues = tableDialog.getModel().getResidues();

        // save to file
        Path savePath = glycoDBpath.resolve(GlycoMassLoader.GLYCAN_RESIDUES_NAME);
        glycanDBloader.updateGlycanResidueDefinitions(updatedResidues);
        saveResiduesToFile(updatedResidues, savePath);
    }

    /**
     * Open a table editor to modify the glycan modification definitions file
     * @param actionEvent
     */
    private void actionBtnEditGlycanMods(ActionEvent actionEvent) {
        GlycanModEditDialog tableDialog = new GlycanModEditDialog(SwingUtils.findParentFrame(this), glycanDBloader.glycanModDefinitions);

        tableDialog.setVisible(true);
        if (tableDialog.getDialogResult() != JOptionPane.OK_OPTION) {
            return;
        }

        // get new data
        stopJTableEditing(tableDialog.table);
        ArrayList<GlycanMod> updatedMods = tableDialog.getModel().getMods(glycanDBloader.glycanResidues);

        // save to file
        Path savePath = glycoDBpath.resolve(GlycoMassLoader.GLYCAN_MODS_NAME);
        glycanDBloader.updateGlycanModDefinitions(updatedMods);
        saveModsToFile(updatedMods, savePath);
    }

    private void saveResiduesToFile(List<GlycanResidue> updatedResidues, Path savePath) {
        ArrayList<String> output = new ArrayList<>();
        output.add(GLYCAN_RESIDUE_HEADER);
        for (GlycanResidue res: updatedResidues) {
            output.add(res.printToDatabaseFile());
        }
        try {
            Files.deleteIfExists(savePath);
            Files.write(savePath, output, StandardOpenOption.CREATE);
        } catch (IOException ex) {
            SwingUtils.showErrorDialogWithStacktrace(ex, this);
        }
    }

    private void saveModsToFile(List<GlycanMod> updatedMods, Path savePath) {
        ArrayList<String> output = new ArrayList<>();
        output.add(GLYCAN_MOD_HEADER);
        for (GlycanMod mod: updatedMods) {
            output.add(mod.printToDatabase());
        }
        try {
            Files.deleteIfExists(savePath);
            Files.write(savePath, output, StandardOpenOption.CREATE);
        } catch (IOException ex) {
            SwingUtils.showErrorDialogWithStacktrace(ex, this);
        }
    }


    /**
     * Save the loaded glycans to all tools and locations as requested
     */
    private void loadGlycansFollowup() {
        if (!glycanDBloader.glycanDB.isEmpty()) {
            TabMsfragger fraggerTab = Fragpipe.getStickyStrict(TabMsfragger.class);
            int numGlycans;
            if (glycanDBloader.optionsPanel.useDetailedOffests()) {
                // detailed offset load
                String detailedOffsetText = glycanDBloader.getGlycanDetailedOffsets();
                fraggerTab.setDetailedOffsets(detailedOffsetText);
                fraggerTab.uiCheckMassOffsetFile.setSelected(true);
                numGlycans = detailedOffsetText.split(";").length;
                if (glycanDBloader.optionsPanel.isNglycanMode()) {
                    fraggerTab.uiComboGlyco.setSelectedItem(MsfraggerParams.GLYCO_OPTIONS.indexOf(MsfraggerParams.GLYCO_OPTION_nglycan));
                }
            } else {
                // regular offset load
                List<String> massStrings = glycanDBloader.getGlycanMassOffsets();
                String offsetsText = String.join(" ", massStrings);
                fraggerTab.setMassOffsets(offsetsText);
                numGlycans = massStrings.size() - 1;    // minus 1 because 0 is always included as an offset
            }
            textLoadGlycans.setText(String.format("Loaded %d unique glycan masses from file", numGlycans));
            log.info(String.format("[Glyco Tab load glycans button] Loaded %d unique mass offsets from file", numGlycans));

            panelGlycanAssign.setGlycanDatabase(glycanDBloader.getGlycanDBString());

            panelOPair.setGlycanDatabase(glycanDBloader.getGlycanDBStringOPair());
            panelOPair.setMaxGlycans(glycanDBloader.optionsPanel.getMaxCombos());   // also sync O-Pair setting for max combos

        } else {
            textLoadGlycans.setText("No glycans loaded");
            log.info("[Glyco Tab load glycans button] no glycans loaded");
        }
    }

    // Find all ".glyc" files in the glycan databases internal folder and return them as a map of name -> File
    private LinkedHashMap<String, File> getGlycanDBs() throws IOException {
        LinkedHashMap<String, File> glycoDBs = new LinkedHashMap<>();

        final Predicate<Path> filter = p -> "glyc".equalsIgnoreCase(StringUtils.afterLastDot(p.getFileName().toString()));
        Files.walk(glycoDBpath).filter(Files::isRegularFile)
                .filter(filter)
                .forEach(Unchecked.consumer(path -> {
                    File f = new File(String.valueOf(path));
                    String s = path.getFileName().toString();
                    String name = StringUtils.upToLastDot(s);
                    glycoDBs.put(StringUtils.isBlank(name) ? s : name, f);
                }));

        return glycoDBs;
    }

    public static boolean stopJTableEditing(JTable t) {
        TableCellEditor editor = t.getCellEditor();
        if (editor == null) {
            return true;
        }
        return editor.stopCellEditing();
    }

}

