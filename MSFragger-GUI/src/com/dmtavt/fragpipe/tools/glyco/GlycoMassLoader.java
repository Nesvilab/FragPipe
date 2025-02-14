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

package com.dmtavt.fragpipe.tools.glyco;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.FragpipeLocations;
import com.dmtavt.fragpipe.dialogs.GlycanLoaderFollowupPanel;
import com.dmtavt.fragpipe.tabs.TabGlyco;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.swing.FileChooserUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import umich.ms.glyco.GlycanParser;
import umich.ms.glyco.Glycan;
import umich.ms.glyco.GlycanResidue;
import umich.ms.glyco.GlycanMod;
import umich.ms.glyco.DatabaseType;

import javax.swing.*;
import javax.swing.filechooser.FileFilter;
import javax.swing.filechooser.FileNameExtensionFilter;
import java.awt.*;
import java.io.*;
import java.util.*;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static javax.swing.JOptionPane.OK_CANCEL_OPTION;

public class GlycoMassLoader {
    public List<Glycan> glycanDB;
    public List<Glycan> glycanDBnoCombos;
    public GlycanLoaderFollowupPanel optionsPanel;
    public HashMap<String, GlycanResidue> glycanResidues;
    public ArrayList<GlycanResidue> glycanResidueDefinitions;
    public HashMap<String, GlycanMod> glycanMods;
    public List<GlycanMod> glycanModDefinitions;

    private static final Logger log = LoggerFactory.getLogger(TabGlyco.class);
    public static final String GLYCAN_RESIDUES_NAME = "glycan_residues.txt";
    public static final String GLYCAN_MODS_NAME = "glycan_mods.txt";
    public static final String PROP_FILECHOOSER_LAST_PATH = "glycoloader.filechooser.path";

    public GlycoMassLoader(boolean msfraggerOnly) {
        glycanDB = new ArrayList<>();
        glycanDBnoCombos = new ArrayList<>();
        glycanResidueDefinitions = new ArrayList<>();
        glycanModDefinitions = new ArrayList<>();

        glycanResidues = GlycanParser.parseGlycoResiduesDB(FragpipeLocations.get().getDirTools().resolve("Glycan_Databases").resolve(GLYCAN_RESIDUES_NAME).toString());
        glycanResidueDefinitions = new ArrayList<>(glycanResidues.values());
        glycanResidueDefinitions.sort(GlycanResidue::compareTo);
        glycanMods = GlycanParser.parseGlycoModsDB(FragpipeLocations.get().getDirTools().resolve("Glycan_Databases").resolve(GLYCAN_MODS_NAME).toString(), glycanResidues.size(), glycanResidues);
        glycanModDefinitions = new ArrayList<>(glycanMods.values());
        glycanModDefinitions.sort(GlycanMod::compareTo);
        glycanResidues.putAll(glycanMods);
    }




    public void updateGlycanResidueDefinitions(ArrayList<GlycanResidue> residues) {
        glycanResidueDefinitions = residues;
        glycanResidues = new HashMap<>();
        for (GlycanResidue residue: residues) {
            glycanResidues.put(residue.name, residue);
        }
    }

    public void updateGlycanModDefinitions(ArrayList<GlycanMod> mods) {
        glycanModDefinitions = mods;
        glycanMods = new HashMap<>();
        for (GlycanMod mod: mods) {
            glycanMods.put(mod.name, mod);
        }
    }

    private void loadGlycansFromFile(String selectedPath) {
        DatabaseType dbType = GlycanParser.detectDBtype(selectedPath);
        glycanDB = GlycanParser.loadGlycansFromText(selectedPath, dbType, glycanResidues);
    }

    public void loadCustomGlycans(Component parent) {
        List<javax.swing.filechooser.FileFilter> glycFilters = new ArrayList<>();
        FileFilter filter = new FileNameExtensionFilter("Glycan or Mod Database (.txt, .csv, .tsv, .glyc, .gdb)", "txt", "csv", "tsv", "glyc", "gdb");
        glycFilters.add(filter);
        String loc = Fragpipe.propsVarGet(PROP_FILECHOOSER_LAST_PATH);
        JFileChooser fc = FileChooserUtils.builder("Select the Glycan or other Mod List file")
                .approveButton("Select").mode(FileChooserUtils.FcMode.FILES_ONLY)
                .acceptAll(false).multi(false).filters(glycFilters)
                .paths(Stream.of(loc)).create();

        String selectedPath;
        int userSelection = fc.showOpenDialog(SwingUtils.findParentFrameForDialog(parent));
        if (JFileChooser.APPROVE_OPTION == userSelection) {
            selectedPath = fc.getSelectedFile().toString();
            Fragpipe.propsVarSet(PROP_FILECHOOSER_LAST_PATH, selectedPath);
            loadGlycansFromFile(selectedPath);
            loadGlycansHelper(parent);
        }
    }

    public void loadGlycansFromDatabase(Component parent, File glycoFile) {
        loadGlycansFromFile(glycoFile.toString());
        loadGlycansHelper(parent);
    }

    public void loadGlycansHelper(Component parent) {
        if (!glycanDB.isEmpty()) {
            // glycan mods, combinations and filtering
            optionsPanel = new GlycanLoaderFollowupPanel(this);

            final int confirmation = SwingUtils.showConfirmDialog2(parent, optionsPanel, "Glycan database options", OK_CANCEL_OPTION);
            if (JOptionPane.OK_OPTION == confirmation) {
                TabGlyco.stopJTableEditing(optionsPanel.tableGlycoMods);
                if (optionsPanel.isGlycanModsChanged()) {
                    // user has specified glycan mods: update glycan database
                    addModsToGlycanDB(optionsPanel.getMaxModCombos());
                }
                if (optionsPanel.useMassFilter()) {
                    glycanDB = filterMasses(glycanDB, optionsPanel.getMinMass(), optionsPanel.getMaxMass());
                }
                // combine glycan masses if requested (e.g. O-glycans)
                glycanDBnoCombos = new ArrayList<>(glycanDB);       // hold a copy of glycans without combination for O-Pair
                if (optionsPanel.getMaxCombos() > 1) {
                    glycanDB = generateGlycanCombos(glycanDB, optionsPanel.getMaxCombos(), optionsPanel.useMassFilter(), optionsPanel.getMaxMass(), optionsPanel.getMinMass());
                }
            } else {
                // reset the loaded glyans if user cancels at this point
                glycanDB = new ArrayList<>();
            }
        }
    }

    /**
     * Update the loaded glycan DB with the user-specified glycan mods.
     * Call before generating glycan combinations or mass filtering.
     */
    private void addModsToGlycanDB(int maxCombos) {
        List<GlycanMod> enabledMods = optionsPanel.tableGlycoMods.model.getModifications(this).stream().filter(GlycanMod::isEnabled).collect(Collectors.toList());
        HashSet<String> glycansInDB = new HashSet<>();
        for (Glycan glycan: glycanDB) {
            glycansInDB.add(glycan.name);
        }
        glycanDB = addModsRecurse(glycanDB, enabledMods, glycansInDB, maxCombos);
    }

    /**
     * Recursive helper for adding modifications, used to generate all combinations of modifications by
     * adding all modified glycans for one mod to the glycan database, then recursing.
     * @param glycans
     * @param mods
     * @return
     */
    private List<Glycan> addModsRecurse(List<Glycan> glycans, List<GlycanMod> mods, HashSet<String> glycansInDB, int maxCombos) {
        if (mods.isEmpty()) {
            // reached the end
            return glycans;
        } else {
            GlycanMod thisMod = mods.remove(0);
            // add this mod to the glycans list
            List<Glycan> glycansWithMod = new ArrayList<>();
            for (Glycan glycan: glycans) {
                if (thisMod.isVariable) {
                    // varible mod - include both modified and unmodified glycans
                    glycansWithMod.add(glycan);
                    if (glycan.numModTypes >= maxCombos) {
                        continue;
                    }
                    int maxAllowed = thisMod.maxAllowed;
                    if (!thisMod.requiredRes.isEmpty()) {
                        int numAllowedSites = glycan.getNumAllowedSites(thisMod);
                        maxAllowed = Math.min(maxAllowed, numAllowedSites);
                    }
                    for (int i = 0; i < maxAllowed; i++) {
                        glycansWithMod.add(glycan.addMod(thisMod, i + 1));
                    }
                } else {
                    // fixed mod - do NOT include unmodified glycan. Ignore max allowed.
                    int maxAllowed = glycan.getNumResidues();
                    if (!thisMod.requiredRes.isEmpty()) {
                        maxAllowed = glycan.getNumAllowedSites(thisMod);
                    }
                    for (int i = 0; i < maxAllowed; i++) {
                        glycansWithMod.add(glycan.addMod(thisMod, i + 1));
                    }
                }
            }
            for (Glycan glycan: glycansWithMod) {
                if (!glycansInDB.contains(glycan.name)) {
                    glycans.add(glycan);
                    glycansInDB.add(glycan.name);
                }
            }
            return addModsRecurse(glycans, mods, glycansInDB, maxCombos);
        }
    }

    // Method for filtering if NOT using mass combinations
    private static List<Glycan> filterMasses(List<Glycan> inputGlycans, double minMass, double maxMass) {
        List<Glycan> filteredGlycans = new ArrayList<>();
        for (Glycan glycan : inputGlycans) {
            if (glycan.mass <= maxMass && glycan.mass >= minMass) {
                filteredGlycans.add(glycan);
            }
        }
        return filteredGlycans;
    }

    /**
     * Generate all combinations of provided masses up to the specified max number. Remove duplicates (at 4 decimal places)
     * Uses combinations with repetition since same glycan can occur multiple times on a peptide.
     * @return
     */
    private List<Glycan> generateGlycanCombos(List<Glycan> glycans, int maxCombos, boolean massFilter, double maxMass, double minMass) {
        Set<String> existingGlycans = new HashSet<>();
        List<Glycan> allGlycans = new ArrayList<>();
        for (int count = 1; count <= maxCombos; count++) {
            // iterate combinations
            List<int[]> combos = GlycoMassLoader.combinationsWithRepetition(glycans.size(), count);
            for (int[] combo : combos) {
                Glycan comboGlycan = new Glycan(new HashMap<>());
                // calculate mass as the sum of the glycans at selected indices
                for (int i : combo) {
                    comboGlycan.addGlycan(glycans.get(i));
                }
                // check for duplicates and add if unique
                String glycKey = comboGlycan.name;
                if (!existingGlycans.contains(glycKey)) {
                    if (!massFilter) {
                        allGlycans.add(comboGlycan);
                        existingGlycans.add(glycKey);
                    } else {
                        // filtering requested
                        if (comboGlycan.mass <= maxMass && comboGlycan.mass >= minMass) {
                            allGlycans.add(comboGlycan);
                            existingGlycans.add(glycKey);
                        }
                    }
                }
            }
        }
        return allGlycans;
    }

    /**
     * Format a string representation of the current glyco database for saving to parameter files.
     * @return
     */
    public String getGlycanDBString() {
        StringBuilder builder = new StringBuilder();
        for (int i=0; i < glycanDB.size(); i++) {
            builder.append(glycanDB.get(i).name);
            if (i < glycanDB.size() - 1) {
                builder.append(",");
            }
        }
        return builder.toString();
    }

    /**
     * Format a string representation of the current glyco database without considering glycan combinations, for O-Pair.
     * @return
     */
    public String getGlycanDBStringOPair() {
        // if combinations aren't requested, use the base database
        List<Glycan> glycans = glycanDBnoCombos.isEmpty() ? glycanDB : glycanDBnoCombos;

        StringBuilder builder = new StringBuilder();
        for (int i=0; i < glycans.size(); i++) {
            builder.append(glycans.get(i).name);
            if (i < glycans.size() - 1) {
                builder.append(",");
            }
        }
        return builder.toString();
    }

    // recursive combination generator (with repetition)
    public static List<int[]> combinationsWithRepetition(int numItems, int numChoices) {
        List<int[]> combinations = new ArrayList<>();
        combinationsHelper(combinations, new int[numChoices], 0, numItems-1, 0);
        return combinations;
    }

    // helper method for recursive combination generator
    private static void combinationsHelper(List<int[]> combinations, int[] data, int start, int end, int index) {
        if (index == data.length) {
            int[] combination = data.clone();
            combinations.add(combination);
        } else if (start <= end) {
            data[index] = start;
            combinationsHelper(combinations, data, start, end, index + 1);  // include start to allow repetition
            combinationsHelper(combinations, data, start + 1, end, index);
        }
    }
}
