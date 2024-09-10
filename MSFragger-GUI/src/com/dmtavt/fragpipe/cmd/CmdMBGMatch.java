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

package com.dmtavt.fragpipe.cmd;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.FragpipeLocations;
import com.dmtavt.fragpipe.api.LcmsFileGroup;
import com.dmtavt.fragpipe.tabs.TabWorkflow;
import com.dmtavt.fragpipe.tools.glyco.GlycoMassLoader;
import com.dmtavt.fragpipe.tools.mbg.MBGParams;

import java.awt.Component;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.jooq.lambda.Seq;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import umich.ms.glyco.GlycanResidue;

import javax.swing.JOptionPane;


public class CmdMBGMatch  extends CmdBase {
    private static final Logger log = LoggerFactory.getLogger(CmdOPair.class);
    public static String NAME = "MBG";
    private static final String JAR_MBG_NAME = "MBG-0.2.1.jar";
    public static final String JAR_MBG_MAIN_CLASS = "com.mbg.MBG";
    public static final String[] JAR_DEPS = {ToolingUtils.BATMASS_IO_JAR};

    public CmdMBGMatch(boolean isRun, Path workDir) {
        super(isRun, workDir);
    }

    @Override
    public String getCmdName() {
        return NAME;
    }

    public boolean configure(Component comp, Path workdir, Map<LcmsFileGroup, Path> sharedMapGroupsToProtxml, MBGParams params, boolean isDryRun, int numThreads, TabWorkflow.InputDataType inputDataType, Map<String, String> uiCompsRepresentation, GlycoMassLoader glycoLoader) {
        initPreConfig();

        final List<Path> classpathJars = FragpipeLocations.checkToolsMissing(Seq.of(JAR_MBG_NAME).concat(JAR_DEPS));
        if (classpathJars == null) {
            return false;
        }

        // check that each group only has lcms files in one directory
        for (LcmsFileGroup group : sharedMapGroupsToProtxml.keySet()) {
            List<String> cmd = new ArrayList<>();
            cmd.add(Fragpipe.getBinJava());
            cmd.add("-cp");
            cmd.add(constructClasspathString(classpathJars));
            cmd.add(JAR_MBG_MAIN_CLASS);
            cmd.add("--match");

            Path psmPath = group.outputDir(workdir).resolve("psm.tsv");     // psm file path relative to group output dir
            cmd.add("--psm");
            cmd.add(psmPath.toString());

            if (!params.getResiduesToAdd().isEmpty()) {
                if (!verifyResidues(params.getResiduesToAdd(), glycoLoader, comp)) {
                    return false;
                }
                cmd.add("--toaddresiduals");
                cmd.add(params.getResiduesToAdd());
            } else {
                if (Fragpipe.headless) {
                    log.error("Error: no glycan residues or mods specified for MBG. Please set residue(s) to match and try again.");
                } else {
                    JOptionPane.showMessageDialog(comp, "Error: no glycan residues or mods specified for MBG. Please set residue(s) to match and try again.", "Error", JOptionPane.ERROR_MESSAGE);
                }
                return false;
            }
            Path fragpipeResiduesFile = FragpipeLocations.get().getDirTools().resolve("Glycan_Databases").resolve("glycan_residues.txt");
            if (!Files.exists(fragpipeResiduesFile)) {
                if (Fragpipe.headless) {
                    log.error(String.format("Error: internal FragPipe glycan residues text file at %s not found! Please check your FragPipe configuration.", fragpipeResiduesFile));
                } else {
                    JOptionPane.showMessageDialog(comp, String.format("Error: internal FragPipe glycan residues text file at %s not found! Please check your FragPipe configuration.", fragpipeResiduesFile), "Error", JOptionPane.ERROR_MESSAGE);
                }
                return false;
            }
            Path fragpipeModsFile = FragpipeLocations.get().getDirTools().resolve("Glycan_Databases").resolve("glycan_mods.txt");
            if (!Files.exists(fragpipeModsFile)) {
                if (Fragpipe.headless) {
                    log.error(String.format("Error: internal FragPipe glycan mods text file at %s not found! Please check your FragPipe configuration.", fragpipeModsFile));
                } else {
                    JOptionPane.showMessageDialog(comp, String.format("Error: internal FragPipe glycan mods text file at %s not found! Please check your FragPipe configuration.", fragpipeModsFile), "Error", JOptionPane.ERROR_MESSAGE);
                }
                return false;
            }
            cmd.add("--residuedb");
            cmd.add(fragpipeResiduesFile.toString());
            cmd.add("--glycanmoddb");
            cmd.add(fragpipeModsFile.toString());
            cmd.add("--maxq");
            cmd.add(String.valueOf(params.getMaxGlycanQ()));
            cmd.add("--minpsms");
            cmd.add(String.valueOf(params.getMinPSMs()));
            cmd.add("--minglycans");
            cmd.add(String.valueOf(params.getMinGlycans()));
            cmd.add("--fdr");
            cmd.add(String.valueOf(params.getFdr()));
            cmd.add("--mztol");
            // get tolerances from IonQuant params
            cmd.add(uiCompsRepresentation.get("ionquant.mztol"));
            cmd.add("--rttol");
            cmd.add(uiCompsRepresentation.get("ionquant.rttol"));
            cmd.add("--imtol");
            cmd.add(uiCompsRepresentation.get("ionquant.imtol"));
            boolean noPASEF = inputDataType != TabWorkflow.InputDataType.ImMsTimsTof;
            cmd.add("--nopasef");
            cmd.add(String.valueOf(noPASEF));
            cmd.add("--numthreads");
            cmd.add(String.valueOf(numThreads));

            ProcessBuilder pb = new ProcessBuilder(cmd);
            pb.directory(wd.toFile());

            pbis.add(PbiBuilder.from(pb));
        }
        isConfigured = true;
        return true;
    }

    /**
     * Confirm that all provided strings are valid residues or mods
     */

    private boolean verifyResidues(String residuesToAdd, GlycoMassLoader glycoLoader, Component comp) {
        String[] splits = residuesToAdd.split("[\\s,;/]+");
        for (String s : splits) {
            if (!glycoLoader.glycanResidues.containsKey(s)) {
                if (!glycoLoader.glycanMods.containsKey(s)) {
                    // check alternate names for residues and mods
                    if (!canFindAltName(s, glycoLoader.glycanResidueDefinitions)) {
                        if (!canFindAltName(s, glycoLoader.glycanModDefinitions)) {
                            // could not find a match for this name anywhere
                            if (Fragpipe.headless) {
                                log.error(String.format("Error: no glycan residue or mod could be matched to input %s. MBG cannot be run.", s));
                            } else {
                                JOptionPane.showMessageDialog(comp, String.format("Error: no glycan residue or mod could be matched to input %s. MBG cannot be run.", s), "Error", JOptionPane.ERROR_MESSAGE);
                            }
                        }
                    }
                }
            }
        }
        return true;
    }

    private boolean canFindAltName(String name, List<? extends GlycanResidue> residues) {
        for (GlycanResidue residue: residues) {
            for (String altname : residue.alternateNames) {
                if (altname.equals(name)) {
                    return true;
                }
            }
        }
        return false;
    }
}
