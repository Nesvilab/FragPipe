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

package org.nesvilab.fragpipe.cmd;

import org.nesvilab.fragpipe.Fragpipe;
import org.nesvilab.fragpipe.FragpipeLocations;
import org.nesvilab.fragpipe.api.InputLcmsFile;
import org.nesvilab.fragpipe.api.LcmsFileGroup;
import org.nesvilab.fragpipe.tabs.TabWorkflow;
import org.nesvilab.fragpipe.tools.glyco.GlycoMassLoader;
import org.nesvilab.fragpipe.tools.mbg.MBGParams;

import java.awt.Component;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import org.jooq.lambda.Seq;
import org.nesvilab.utils.SwingUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import umich.ms.glyco.Glycan;
import umich.ms.glyco.GlycanResidue;
import static umich.ms.glyco.GlycanParser.parseGlycanString;
import javax.swing.JOptionPane;
import static org.nesvilab.fragpipe.cmd.CmdPairScans.checkCompatibleFormats;


public class CmdMBGMatch  extends CmdBase {
    private static final Logger log = LoggerFactory.getLogger(CmdMBGMatch.class);
    public static String NAME = "MBG";
    private static final String JAR_MBG_NAME = "MBG-0.3.5.jar";
    public static final String JAR_MBG_MAIN_CLASS = "com.mbg.MBG";
    public static final String[] JAR_DEPS = {ToolingUtils.BATMASS_IO_JAR};
    private static final List<String> SUPPORTED_FORMATS = Arrays.asList("mzML", "mzXML");

    public CmdMBGMatch(boolean isRun, Path workDir) {
        super(isRun, workDir);
    }

    @Override
    public String getCmdName() {
        return NAME;
    }

    public boolean configure(Component component,
                             Path workdir,
                             Map<LcmsFileGroup, Path> sharedMapGroupsToProtxml,
                             MBGParams params,
                             boolean isDryRun,
                             int numThreads,
                             TabWorkflow.InputDataType inputDataType,
                             Map<String, String> uiCompsRepresentation,
                             GlycoMassLoader glycoLoader,
                             Path manifestPath,
                             Path binIonQuant,
                             Path extLibsThermo,
                             Path extLibsBruker,
                             List<InputLcmsFile> lcmsFiles,
                             boolean isRunTmt) {
        initPreConfig();

        final List<Path> classpathJars = FragpipeLocations.checkToolsMissing(Seq.of(JAR_MBG_NAME).concat(JAR_DEPS));
        if (classpathJars == null) {
            return false;
        }

        // check raw data reader libraries
        List<String> sup = new ArrayList<>(SUPPORTED_FORMATS);
        if (extLibsBruker != null) {
            sup.add("d");
        }
        if (extLibsThermo != null) {
            sup.add("raw");
        }
        if (!checkCompatibleFormats(component, lcmsFiles, sup)) {
            return false;
        }
        String brukerLib = "";
        String thermoLib = "";
        if (extLibsBruker != null) {
            brukerLib = createJavaDParamString("libs.bruker.dir", extLibsBruker.toString());
        } else {
            if (lcmsFiles.stream().anyMatch(f -> f.getPath().getFileName().toString().toLowerCase().endsWith(".d"))) {
                if (Fragpipe.headless) {
                    log.error("When processing .d files, MBG requires native Bruker libraries. Native libraries come with MSFragger zip download, contained in ext sub-directory.");
                } else {
                    SwingUtils.showErrorDialog(component, "When processing .d files, MBG requires native Bruker libraries. Native libraries come with MSFragger zip download, contained in ext sub-directory.", NAME + " error");
                }
                return false;
            }
        }
        if (extLibsThermo != null) {
            thermoLib = createJavaDParamString("libs.thermo.dir", extLibsThermo.toString());
        } else {
            if (lcmsFiles.stream().anyMatch(f -> f.getPath().getFileName().toString().toLowerCase().endsWith(".raw"))) {
                if (Fragpipe.headless) {
                    log.error("When processing .RAW files, MBG requires native Thermo libraries. Native libraries come with MSFragger zip download, contained in ext sub-directory.");
                } else {
                    SwingUtils.showErrorDialog(component, "When processing .RAW files, MBG requires native Thermo libraries. Native libraries come with MSFragger zip download, contained in ext sub-directory.", NAME + " error");
                }
                return false;
            }
        }

        // check that each group only has lcms files in one directory
        for (LcmsFileGroup group : sharedMapGroupsToProtxml.keySet()) {
            List<String> cmd = new ArrayList<>();
            cmd.add(Fragpipe.getBinJava());
            if (!brukerLib.isEmpty()) {
                cmd.add(brukerLib);
            }
            if (!thermoLib.isEmpty()) {
                cmd.add(thermoLib);
            }
            cmd.add("-cp");
            cmd.add(constructClasspathString(classpathJars, binIonQuant));
            cmd.add(JAR_MBG_MAIN_CLASS);
            cmd.add("--match");

            Path psmPath = group.outputDir(workdir).resolve("psm.tsv");     // psm file path relative to group output dir
            cmd.add("--psm");
            cmd.add(psmPath.toString());
            cmd.add("--manifest");
            cmd.add(manifestPath.toAbsolutePath().toString());

            if (!params.getResiduesToAdd().isEmpty()) {
                if (!verifyResidues(params.getResiduesToAdd(), glycoLoader, component)) {
                    return false;
                }
                cmd.add("--toaddresiduals");
                cmd.add(params.getResiduesToAdd());
            } else {
                if (Fragpipe.headless) {
                    log.error("Error: no glycan residues or mods specified for MBG. Please set residue(s) to match and try again.");
                } else {
                    JOptionPane.showMessageDialog(component, "Error: no glycan residues or mods specified for MBG. Please set residue(s) to match and try again.", "Error", JOptionPane.ERROR_MESSAGE);
                }
                return false;
            }
            Path fragpipeResiduesFile = FragpipeLocations.get().getDirTools().resolve("Glycan_Databases").resolve("glycan_residues.txt");
            if (!Files.exists(fragpipeResiduesFile)) {
                if (Fragpipe.headless) {
                    log.error("Error: internal FragPipe glycan residues text file at {} not found! Please check your FragPipe configuration.", fragpipeResiduesFile);
                } else {
                    JOptionPane.showMessageDialog(component, String.format("Error: internal FragPipe glycan residues text file at %s not found! Please check your FragPipe configuration.", fragpipeResiduesFile), "Error", JOptionPane.ERROR_MESSAGE);
                }
                return false;
            }
            Path fragpipeModsFile = FragpipeLocations.get().getDirTools().resolve("Glycan_Databases").resolve("glycan_mods.txt");
            if (!Files.exists(fragpipeModsFile)) {
                if (Fragpipe.headless) {
                    log.error("Error: internal FragPipe glycan mods text file at {} not found! Please check your FragPipe configuration.", fragpipeModsFile);
                } else {
                    JOptionPane.showMessageDialog(component, String.format("Error: internal FragPipe glycan mods text file at %s not found! Please check your FragPipe configuration.", fragpipeModsFile), "Error", JOptionPane.ERROR_MESSAGE);
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
            cmd.add("--runtmt");
            cmd.add(String.valueOf(isRunTmt));
            cmd.add("--expanddb");
            cmd.add(String.valueOf(params.getExpandDB()));
            cmd.add("--maxskips");
            cmd.add(String.valueOf(params.getMaxSkips()));
            cmd.add("--allowchimeric");
            cmd.add(String.valueOf(params.allowChimeric()));

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
            Glycan testGlyc = parseGlycanString(s, glycoLoader.glycanResidues);
            if (testGlyc == null) {
                // could not parse the string as a glycan
                if (Fragpipe.headless) {
                    log.error("Error: MBG glycan difference input {} for expansion could not be parsed as a glycan. Please check that all residues/mods are defined and the format is correct.", s);
                } else {
                    JOptionPane.showMessageDialog(comp, String.format("Error: MBG glycan difference input %s could not be parsed as a glycan. Please check that all residues/mods are defined and the format is correct.", s), "Error", JOptionPane.ERROR_MESSAGE);
                }
                return false;
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
