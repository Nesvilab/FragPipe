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
import org.nesvilab.fragpipe.api.DotnetInfo;
import org.nesvilab.fragpipe.api.InputLcmsFile;
import org.nesvilab.fragpipe.api.LcmsFileGroup;
import org.nesvilab.fragpipe.exceptions.UnexpectedException;
import org.nesvilab.fragpipe.exceptions.ValidationException;
import org.nesvilab.fragpipe.tabs.TabGlyco;
import org.nesvilab.fragpipe.tools.enums.ActivationTypes;
import org.nesvilab.fragpipe.tools.glyco.GlycoMassLoader;
import org.nesvilab.fragpipe.tools.opair.OPairParams;
import org.nesvilab.utils.OsUtils;
import org.nesvilab.utils.StringUtils;
import org.nesvilab.utils.SwingUtils;
import java.awt.Component;
import java.awt.Desktop;
import java.io.BufferedWriter;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import org.jooq.lambda.Seq;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import javax.swing.JOptionPane;


public class CmdOPair  extends CmdBase {
    private static final Logger log = LoggerFactory.getLogger(CmdOPair.class);
    public static String NAME = "OPair";
    public static final String OPAIR_FOLDER = "opair";
    public static final String OPAIR_MODS_FOLDER = "Glycan_Mods";
    public static final String DEFAULT_OXO_FILE = "oxonium_filter.txt";

    public CmdOPair(boolean isRun, Path workDir) {
        super(isRun, workDir);
    }

    @Override
    public String getCmdName() {
        return NAME;
    }

    public boolean configure(Component comp, Path workdir, Map<LcmsFileGroup, Path> sharedMapGroupsToProtxml, OPairParams params, boolean isDryRun, boolean hasCalibratedMzml, int numThreads) {
        initPreConfig();

        // check that .NET is available
        try {
            DotnetInfo.fromCommand(DotnetInfo.COMMAND);
        } catch (ValidationException | UnexpectedException e) {
            if (Fragpipe.headless) {
                log.error("O-Pair was enabled, but .NET Core 6.0 was not found. Please download it from https://dotnet.microsoft.com/en-us/download");
            } else {
                int choice = SwingUtils.showChoiceDialog(comp, "Download .NET Core", "O-Pair was enabled, but .NET Core 6.0 was not found.\nDownload the installer from Microsoft website?", new String[]{"Yes", "No"}, 0);
                if (choice == 0) {
                    try {
                        Desktop.getDesktop().browse(URI.create("https://dotnet.microsoft.com/en-us/download"));
                    } catch (Exception ex) {
                        SwingUtils.showErrorDialogWithStacktrace(ex, null);
                    }
                }
                return false;
            }
        }
        
        if (params.isSingleScanType() && (params.getActivation1().equals(ActivationTypes.HCD.getText())) || params.getActivation1().equals(ActivationTypes.CID.getText())) {
            if (Fragpipe.headless) {
                log.error("Single activation type must be hybrid, not HCD/CID, for O-Pair because c/z ions are required.");
            } else {
                JOptionPane.showMessageDialog(comp, "Single activation type must be hybrid, not HCD/CID, for O-Pair because c/z ions are required.", "Error", JOptionPane.ERROR_MESSAGE);
            }
            return false;
        }

        // check that each group only has lcms files in one directory
        for (LcmsFileGroup group : sharedMapGroupsToProtxml.keySet()) {
            List<Path> lcmsPathList = group.lcmsFiles.stream().map(InputLcmsFile::getPath).distinct().sorted().collect(Collectors.toList());
            Path experimentPath = wd.resolve(group.name);
            Path fileListPath = experimentPath.resolve("filelist_opair.txt");
            if (!isDryRun) {
                try {
                    Files.createDirectories(experimentPath);
                    BufferedWriter bufferedWriter = Files.newBufferedWriter(fileListPath);
                    for (Path p : lcmsPathList) {
                        String baseName = StringUtils.upToLastDot(p.toAbsolutePath().normalize().toString());
                        String extension = StringUtils.afterLastDot(p.toAbsolutePath().normalize().toString());
                        if (hasCalibratedMzml) {
                            bufferedWriter.write(baseName + "_calibrated.mzML\n");
                        } else if (extension.equalsIgnoreCase("raw") || extension.equalsIgnoreCase("d")) {
                            bufferedWriter.write(baseName + "_uncalibrated.mzML\n");
                        } else {
                            bufferedWriter.write(p.toAbsolutePath() + "\n");
                        }
                    }
                    bufferedWriter.close();
                } catch (Exception ex) {
                    SwingUtils.showErrorDialogWithStacktrace(ex, comp);
                    return false;
                }
            }

            List<String> cmd = new ArrayList<>();
            final String opair_bin = OsUtils.isUnix() ? "opair/CMD.dll" :
                    OsUtils.isWindows() ? "opair/CMD.exe" : null;

            if (opair_bin == null) {
                if (Fragpipe.headless){
                    log.error("O-Pair only supports Windows, Linux, and Unix. Could not locate an appropriate binary for this OS.");
                } else {
                    SwingUtils.showErrorDialog(comp, "O-Pair only supports Windows, Linux, and Unix.", "Error");
                }return false;
            }

            List<Path> t = FragpipeLocations.checkToolsMissing(Seq.of(opair_bin));
            if (t == null || t.isEmpty()) {
                if (Fragpipe.headless) {
                    log.error("Could not find O-Pair executable file.");
                } else {
                    SwingUtils.showErrorDialog(comp, "Could not find O-Pair executable file.", "Error");
                }
                return false;
            }

            // get glycan residue and mod definition files
            final Path dirTools = FragpipeLocations.get().getDirTools();
            Path glycanDBfolder = Paths.get(dirTools.toString(), TabGlyco.glycanDBfolder);
            Path modsFolder = Paths.get(dirTools.toString(), OPAIR_FOLDER, OPAIR_MODS_FOLDER);
            Path residuesPath = glycanDBfolder.resolve(GlycoMassLoader.GLYCAN_RESIDUES_NAME);
            if (!Files.exists(residuesPath)) {
                if (Fragpipe.headless) {
                    log.error(String.format("Could not find Glycan residue definitions file at %s. Please make sure this file has not been removed and try again.", residuesPath));
                } else {
                    SwingUtils.showErrorDialog(comp, String.format("Could not find Glycan residue definitions file at %s. Please make sure this file has not been removed and try again.", residuesPath), "Error");
                }
                return false;
            }
            Path modsPath = glycanDBfolder.resolve(GlycoMassLoader.GLYCAN_MODS_NAME);
            if (!Files.exists(modsPath)) {
                if (Fragpipe.headless) {
                    log.error(String.format("Could not find Glycan mod definitions file at %s. Please make sure this file has not been removed and try again.", modsPath));
                } else {
                    SwingUtils.showErrorDialog(comp, String.format("Could not find Glycan mod definitions file at %s. Please make sure this file has not been removed and try again.", modsPath), "Error");
                }
                return false;
            }

            if (OsUtils.isUnix()) {
                cmd.add("dotnet");
            }

            cmd.add(t.get(0).toString());

            Path psmPath = group.outputDir(workdir).resolve("psm.tsv");     // psm file path relative to group output dir
            cmd.add("-b " + params.getProductPPMtol());
            cmd.add("-c " + params.getPrecursorPPMtol());
            if (params.isFilterOxonium()) {
                if (params.getOxoRulesFilePath().isEmpty()) {
                    // no file provided - use default file in FragPipe
                    Path oxoPath = modsFolder.resolve(DEFAULT_OXO_FILE);
                    if (!Files.exists(oxoPath)) {
                        if (Fragpipe.headless) {
                            log.error(String.format("Could not find default oxonium ion definitions file at %s. Please make sure this file has not been removed and try again.", oxoPath));
                        } else {
                            SwingUtils.showErrorDialog(comp, String.format("Could not find default oxonium ion definitions file at %s. Please make sure this file has not been removed and try again.", oxoPath), "Error");
                        }
                        return false;
                    }
                    cmd.add("-f " + oxoPath.toAbsolutePath());
                } else {
                    cmd.add("-f " + params.getOxoRulesFilePath());
                }
                cmd.add("-m " + params.getOxoMinInt());
            }
            if (params.getOglycanDB().length() > 0) {
                cmd.add("-g " + params.getOglycanDB());
            }
            cmd.add("-x " + residuesPath.toAbsolutePath());
            cmd.add("-y " + modsPath.toAbsolutePath());
            cmd.add("-n " + params.getMaxNumGlycans());
            cmd.add("-z " + params.getAllowedSites());
            cmd.add("-t " + numThreads);
            cmd.add("-i " + params.getMinIsotope());
            cmd.add("-j " + params.getMaxIsotope());
            cmd.add("-r " + fileListPath.toAbsolutePath());
            cmd.add("-s " + psmPath);
            cmd.add("-o " + group.outputDir(workdir));          // output dir relative to group

            ProcessBuilder pb = new ProcessBuilder(cmd);
            pb.directory(wd.toFile());

            pbis.add(PbiBuilder.from(pb));
        }
        isConfigured = true;
        return true;
    }
}
