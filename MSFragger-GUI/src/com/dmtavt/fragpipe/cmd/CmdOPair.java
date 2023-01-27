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
import com.dmtavt.fragpipe.api.DotnetInfo;
import com.dmtavt.fragpipe.api.InputLcmsFile;
import com.dmtavt.fragpipe.api.LcmsFileGroup;
import com.dmtavt.fragpipe.exceptions.UnexpectedException;
import com.dmtavt.fragpipe.exceptions.ValidationException;
import com.dmtavt.fragpipe.tools.opair.OPairParams;
import com.github.chhh.utils.OsUtils;
import com.github.chhh.utils.StringUtils;
import com.github.chhh.utils.SwingUtils;
import java.awt.Component;
import java.awt.Desktop;
import java.io.BufferedWriter;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import org.jooq.lambda.Seq;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class CmdOPair  extends CmdBase {
    private static final Logger log = LoggerFactory.getLogger(CmdOPair.class);
    public static String NAME = "OPair";

    public CmdOPair(boolean isRun, Path workDir) {
        super(isRun, workDir);
    }

    @Override
    public String getCmdName() {
        return NAME;
    }

    public boolean configure(Component comp, Path workdir, Map<LcmsFileGroup, Path> sharedMapGroupsToProtxml, OPairParams params, boolean isDryRun, boolean hasCalibratedMzml) {
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
                        String baseName = StringUtils.upToLastDot(p.toAbsolutePath().toString());
                        String extension = StringUtils.afterLastDot(p.toAbsolutePath().toString());
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
                SwingUtils.showErrorDialog(comp, "O-Pair ony supports Windows, Linux, and Unix.", "Error");
                return false;
            }

            List<Path> t = FragpipeLocations.checkToolsMissing(Seq.of(opair_bin));
            if (t == null || t.isEmpty()) {
                SwingUtils.showErrorDialog(comp, "Could not find O-Pair executable file.", "Error");
                return false;
            }

            if (OsUtils.isUnix()) {
                cmd.add("dotnet");
            }

            cmd.add(t.get(0).toString());

            Path psmPath = group.outputDir(workdir).resolve("psm.tsv");     // psm file path relative to group output dir
            cmd.add("-b " + params.getProductPPMtol());
            cmd.add("-c " + params.getPrecursorPPMtol());
            if (params.getOglycanDB().length() > 0) {
                cmd.add("-g " + params.getOglycanDB());
            }
            cmd.add("-n " + params.getMaxNumGlycans());
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
