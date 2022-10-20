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

package com.dmtavt.fragpipe.cmd;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.FragpipeLocations;
import com.dmtavt.fragpipe.api.InputLcmsFile;
import com.dmtavt.fragpipe.api.LcmsFileGroup;
import com.dmtavt.fragpipe.tools.opair.OPairParams;
import com.github.chhh.utils.OsUtils;
import com.github.chhh.utils.SwingUtils;
import org.jooq.lambda.Seq;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.*;
import java.awt.*;
import java.nio.file.Path;
import java.util.*;
import java.util.List;
import java.util.stream.Collectors;

import static com.github.chhh.utils.OsUtils.isUnix;
import static com.github.chhh.utils.OsUtils.isWindows;

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

    public boolean configure(Component comp, Path workdir, Map<LcmsFileGroup, Path> sharedMapGroupsToProtxml, OPairParams params) {
        initPreConfig();

        // check that each group only has lcms files in one directory
        ArrayList<Path> allRawPaths = new ArrayList<>();
        for (LcmsFileGroup group : sharedMapGroupsToProtxml.keySet()) {
            List<Path> lcmsPathsForGroup = group.lcmsFiles.stream().map(inputLcmsFile -> inputLcmsFile.getPath().getParent()).distinct().collect(Collectors.toList());
            if (lcmsPathsForGroup.size() != 1) {
                if (Fragpipe.headless) {
                    log.error("O-Pair requires all LCMS files in a group/experiment to be in one directory.");
                } else {
                    String msg = "O-Pair requires all LCMS files in a group/experiment to be in one directory.\n<br/><br/>"
                            + "<b>Check 'Workflows' tab, 'Input LCMS files' section.</b>";
                    SwingUtils.showDialog(comp, SwingUtils.createClickableHtml(msg), NAME + " configuration error", JOptionPane.WARNING_MESSAGE);
                    log.error(msg);
                }
                return false;
            } else {
                allRawPaths.add(lcmsPathsForGroup.get(0));
            }
        }

        List<String> cmd = new ArrayList<>();
        final String opair_bin = OsUtils.isUnix() ? "opair/CMD.dll" :
                OsUtils.isWindows() ? "opair/CMD.exe" : null;
        if (OsUtils.isUnix()) {
            cmd.add("dotnet ");
        }
        cmd.add(FragpipeLocations.checkToolsMissing(Seq.of(opair_bin)).get(0).toString());

        Path psmPath = workdir.resolve("psm.tsv");
        cmd.add("-b " + params.getProductPPMtol());
        cmd.add("-c " + params.getPrecursorPPMtol());
        if (params.getOglycanDB().length() > 0) {
            cmd.add("-g " + params.getOglycanDB());
        }
        cmd.add("-n " + params.getMaxNumGlycans());
        cmd.add("-d " + allRawPaths.get(0));        // rawfile dir   // todo: fix for multiple groups
        cmd.add("-s " + psmPath);        // psm file path
        cmd.add("-o " + workdir);        // output dir

        ProcessBuilder pb = new ProcessBuilder(cmd);
        pb.directory(wd.toFile());

        pbis.add(PbiBuilder.from(pb));

        isConfigured = true;
        return true;
    }
}
