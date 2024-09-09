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

    public boolean configure(Component comp, Path workdir, Map<LcmsFileGroup, Path> sharedMapGroupsToProtxml, MBGParams params, boolean isDryRun, boolean hasCalibratedMzml, int numThreads) {
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

            if (params.getGlycanDB().length() > 0) {
                cmd.add("--glycandb");
                cmd.add(params.getGlycanDB());
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
            cmd.add("--residuedb");
            cmd.add(fragpipeResiduesFile.toString());
            cmd.add("--maxq");
            cmd.add(String.valueOf(params.getMaxGlycanQ()));
            cmd.add("--minpsms");
            cmd.add(String.valueOf(params.getMinPSMs()));
            cmd.add("--minglycans");
            cmd.add(String.valueOf(params.getMinGlycans()));

            ProcessBuilder pb = new ProcessBuilder(cmd);
            pb.directory(wd.toFile());

            pbis.add(PbiBuilder.from(pb));
        }
        isConfigured = true;
        return true;
    }
}
