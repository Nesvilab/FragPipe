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
import org.nesvilab.fragpipe.exceptions.NoStickyException;
import org.nesvilab.fragpipe.messages.NoteConfigPython;
import org.nesvilab.fragpipe.tabs.TabDownstream;
import org.nesvilab.fragpipe.tabs.TabQuantificationLabeling;
import org.nesvilab.fragpipe.tools.fpop.FpopScript;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.*;
import java.awt.*;
import java.nio.file.Path;
import java.util.*;
import java.util.List;

public class CmdFpopQuant extends CmdBase{
    private static final Logger log = LoggerFactory.getLogger(CmdFpopQuant.class);
    public static final String NAME = "FPOP";
    private static final String SCRIPT_FPOP_QUANT = "fpop/FragPipe_FPOP_Analysis.py";

    public CmdFpopQuant(boolean isRun, Path workDir) {
        super(isRun, workDir);
    }

    @Override
    public String getCmdName() {
        return NAME;
    }

    public boolean configure(Component comp) {

        initPreConfig();
        final TabDownstream tabDownstream = Fragpipe.getStickyStrict(TabDownstream.class);
        final Path filepath;
        final Path secondTMTpath;
        // Get file paths
        if (tabDownstream.pFpop.isFpopTmt()) {
            final TabQuantificationLabeling tabTMT = Fragpipe.getStickyStrict(TabQuantificationLabeling.class);
            filepath = getTmtiMultisiteTsv(tabTMT.panelTmtI.getNormMethod());
            secondTMTpath = getTmtiPeptideTsv(tabTMT.panelTmtI.getNormMethod());
        } else {
            filepath = wd.resolve("combined_modified_peptide.tsv");
            secondTMTpath = null;
        }

        List<String> cmd = new ArrayList<>();
        if (!FpopScript.get().isInitialized()) {
            if (Fragpipe.headless) {
                log.error("Fpop quant script was requested but not initialized or Python not configured. Check the bottom of the config tab for Python details.");
            } else {
                JOptionPane.showMessageDialog(comp,
                        "Fpop quant script was requested but not initialized or Python not configured. Check the bottom of the config tab for Python details.",
                        "Error", JOptionPane.ERROR_MESSAGE);
            }
            return false;
        }

        try {
            NoteConfigPython configPython = Fragpipe.getSticky(NoteConfigPython.class);
            cmd.add(configPython.pi.getCommand());
            cmd.add(FpopScript.get().getScriptFpopPath().toAbsolutePath().normalize().toString());
        } catch (NoStickyException e) {
            if (Fragpipe.headless) {
                log.error("Fpop quant script was enabled, but Python was not configured.");
            } else {
                JOptionPane.showMessageDialog(comp, "\"Fpop quant script was enabled, but Python was not configured.", "Error", JOptionPane.ERROR_MESSAGE);
            }
            return false;
        }

        // python script args are 1) filepath, 2) region size, 3) FPOP sample label, 4) control sample label
        cmd.add(filepath.toString());
        cmd.add(String.valueOf(tabDownstream.pFpop.getFpopRegionSize()));
        cmd.add(tabDownstream.pFpop.getFpopControlLabel());
        cmd.add(tabDownstream.pFpop.getFpopFpopLabel());
        cmd.add(String.valueOf(tabDownstream.pFpop.getFpopSubtractControl()));
        cmd.add(String.valueOf(tabDownstream.pFpop.isFpopTmt()));
        if (tabDownstream.pFpop.isFpopTmt()) {
            cmd.add(secondTMTpath.toString());
        }

        ProcessBuilder pb = new ProcessBuilder(cmd);
        pb.directory(wd.toFile());

        pbis.add(PbiBuilder.from(pb));
        isConfigured = true;
        return true;
    }

    /**
     * Resolve TMT-Integrator file paths from second unmodified run, preferring no normalization
     * if multiple are present. Case strings are from TmtiConfProps.java / COMBO_NORM
     * @return
     */
    private Path getTmtiPeptideTsv(String normalizationMethod) {
        Path tmtDir = wd.resolve("tmt-report-unmod");
        switch (normalizationMethod) {
            case "None":
            case "All":
                return tmtDir.resolve("abundance_peptide_None.tsv");
            case "MD (median centering)":
                return tmtDir.resolve("abundance_peptide_MD.tsv");
            case "GN (median centering + variance scaling)":
                return tmtDir.resolve("abundance_peptide_GN.tsv");
        }
        return null;
    }

    /**
     * Resolve TMT-Integrator file paths, preferring no normalization if multiple are present.
     * Case strings are from TmtiConfProps.java / COMBO_NORM
     * @return
     */
    private Path getTmtiMultisiteTsv(String normalizationMethod) {
        Path tmtDir = wd.resolve("tmt-report");
        switch (normalizationMethod) {
            case "None":
            case "All":
                return tmtDir.resolve("abundance_multi-site_None.tsv");
            case "MD (median centering)":
                return tmtDir.resolve("abundance_multi-site_MD.tsv");
            case "GN (median centering + variance scaling)":
                return tmtDir.resolve("abundance_multi-site_GN.tsv");
        }
        return null;
    }

}
