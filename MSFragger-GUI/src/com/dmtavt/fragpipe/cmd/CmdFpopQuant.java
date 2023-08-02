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
import com.dmtavt.fragpipe.exceptions.NoStickyException;
import com.dmtavt.fragpipe.messages.NoteConfigPython;
import com.dmtavt.fragpipe.tools.fpop.FpopScript;
import com.dmtavt.fragpipe.tools.ionquant.QuantPanelLabelfree;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.*;
import java.awt.*;
import java.nio.file.Path;
import java.util.*;
import java.util.List;

public class CmdFpopQuant extends CmdBase{
    private static final Logger log = LoggerFactory.getLogger(CmdFreequant.class);
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

        final Path filepath = wd.resolve("combined_modified_peptide.tsv");
        final QuantPanelLabelfree lfqPanel = Fragpipe.getStickyStrict(QuantPanelLabelfree.class);
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
        cmd.add(String.valueOf(lfqPanel.getFpopRegionSize()));
        cmd.add(lfqPanel.getFpopFpopLabel());
        cmd.add(lfqPanel.getFpopControlLabel());

        ProcessBuilder pb = new ProcessBuilder(cmd);
        pb.directory(wd.toFile());

        pbis.add(PbiBuilder.from(pb));
        isConfigured = true;
        return true;
    }

}
