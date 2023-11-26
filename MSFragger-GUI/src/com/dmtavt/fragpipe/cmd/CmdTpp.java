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
import com.dmtavt.fragpipe.tabs.TabDownstream;
import com.dmtavt.fragpipe.tools.tpp.TppScript;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.*;
import java.awt.*;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

public class CmdTpp extends CmdBase{
    private static final Logger log = LoggerFactory.getLogger(CmdFreequant.class);
    public static final String NAME = "TPP";

    public CmdTpp(boolean isRun, Path workDir) {
        super(isRun, workDir);
    }

    @Override
    public String getCmdName() {
        return NAME;
    }

    public boolean configure(Component comp,Path pathFasta) {

        initPreConfig();
        final TabDownstream tabDownstream = Fragpipe.getStickyStrict(TabDownstream.class);

        //Main FRagPipe Ouput Folder
        final Path folderpath;
        //TMT output needed for 2DTPP Analysis
        final Path tmtipath;
        // Get file paths
        if (tabDownstream.paneltpp.istwoDTpp()){
            folderpath = wd;
            Path tmtDir = wd.resolve("tmt-report");
            tmtipath = tmtDir.resolve("ratio_protein_None.tsv");
        }else{
            folderpath = null;
            tmtipath = null;

        }

        List<String> cmd = new ArrayList<>();
        if (!TppScript.get().isInitialized()) {
            if (Fragpipe.headless) {
                log.error("TPP script was requested but not initialized or Python not configured. Check the bottom of the config tab for Python details.");
            } else {
                JOptionPane.showMessageDialog(comp,
                        "TPP script was requested but not initialized or Python not configured. Check the bottom of the config tab for Python details.",
                        "Error", JOptionPane.ERROR_MESSAGE);
            }
            return false;
        }

        try {
            NoteConfigPython configPython = Fragpipe.getSticky(NoteConfigPython.class);
            cmd.add(configPython.pi.getCommand());
            cmd.add(TppScript.get().getScriptTpp().toAbsolutePath().normalize().toString());
        } catch (NoStickyException e) {
            if (Fragpipe.headless) {
                log.error("TPP script was enabled, but Python was not configured.");
            } else {
                JOptionPane.showMessageDialog(comp, "\"TPP script was enabled, but Python was not configured.", "Error", JOptionPane.ERROR_MESSAGE);
            }
            return false;
        }

        //Here add parameters in the order needed by the command line

        // 1) check 1DTPP (TPP-R) Analysis
        cmd.add(String.valueOf(tabDownstream.paneltpp.isoneDTppR()));
        //2) check 1DTPP Analysis
        cmd.add(String.valueOf(tabDownstream.paneltpp.isoneDTpp()));
        //3) check 2DTPP Analysis
        cmd.add(String.valueOf(tabDownstream.paneltpp.istwoDTpp()));
        //4) FragPipeoutput folder
        cmd.add(folderpath.toString());
        //5) get path to local R installation
        cmd.add(" ");
        //6) get path to database file (already in string form from CmdBase)
        cmd.add(pathFasta.toString());
        //7 Path to "ratio_protein_None.tsv", output by TMTI
        cmd.add(tmtipath.toString());



        ProcessBuilder pb = new ProcessBuilder(cmd);
        pb.directory(wd.toFile());

        pbis.add(PbiBuilder.from(pb));
        isConfigured = true;
        return true;
    }



}
