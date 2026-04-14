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

import java.awt.Component;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import org.nesvilab.fragpipe.Fragpipe;
import org.nesvilab.fragpipe.FragpipeLocations;
import org.nesvilab.fragpipe.Version;
import org.nesvilab.fragpipe.util.ExtendedAAFastaEdit;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Command that runs {@link ExtendedAAFastaEdit} to produce a copy of the input FASTA file
 * with all extended (non-canonical) amino acid name patterns replaced by "X". The output
 * file is used by downstream tools (PeptideProphet, ProteinProphet, etc.) when the
 * "Use Extended AA Definitions" option is enabled in the MSFragger tab.
 */
public class CmdExtendedAAFastaEdit extends CmdBase {

    public static final String NAME = "ExtendedAAFastaEdit";
    private static final Logger log = LoggerFactory.getLogger(CmdExtendedAAFastaEdit.class);

    public CmdExtendedAAFastaEdit(boolean isRun, Path workDir) {
        super(isRun, workDir);
    }

    @Override
    public String getCmdName() {
        return NAME;
    }

    /**
     * Configures the command to run {@link ExtendedAAFastaEdit} on the given FASTA file.
     *
     * @param component   parent UI component (for error dialogs)
     * @param jarFragpipe path to the FragPipe JAR / install root
     * @param ramGb       JVM max heap size in GB
     * @param fastaPath   absolute path to the input FASTA file
     * @return true if configuration succeeded
     */
    public boolean configure(Component component, Path jarFragpipe, int ramGb, String fastaPath) {
        initPreConfig();

        List<String> cmd = new ArrayList<>();
        cmd.add(Fragpipe.getBinJava());
        cmd.add("-Xmx" + ramGb + "G");
        cmd.add("-cp");
        Path root = FragpipeLocations.get().getDirFragpipeRoot();
        String libsDir = root.resolve("lib").toAbsolutePath().normalize() + "/*";
        if (Files.isDirectory(jarFragpipe)) {
            libsDir = jarFragpipe.toAbsolutePath().getParent().getParent().getParent().getParent()
                .resolve("build/install/fragpipe-" + Version.version() + "/lib")
                .toAbsolutePath().normalize() + "/*";
        }
        cmd.add(libsDir);
        cmd.add(ExtendedAAFastaEdit.class.getCanonicalName());
        cmd.add(fastaPath);

        ProcessBuilder pb = new ProcessBuilder(cmd);
        pbis.add(PbiBuilder.from(pb));

        isConfigured = true;
        return true;
    }
}
