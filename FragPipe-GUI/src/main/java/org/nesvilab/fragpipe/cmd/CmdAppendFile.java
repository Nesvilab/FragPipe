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
import java.util.ArrayList;
import java.util.List;
import org.nesvilab.fragpipe.Fragpipe;
import org.nesvilab.fragpipe.FragpipeLocations;
import org.nesvilab.fragpipe.Version;
import org.nesvilab.fragpipe.util.AppendToFile;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CmdAppendFile extends CmdBase {

    public static final String NAME = "AppendFile";
    private final Path workdir;
    private static final Logger log = LoggerFactory.getLogger(AppendToFile.class);

    public CmdAppendFile(boolean isRun, Path workDir) {
        super(isRun, workDir);
        workdir = workDir;
    }

    @Override
    public String getCmdName() {
        return NAME;
    }

    public boolean configure(Component component, Path jarFragpipe, String fileAppendTo, String fileAppendFrom) {
        initPreConfig();

        List<String> cmd = new ArrayList<>();
        cmd.add(Fragpipe.getBinJava());
        cmd.add("-cp");
        Path root = FragpipeLocations.get().getDirFragpipeRoot();
        String libsDir = root.resolve("lib") + "/*";
        if (Files.isDirectory(jarFragpipe)) {
            libsDir = jarFragpipe.toAbsolutePath().getParent().getParent().getParent().getParent().resolve("build/install/fragpipe-" + Version.version() + "/lib").toString() + "/*";
        }
        cmd.add(libsDir);
        cmd.add(AppendToFile.class.getCanonicalName());
        cmd.add(workdir.resolve(fileAppendTo).toString());
        cmd.add(workdir.resolve(fileAppendFrom).toString());
        ProcessBuilder pb = new ProcessBuilder(cmd);
        pbis.add(PbiBuilder.from(pb));

        isConfigured = true;
        return true;
    }
}
