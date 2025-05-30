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

import org.nesvilab.fragpipe.FragpipeLocations;
import org.nesvilab.fragpipe.tabs.TabBatch;
import org.nesvilab.fragpipe.util.BatchRun;
import java.awt.Component;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import org.nesvilab.utils.OsUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CmdBatch extends CmdBase {

    private static final Logger log = LoggerFactory.getLogger(CmdBatch.class);
    public static final String NAME = "Batched Run";


    public CmdBatch(boolean isRun, Path workDir) {
        super(isRun, workDir);
    }

    @Override
    public String getCmdName() {
        return NAME;
    }

    public boolean configure(Component comp, BatchRun run) {
        initPreConfig();
        if (!TabBatch.checkPaths(comp, run, true)) {
            return false;
        }

        List<String> cmd = new ArrayList<>();
        cmd.add(FragpipeLocations.get().getFragpipeBin().toAbsolutePath().normalize().toString());
        cmd.add("--headless");
        cmd.add("--workflow");
        cmd.add(String.valueOf(run.workflowPath.toAbsolutePath().normalize()));
        cmd.add("--manifest");
        cmd.add(String.valueOf(run.manifestPath.toAbsolutePath().normalize()));
        cmd.add("--workdir");
        cmd.add(String.valueOf(run.outputPath.toAbsolutePath().normalize()));
        cmd.add("--config-tools-folder");
        cmd.add(String.valueOf(run.toolsPath.toAbsolutePath().normalize()));
        cmd.add("--ram");
        cmd.add(String.valueOf(run.ram));
        cmd.add("--threads");
        cmd.add(String.valueOf(run.threads));

        ProcessBuilder pb = new ProcessBuilder(cmd);
        // use bundled Java if on Windows
        if (OsUtils.isWindows()) {
            Path javaPath = FragpipeLocations.get().getDirFragpipeRoot().resolve("jre");
            String javaHome = javaPath.toAbsolutePath().normalize().toString();
            pb.environment().put("JAVA_HOME", javaHome);
            // update the PATH to have the bundled Java first
            pb.environment().compute("Path", (k, path) -> javaHome + "\\bin;" + path);
        }
        pb.directory(run.outputPath.toFile());
        pbis.add(PbiBuilder.from(pb));

        isConfigured = true;
        return true;
    }
}
