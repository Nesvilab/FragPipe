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


import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CmdCheckJava extends CmdBase {

    public static final String NAME = "CheckJava";
    private static final Logger log = LoggerFactory.getLogger(CmdCheckJava.class);

    public CmdCheckJava(boolean isRun, Path workDir) {
        super(isRun, workDir);
    }

    @Override
    public String getCmdName() {
        return NAME;
    }

    public boolean configure() {
        initPreConfig();

        List<String> cmd = new ArrayList<>();
        cmd.add("java");
        cmd.add("-version");

        ProcessBuilder pb = new ProcessBuilder(cmd);
        pbis.add(PbiBuilder.from(pb));

        isConfigured = true;
        return true;
    }
}
