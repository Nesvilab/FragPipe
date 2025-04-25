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

package org.nesvilab.fragpipe.util;

import org.nesvilab.utils.PathUtils;

import java.nio.file.Path;

public class BatchRun {
    public String name;
    public String workflow;
    public String manifest;
    public String outputStr;
    public String toolsStr;
    public String fastaStr;
    public int ram;
    public int threads;
    public Path workflowPath;
    public Path manifestPath;
    public Path outputPath;
    public Path toolsPath;
    public Path fastaPath;

    public BatchRun(String name, String workflow, String manifest, String outputStr, String toolsStr, String fastaStr, int ram, int threads) {
        // record the input strings for error logging later (if needed)
        this.name = name;
        this.workflow = workflow;
        this.manifest = manifest;
        this.outputStr = outputStr;
        this.toolsStr = toolsStr;
        this.fastaStr = fastaStr;
        this.ram = ram;
        this.threads = threads;
        workflowPath = PathUtils.existing(workflow);
        manifestPath = PathUtils.existing(manifest);
        outputPath = Path.of(outputStr);
        toolsPath = PathUtils.existing(toolsStr);
        fastaPath = fastaStr.isEmpty() ? null : PathUtils.existing(fastaStr);
    }

    // return tab delimited string
    public String toString() {
        return name + "\t" +
                workflow + "\t" +
                manifest + "\t" +
                outputStr + "\t" +
                toolsStr + "\t" +
                fastaStr + "\t" +
                ram + "\t" +
                threads;
    }
}
