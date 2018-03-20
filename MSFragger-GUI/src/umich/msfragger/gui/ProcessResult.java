/* 
 * Copyright (C) 2018 Dmitry Avtonomov
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package umich.msfragger.gui;

import java.nio.file.Path;

/**
 *
 * @author Dmitry Avtonomov
 */
class ProcessResult {
    private ProcessBuilder processBuilder;

    public ProcessResult(ProcessBuilder processBuilder) {
        this.processBuilder = processBuilder;
    }
    private boolean started;
    private Path workingDir;
    private StringBuilder output = new StringBuilder();
    private Integer exitCode;

    public Path getWorkingDir() {
        return workingDir;
    }

    public void setWorkingDir(Path workingDir) {
        this.workingDir = workingDir;
    }

    public ProcessBuilder getProcessBuilder() {
        return processBuilder;
    }

    public boolean isStarted() {
        return started;
    }

    public void setStarted(boolean started) {
        this.started = started;
    }

    public StringBuilder getOutput() {
        return output;
    }

    public void setOutput(StringBuilder output) {
        this.output = output;
    }

    public Integer getExitCode() {
        return exitCode;
    }

    public void setExitCode(Integer exitCode) {
        this.exitCode = exitCode;
    }
    
    
}
