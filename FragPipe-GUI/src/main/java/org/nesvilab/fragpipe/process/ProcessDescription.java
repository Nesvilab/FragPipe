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

package org.nesvilab.fragpipe.process;

public class ProcessDescription {
  public final String name;
  public final String workDir;
  public final String command;

  public ProcessDescription(String name, String workDir, String command) {
    this.name = name;
    this.workDir = workDir;
    this.command = command;
  }

  public static class Builder {

    private String name;
    private String workDir;
    private String command;

    public Builder setName(String name) {
      this.name = name;
      return this;
    }

    public Builder setWorkDir(String workDir) {
      this.workDir = workDir;
      return this;
    }

    public Builder setCommand(String command) {
      this.command = command;
      return this;
    }

    public ProcessDescription create() {
      return new ProcessDescription(name, workDir, command);
    }
  }
}
