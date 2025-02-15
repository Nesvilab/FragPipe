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

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class ProcessBuildersDescriptor {
  public final List<ProcessBuilderInfo> pbis;
  public final String name;
  public final String fnStdout;
  public final String fnStderr;
  private String parallelGroup = null;

  public ProcessBuildersDescriptor(String name, String fnStdout, String fnStderr) {
    this.name = name;
    this.pbis = new ArrayList<>();
    this.fnStdout = fnStdout;
    this.fnStderr = fnStderr;
  }

  public String getParallelGroup() {
    return parallelGroup;
  }

  public void setParallelGroup(String parallelGroup) {
    this.parallelGroup = parallelGroup;
  }

  public int size() {
    return pbis.size();
  }

  public boolean isEmpty() {
    return pbis.isEmpty();
  }

  public ProcessBuildersDescriptor add(ProcessBuilderInfo pbi) {
    pbis.add(pbi);
    return this;
  }

  public ProcessBuildersDescriptor addAll(Collection<? extends ProcessBuilderInfo> c) {
    pbis.addAll(c);
    return this;
  }


}
