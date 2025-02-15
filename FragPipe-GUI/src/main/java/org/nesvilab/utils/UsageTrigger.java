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

package org.nesvilab.utils;

import java.nio.file.Path;
import java.util.HashSet;
import java.util.Set;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Describes the "used" state of some entity. Once {@link #setUsed(boolean)} method was
 * called with 'true' parameter, it can't be reset back.
 */
public class UsageTrigger {
  private static final Logger log = LoggerFactory.getLogger(UsageTrigger.class);
  private final String bin;
  private final Set<Path> workDirs;
  private boolean isUsed = false;
  private final String desc;

  public UsageTrigger(String bin, String desc) {
    if (bin == null) {
      log.warn("Usage trigger given null binary path");
      bin = "";
    }
    this.bin = bin;
    this.desc = desc;
    this.workDirs = new HashSet<>();
  }

  public boolean isUsed() {
    return isUsed;
  }

  public void setUsed(boolean used) {
    if (isUsed)
      return;
    isUsed = used;
  }

  public String getDesc() {
    return desc;
  }

  public String getBin() {
    if (bin.isEmpty()) {
      System.err.println(desc + " binary path is empty. Please check if it exists and executable.");
    }
    return bin;
  }

  public Set<Path> getWorkDirs() {
    return workDirs;
  }

  /**
   * Get the bin path and set the "used" flag to true.
   * @return Path to binary.
   */
  public final String useBin(Path workDir) {
    if (bin.isEmpty()) {
      System.err.println(desc + " binary path is empty. Please check if it exists and executable.");
    }
    isUsed = true;
    if (workDir != null)
      workDirs.add(workDir);
    return bin;
  }

  public final String useBin() {
    return useBin(null);
  }
}
