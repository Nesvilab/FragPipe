package com.github.chhh.utils;

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
    isUsed = true;
    if (workDir != null)
      workDirs.add(workDir);
    return bin;
  }

  public final String useBin() {
    return useBin(null);
  }
}
