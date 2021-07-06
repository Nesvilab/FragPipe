package com.dmtavt.fragpipe.api;

import java.nio.file.Path;
import java.util.List;

public class LcmsFileGroup implements Comparable<LcmsFileGroup> {
    public final String name;
    public final List<InputLcmsFile> lcmsFiles;

    public LcmsFileGroup(String name, List<InputLcmsFile> lcmsFiles) {
        this.name = name;
        this.lcmsFiles = lcmsFiles;
    }

    public Path outputDir(Path workDir) {
        return workDir.resolve(name);
    }

    public boolean equals(Object other) {
        if (other instanceof LcmsFileGroup) {
            return ((LcmsFileGroup) other).name.contentEquals(name);
        } else {
            return false;
        }
    }

    public int compareTo(LcmsFileGroup other) {
        return name.compareTo(other.name);
    }
}
