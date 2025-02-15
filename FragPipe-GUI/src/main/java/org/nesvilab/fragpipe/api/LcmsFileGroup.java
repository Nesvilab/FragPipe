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

package org.nesvilab.fragpipe.api;

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

    @Override
    public boolean equals(Object other) {
        if (other instanceof LcmsFileGroup) {
            return ((LcmsFileGroup) other).name.contentEquals(name);
        } else {
            return false;
        }
    }

    @Override
    public int compareTo(LcmsFileGroup other) {
        return name.compareTo(other.name);
    }

    @Override
    public int hashCode() {
        return name.hashCode();
    }
}
