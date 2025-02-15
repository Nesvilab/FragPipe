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

import java.util.Comparator;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;

/**
 * Compares version number strings.
 */
public class VersionComparator implements Comparator<String> {
    private static VersionComparator INSTANCE = new VersionComparator();

    public static int cmp(String v1, String v2) {
        try {
            return INSTANCE.compare(v1, v2);
        } catch (IllegalArgumentException ex) {
            throw new IllegalArgumentException("v1:"+ v1 + "\tv2:" + v2, ex);
        }
    }

    public boolean equals(String o1, String o2) {
        return compare(o1, o2) == 0;
    }

    @Override
    public int compare(String o1, String o2) {
        DefaultArtifactVersion version1 = new DefaultArtifactVersion(o1);
        DefaultArtifactVersion version2 = new DefaultArtifactVersion(o2);
        return version1.compareTo(version2);
    }

}

