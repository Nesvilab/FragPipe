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

import java.nio.file.attribute.PosixFilePermission;
import java.util.HashSet;
import java.util.Set;

public class PosixFileAttributes {
    static class UnixConstants {
        static final int S_IAMB = 511;
        static final int S_IRUSR = 256;
        static final int S_IWUSR = 128;
        static final int S_IXUSR = 64;
        static final int S_IRGRP = 32;
        static final int S_IWGRP = 16;
        static final int S_IXGRP = 8;
        static final int S_IROTH = 4;
        static final int S_IWOTH = 2;
        static final int S_IXOTH = 1;

        private UnixConstants() {
        }
    }

    static public Set<PosixFilePermission> permissions(final int st_mode) {
        int bits = (st_mode & UnixConstants.S_IAMB);
        HashSet<PosixFilePermission> perms = new HashSet<>();

        if ((bits & UnixConstants.S_IRUSR) > 0)
            perms.add(PosixFilePermission.OWNER_READ);
        if ((bits & UnixConstants.S_IWUSR) > 0)
            perms.add(PosixFilePermission.OWNER_WRITE);
        if ((bits & UnixConstants.S_IXUSR) > 0)
            perms.add(PosixFilePermission.OWNER_EXECUTE);

        if ((bits & UnixConstants.S_IRGRP) > 0)
            perms.add(PosixFilePermission.GROUP_READ);
        if ((bits & UnixConstants.S_IWGRP) > 0)
            perms.add(PosixFilePermission.GROUP_WRITE);
        if ((bits & UnixConstants.S_IXGRP) > 0)
            perms.add(PosixFilePermission.GROUP_EXECUTE);

        if ((bits & UnixConstants.S_IROTH) > 0)
            perms.add(PosixFilePermission.OTHERS_READ);
        if ((bits & UnixConstants.S_IWOTH) > 0)
            perms.add(PosixFilePermission.OTHERS_WRITE);
        if ((bits & UnixConstants.S_IXOTH) > 0)
            perms.add(PosixFilePermission.OTHERS_EXECUTE);
        return perms;
    }

}
