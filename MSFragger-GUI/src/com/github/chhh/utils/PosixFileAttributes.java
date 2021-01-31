package com.github.chhh.utils;

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
