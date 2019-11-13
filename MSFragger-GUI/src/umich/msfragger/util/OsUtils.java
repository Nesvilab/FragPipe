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
package umich.msfragger.util;

import java.util.List;
import java.util.Arrays;

/**
 *
 * @author Dmitry Avtonomov
 */
public class OsUtils {

    private OsUtils() {
    }

    public static String OsInfo() {
        String os = OsUtils.getOsName();
        os = StringUtils.isNullOrWhitespace(os) ? "?" : os;
        String arch = OsUtils.getSystemArch();
        arch = StringUtils.isNullOrWhitespace(arch) ? "?" : arch;
        return "System OS: " + os + ", Architecture: " + arch;
    }

    public static String JavaInfo() {
        List<String> propNames = Arrays.asList(
                "java.version",
                "java.vm.name",
                "java.vm.vendor"
        );
        StringBuilder sb = new StringBuilder("Java Info: ");
        for (int i = 0; i < propNames.size(); i++) {
            String p = propNames.get(i);
            String val = System.getProperty(p);
            if (!StringUtils.isNullOrWhitespace(val)) {
                sb.append(val);
            }
            if (i < propNames.size() - 1) {
                sb.append(", ");
            }
        }

        return sb.toString();
    }

    public static boolean isWindows() {
        String osName = System.getProperty("os.name");
        if (osName == null) {
            return true; // just the default
        }
        return osName.toLowerCase().startsWith("win");
    }

    /**
     * OS name. E.g. 'Linux' or 'Windows XP'.
     *
     * @return
     */
    public static String getOsName() {
        String osName = System.getProperty("os.name");
        return osName;
    }

    /**
     * Tries to determine processor architecture from system properties.
     *
     * @return null if could not determine.
     */
    public static String getSystemArch() {

        try {
            if (isWindows()) {
                String wow64 = System.getenv("PROCESSOR_ARCHITEW6432");
                if (wow64 != null) {
                    return wow64;
                }
                String arch = System.getenv("PROCESSOR_ARCHITECTURE");
                if (arch != null) {
                    return arch;
                }

            } else {
                String osArch = System.getProperty("os.arch");
                if (osArch != null) {
                    return osArch;
                }
            }
        } catch (IllegalArgumentException e) {
            // could not map os.arch or whatever to our enum values, not a biggie
        }

        // if all else fails, at least try to get the bitness of the JRE
        // which might still be null
        String osArch = System.getProperty("os.arch");
        return osArch;
    }

    /**
     * Calculate a reasonable size for JVM's memory allocation pool (-Xmx), to be used when a user does not
     * set the size
     *
     * @return recommended size in Gb for JVM's -Xmx option
     */
    public static int getDefaultXmx() {
        final com.sun.management.OperatingSystemMXBean operatingSystemMXBean = ((com.sun.management.OperatingSystemMXBean) java.lang.management.ManagementFactory
            .getOperatingSystemMXBean());
        // Java has no builtin method to get system available memory, system free memory can be close to zero when system available memory is high.
        // If gave an arbitrary value, it would have a high chance of exceeding the actual available memory, which would crash the program because GC cannot work appropriately.
        // There is nothing we can do. Just give 0 and let users troubleshoot their computers.
        return (int) (operatingSystemMXBean.getFreePhysicalMemorySize() / 1024.0 / 1024.0 / 1024.0);
    }
}
