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

import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import org.nesvilab.fragpipe.api.DotnetInfo;

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
        List<List<String>> propNames = Arrays.asList(
            Arrays.asList("java.version", "java.vm.version", "java.runtime.version", "java.specification.version", "java.vm.specification.version"),
            Arrays.asList("java.vm.name"),
            Arrays.asList("java.vendor", "java.vm.vendor")
        );
        StringBuilder sb = new StringBuilder("Java Info: ");
        String info = propNames.stream()
            .map(variants -> variants.stream().map(System::getProperty).filter(StringUtils::isNotBlank).findFirst().orElse(""))
            .filter(StringUtils::isNotBlank)
            .collect(Collectors.joining(", "));
        sb.append(StringUtils.isBlank(info) ? "Could not collect info" : info);
        return sb.toString();
    }

    public static String NetCoreInfo() {
        StringBuilder sb = new StringBuilder(".NET Core Info: ");
        try {
            DotnetInfo dotnetInfo = DotnetInfo.fromCommand(DotnetInfo.COMMAND);
            sb.append(dotnetInfo.getVersion());
        } catch (Exception e) {
            sb.append("N/A");
        }
        return sb.toString();
    }

    public static boolean isWindows() {
        return getOsName().toLowerCase().contains("win");
    }

    public static boolean isMac() {
        return getOsName().toLowerCase().contains("mac");
    }

    public static boolean isUnix() {
        final String os = getOsName().toLowerCase();
        return (os.contains("nix") || os.contains("nux") || os.contains("aix"));

    }

    /**
     * OS name. E.g. 'Linux' or 'Windows XP'.
     *
     * @return
     */
    public static String getOsName() {
        return System.getProperty("os.name", "unknown");
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
        double freeMem;
        final com.sun.management.OperatingSystemMXBean operatingSystemMXBean = ((com.sun.management.OperatingSystemMXBean) java.lang.management.ManagementFactory.getOperatingSystemMXBean());
        if (isWindows()) {
            // for Windows, this will get available memory
            freeMem = operatingSystemMXBean.getFreePhysicalMemorySize() / 1024.0 / 1024.0 / 1024.0;
        } else {
            // for linux, getFreePhysicalMemorySize, returns system free memory.
            try (final java.io.InputStream inputStream = new ProcessBuilder("free", "-wg").start().getInputStream()) {
                final String s = new java.util.Scanner(inputStream).useDelimiter("\\A").next();
                freeMem = Integer.parseInt(s.split("\n")[1].split(" +")[7]);
            } catch (IOException | NumberFormatException ex) {
                // return system free memory if we can't get available memory
                freeMem = operatingSystemMXBean.getFreePhysicalMemorySize() / 1024.0 / 1024.0 / 1024.0;
            }
        }
        if (freeMem > 120) {
            return (int) (freeMem * 0.9);
        } else if ((int) freeMem - 2 > 0) {
            return (int) freeMem - 2;
        } else {
            return Math.max(1, (int) Math.ceil(freeMem));
        }
    }

    public static String asSingleArgument(final String s) {
        String p = s.trim();
        if (isWindows() && p.contains(" ") && !p.startsWith("\"") && !p.endsWith("\"")) {
            return "\"" + p + "\"";
        } else {
            return p;
        }
    }
}
