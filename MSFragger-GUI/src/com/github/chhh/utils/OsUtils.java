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
package com.github.chhh.utils;

import java.io.IOException;
import java.util.List;
import java.util.Arrays;
import java.util.stream.Collectors;
import org.jsoup.helper.StringUtil;

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
        if (isWindows()) {
            // for Windows, this will get available memory
            return (int) (operatingSystemMXBean.getFreePhysicalMemorySize() / 1024.0 / 1024.0 / 1024.0);
        } else {
            // for linux, getFreePhysicalMemorySize, returns system free memory.
            final int availMem;
            try (final java.io.InputStream inputStream = new ProcessBuilder("free", "-wg").start().getInputStream()) {
                final String s = new java.util.Scanner(inputStream).useDelimiter("\\A").next();
                availMem = Integer.parseInt(s.split("\n")[1].split(" +")[7]);
            } catch (IOException | NumberFormatException ex) {
                // return system free memory if we can't get available memory
                return (int) (operatingSystemMXBean.getFreePhysicalMemorySize() / 1024.0 / 1024.0 / 1024.0);
            }
            return availMem - 3; // Leave a few GB to make sure that the value for -Xmx is always smaller than the available memory.
        }
    }
}
