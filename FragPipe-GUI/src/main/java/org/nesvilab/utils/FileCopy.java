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
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.Locale;

/**
 *
 * @author Dmitry Avtonomov
 */
public class FileCopy {
    public static void main(String[] args) throws IOException {
        Locale.setDefault(Locale.US);
        if (args.length != 2) {
            throw new IllegalArgumentException("Input must be exactly 2 arguments: origin and destination");
        }

        Path origin = null;
        Path destination = null;
        try {
            origin = Paths.get(args[0]);
            destination = Paths.get(args[1]);
        } catch (InvalidPathException e) {
            System.err.println("Given paths are not valid: " + e.getMessage());
            System.exit(1);
        }
        if (!Files.exists(origin)) {
            System.err.println("Origin file does not exist: " + origin.toString());
            System.exit(1);
        }
        if (!Files.exists(destination.toAbsolutePath().getParent())) {
            System.err.println("Destination directory does not exist: " + destination.toAbsolutePath().getParent());
            System.exit(1);
        }

        Files.copy(origin, destination, StandardCopyOption.REPLACE_EXISTING);
    }
}
