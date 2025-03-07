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
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;


public class FileMoveWithExtension {

    public static void main(String[] args) throws IOException {
        if (args.length != 3) {
            throw new IllegalArgumentException("Input must be 3 arguments: origin dir, extension, and destination.");
        }

        Path originDir = Paths.get(args[0]);
        String ext = args[1].trim();
        Path dest = Paths.get(args[2]);

        List<Path> pp = new ArrayList<>(1);
        Files.walk(originDir).forEach(p -> {
            if (Files.isRegularFile(p) && p.toString().endsWith(ext)) {
                pp.add(p);
            }
        });

        if (pp.size() == 1) {
            Files.move(pp.get(0), dest, StandardCopyOption.REPLACE_EXISTING);
        } else if (pp.size() > 1) {
            System.err.println("Multiple files found in " + originDir + " with extension " + ext + ": " + pp.stream().map(Path::toAbsolutePath).map(Path::toString).collect(Collectors.joining(", ")));
            System.exit(1);
        } else {
            System.err.println("No files found in " + originDir + " with extension " + ext);
            System.exit(1);
        }
    }
}
