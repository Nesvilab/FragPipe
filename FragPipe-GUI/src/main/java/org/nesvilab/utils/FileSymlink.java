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

/**
 *
 * @author dmitr
 */
public class FileSymlink {
    public static void main(String[] args) throws IOException {
        if (args.length  != 2)
            throw new IllegalArgumentException("Must provide exactly 2 arguments. The file to create"
                    + " the link to and the symlink file path itself.");
        Path origin = Paths.get(args[0]);
        Path symlink = Paths.get(args[1]);
        Files.createSymbolicLink(origin, symlink);
    }
}
