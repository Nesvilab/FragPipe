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
import java.util.Locale;
import org.apache.commons.io.FileUtils;

/**
 * Usage: <code>java -cp path-to-jar org.nesvilab.utils.FileMove path-from path-to</code>.<br/>
 * To independent from system's copy/move commands, we have this convenience class.
 *
 * @author Dmitry Avtonomov
 */
public class FileMove {
    public static final String NO_ERR = "--no-err";
    /**
     *
     * @param args Two args: {@code from}, {@code to}
     * @throws IOException
     */
    public static void main(String[] args) throws IOException {
        Locale.setDefault(Locale.US);
        if (args.length != 2 && args.length != 3) {
            throw new IllegalArgumentException("Input must be either 2 arguments: origin and destination "
                + "or optionally prepended with --no-err ot suppress file existence checks.");
        }
        boolean noErrors = NO_ERR.equals(args[0]);
        int ptr = 0;
        if (noErrors)
            ptr++;

        Path origin = Paths.get(args[ptr++]);
        Path destination = Paths.get(args[ptr++]);

        if (!Files.exists(origin)) {
            if (noErrors) {
                System.exit(0);
            } else {
                System.err.printf("File does not exist: %s", origin);
                System.exit(1);
            }
        }

        try {
            if (Files.isDirectory(origin))
                FileUtils.moveDirectory(origin.toFile(), destination.toFile());
            else
                Files.move(origin, destination, StandardCopyOption.REPLACE_EXISTING);
        } catch (Exception e) {
            e.printStackTrace();
            System.exit(1);
        }
    }


}
