/*
 * Copyright 2016 Dmitry Avtonomov.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package umich.msfragger.util;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;

/**
 * Usage: <code>java -cp path-to-jar umich.msfragger.util.FileMove path-from path-to</code>.<br/>
 * To independent from system's copy/move commands, we have this convenience class.
 *
 * @author Dmitry Avtonomov
 */
public class FileMove {

    public static void main(String[] args) throws IOException {
        if (args.length != 2) {
            throw new IllegalArgumentException("Input must be exactly 2 arguments: origin and destination");
        }
        Path origin = Paths.get(args[0]);
        Path destination = Paths.get(args[1]);
        if (!Files.exists(origin)) {
            System.err.printf("File does not exist: %s", origin.toString());
            System.exit(1);
        }
        Files.move(origin, destination, StandardCopyOption.REPLACE_EXISTING);
    }


}
