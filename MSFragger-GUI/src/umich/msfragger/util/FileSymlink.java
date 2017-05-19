/*
 * Copyright 2016 dmitr.
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
