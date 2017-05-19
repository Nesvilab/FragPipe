/*
 * Copyright 2017 Dmitry Avtonomov.
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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author Dmitry Avtonomov
 */
public class IOUtils {
    private IOUtils() {}
    
    /**
     * Like {@link Files#readAllLines(java.nio.file.Path, java.nio.charset.Charset) } method,
     * but works with streams.
     * @param is  The stream is closed after reading. It's up to the user to make
     * sure it's not an endless stream.
     * @return 
     * @throws java.io.IOException 
     */
    public static List<String> readAllLines(InputStream is) throws IOException {
        return readAllLines(is, Charset.forName("UTF-8"));
    }
    
    /**
     * Like {@link Files#readAllLines(java.nio.file.Path, java.nio.charset.Charset) } method,
     * but works with streams.
     * @param is  The stream is closed after reading. It's up to the user to make
     * sure it's not an endless stream.
     * @param cs  Charset for decoder.
     * @return 
     * @throws java.io.IOException 
     */
    public static List<String> readAllLines(InputStream is, Charset cs) throws IOException {
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(is, cs))) {
            List<String> result = new ArrayList<>();
            for (;;) {
                String line = reader.readLine();
                if (line == null)
                    break;
                result.add(line);
            }
            return result;
        }
    }
    
}
