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
