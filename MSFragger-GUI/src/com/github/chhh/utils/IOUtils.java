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

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Base64;
import java.util.List;
import okio.BufferedSource;
import okio.ByteString;
import okio.Okio;
import okio.Source;

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
        return readAllLines(is, StandardCharsets.UTF_8);
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

    /** Read the object from Base64 string. */
    private static Object fromString( String s ) throws IOException , ClassNotFoundException {
        byte [] data = Base64.getDecoder().decode( s );
        ObjectInputStream ois = new ObjectInputStream(
            new ByteArrayInputStream(  data ) );
        Object o  = ois.readObject();
        ois.close();
        return o;
    }

    /** Write the object to a Base64 string. */
    private static String toString( Serializable o ) throws IOException {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        ObjectOutputStream oos = new ObjectOutputStream( baos );
        oos.writeObject( o );
        oos.close();
        return Base64.getEncoder().encodeToString(baos.toByteArray());
    }

    public static void tokenize(Path path, String start) throws IOException {
        if (!Files.exists(path))
            throw new FileNotFoundException(path.toString());

        ByteString bs = new ByteString(start.getBytes());

        try (Source src = Okio.source(path); BufferedSource buf = Okio.buffer(src)) {

        }
    }
}
