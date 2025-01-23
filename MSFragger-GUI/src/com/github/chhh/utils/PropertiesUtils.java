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
package com.github.chhh.utils;

import com.dmtavt.fragpipe.exceptions.FileWritingException;
import com.dmtavt.fragpipe.params.PropLine;
import com.dmtavt.fragpipe.params.PropertyFileContent;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.StringReader;
import java.net.URI;
import java.net.URLConnection;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.apache.commons.io.IOUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author dmitriya
 */
public final class PropertiesUtils {
    private static final Logger log = LoggerFactory.getLogger(PropertiesUtils.class);

    private PropertiesUtils() {
    }

    /**
     * Replacement for standard Properties writer, which does not maintain the order of entries
     * anymore.
     */
    public static void storeSorted(Properties props, OutputStream os, String[] comments, boolean escUnicode) throws IOException {
        try (BufferedWriter bw = new BufferedWriter(
            new OutputStreamWriter(os, StandardCharsets.UTF_8))) {
            if (comments != null) {
                for (String comment : comments) {
                    bw.write("# ");
                    bw.write(comment);
                    bw.newLine();
                }
            }
            bw.newLine();

            // Move database.db-path to the top. Other run specific parameters are provided through command line
            final Map<Boolean, List<String>> namesPartition = props.stringPropertyNames().stream().sorted().collect(Collectors.partitioningBy(e -> {
                String k = e.toLowerCase();
                return !k.contains("db-path");
            }));

            final List<String> names = Stream.concat(namesPartition.get(false).stream(), namesPartition.get(true).stream()).collect(Collectors.toList());
            for (String name : names) {
                name = saveConvert(name, true, escUnicode);
                String val = props.getProperty(name, "");

                if (name.contains("db-path")) {
                    val = saveConvert(Paths.get(val).toAbsolutePath().toString(), false, escUnicode); // save absolute path of the fasta file
                    bw.newLine();
                    bw.write("# Please edit the following path to point to the correct location.");
                    bw.newLine();
                    bw.write("# In Windows, please replace single '\\' with '\\\\'");
                    bw.newLine();
                    bw.write(name);
                    bw.write("=");
                    bw.write(val);
                    bw.newLine();
                    bw.newLine();
                } else {
                    val = saveConvert(val, false, escUnicode);
                    bw.write(name);
                    bw.write("=");
                    bw.write(val);
                    bw.newLine();
                }
            }
        }
    }

    private static String saveConvert(String theString,
        boolean escapeSpace,
        boolean escapeUnicode) {
        int len = theString.length();
        int bufLen = len * 2;
        if (bufLen < 0) {
            bufLen = Integer.MAX_VALUE;
        }
        StringBuilder outBuffer = new StringBuilder(bufLen);

        for(int x=0; x<len; x++) {
            char aChar = theString.charAt(x);
            // Handle common case first, selecting largest block that
            // avoids the specials below
            if ((aChar > 61) && (aChar < 127)) {
                if (aChar == '\\') {
                    outBuffer.append('\\'); outBuffer.append('\\');
                    continue;
                }
                outBuffer.append(aChar);
                continue;
            }
            switch(aChar) {
                case ' ':
                    if (x == 0 || escapeSpace)
                        outBuffer.append('\\');
                    outBuffer.append(' ');
                    break;
                case '\t':outBuffer.append('\\'); outBuffer.append('t');
                    break;
                case '\n':outBuffer.append('\\'); outBuffer.append('n');
                    break;
                case '\r':outBuffer.append('\\'); outBuffer.append('r');
                    break;
                case '\f':outBuffer.append('\\'); outBuffer.append('f');
                    break;
                case '=': // Fall through
                case ':': // Fall through
                case '#': // Fall through
                case '!':
                    outBuffer.append('\\'); outBuffer.append(aChar);
                    break;
                default:
                    if (((aChar < 0x0020) || (aChar > 0x007e)) & escapeUnicode ) {
                        outBuffer.append('\\');
                        outBuffer.append('u');
                        outBuffer.append(toHex((aChar >> 12) & 0xF));
                        outBuffer.append(toHex((aChar >>  8) & 0xF));
                        outBuffer.append(toHex((aChar >>  4) & 0xF));
                        outBuffer.append(toHex( aChar        & 0xF));
                    } else {
                        outBuffer.append(aChar);
                    }
            }
        }
        return outBuffer.toString();
    }

    /**
     * Convert a nibble to a hex character
     * @param   nibble  the nibble to convert.
     */
    private static char toHex(int nibble) {
        return hexDigit[(nibble & 0xF)];
    }

    /** A table of hex digits */
    private static final char[] hexDigit = {
        '0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'
    };


    /** New Properties object as a combination of all given ones. */
    public static Properties merge(Properties... properties) {
        Properties merged = new Properties();
        for (Properties property : properties) {
            merged.putAll(property);
        }
        return merged;
    }

    /** Merge a base Properties object with all given ones. Modifies the 1st one. */
    public static Properties merge(Properties base, List<Properties> toMerge) {
        for (Properties merge : toMerge) {
            base.putAll(merge);
        }
        return base;
    }

    public static Properties initProperties(List<String> urls, int timeout) {
        Properties props = null;
        try {
            props = PropertiesUtils.fetchPropertiesFromRemote(urls, timeout);
        } catch (Exception e) {
            log.warn("Could not fetch properties from remote", e);
        }
        if (props == null) {
            log.debug("Did not get properties from any of remote sources");
            props = new Properties();
        }
        return props;
    }

    public static Properties initProperties(String propFileName, Class<?> clazz) {
        Properties props = PropertiesUtils.loadPropertiesLocal(clazz, propFileName);
        if (props == null) {
            throw new IllegalStateException("Could not init properties object");
        }
        return props;
    }

    /**
     * Loads properties from a properties file that sits next to a given class on the classpath.
     * @param clazz Class relative to which to look for.
     * @param propertiesFile Properties file name.
     * @return Properties loaded from the file.
     * @throws IllegalStateException in case of any errors. The only errors that can be
     *  are path mismatches, which should be caught at testing.
     */
    public static Properties loadPropertiesLocal(Class<?> clazz, String propertiesFile) {
        try (InputStream is = clazz.getResourceAsStream(propertiesFile)) {
            if (is == null) {
                throw new IllegalStateException(String.format(
                        "Could not read '%s' from the classpath of '%s'", propertiesFile, clazz.getName()));
            }
            Properties p = new Properties();
            p.load(is);
            return p;
        } catch (IOException e) {
            throw new IllegalStateException("Error reading properties from the classpath");
        }
    }

    /**
     * Loads properties from a properties file that sits next to a given class on the classpath.
     * @param clazz Class relative to which to look for.
     * @param propertiesFile Properties file name.
     * @return Properties loaded from the file or null if the file was not found
     */
    public static Properties loadPropertiesLocal(Class<?> clazz, String propertiesFile, boolean fake) {
        try (InputStream is = clazz.getResourceAsStream(propertiesFile)) {
            if (is == null) {
                log.debug("properties file was not found");
                return null;
            }
            Properties p = new Properties();
            p.load(is);
            return p;
        } catch (IOException e) {
            log.debug("exception reading properties file", e);
            return null;
        }
    }

    private static Properties loadPropertiesRemote(URI uri, int timeout) {
        try {
            URLConnection connection = uri.toURL().openConnection();
            connection.setConnectTimeout(timeout);
            connection.setReadTimeout(timeout);

            String remoteText = IOUtils.toString(connection.getInputStream(), StandardCharsets.UTF_8);

            final Properties p = new Properties();
            p.load(new StringReader(remoteText));

            return p;
        } catch (Exception ex) {
            return null;
        }
    }

    /**
     * Write the content of the property file with possible modifications to a
     * new file, keeping the formatting as close to original as possible.
     *
     * @param pfc modified contents of the file
     * @param out The stream should be connected to a file. The stream will be
     * closed after this call.
     * @throws com.dmtavt.fragpipe.exceptions.FileWritingException
     */
    public static void writePropertiesContent(PropertyFileContent pfc, OutputStream out) throws FileWritingException {
        try (PrintWriter pw = new PrintWriter(new OutputStreamWriter(out, StandardCharsets.UTF_8), true)) {

            Map<Integer, PropLine> mapLines = pfc.getMapLines();
            Properties props = pfc.getProps();
            HashSet<String> propNamesWritten = new HashSet<String>();
            for (Map.Entry<Integer, PropLine> entry : mapLines.entrySet()) {
                int lineNum = entry.getKey();
                PropLine propLine = entry.getValue();
                if (propLine.isSimpleLine()) {
                    pw.println(propLine.getJustALine());
                } else {
                    String propName = propLine.getName();
                    String propValue = props.getProperty(propName);
                    pw.print(propName + " = " + propValue);
                    propNamesWritten.add(propName);
                    if (propLine.getComment() != null) {
                        pw.print("\t\t\t" + propLine.getComment());
                    }
                    pw.println();
                }
            }
            Set<String> stringPropertyNames = props.stringPropertyNames();
            // if there was something else added on top of what was in the file
            // we will append to the end of the file
            for (String propName : stringPropertyNames) {
                if (propNamesWritten.contains(propName)) {
                    continue;
                }
                pw.println(propName + " = " + props.getProperty(propName));
            }
        } finally {
            if (out != null) {
                try {
                    out.close();
                } catch (IOException ex) {
                    throw new FileWritingException("This is strange, error happened while trying to close the output stream.");
                }
            }
        }

    }

    public static Properties from(Path file) throws IOException {
        Properties p = new Properties();
        p.load(Files.newBufferedReader(file));
        return p;
    }

    public static Properties from(Map<String, String> map) {
        Properties p = new Properties();
        for (Entry<String, String> e : map.entrySet()) {
            if (StringUtils.isNullOrWhitespace(e.getKey()))
                continue;
            p.setProperty(e.getKey(), e.getValue());
        }
        return p;
    }

    public static Map<String, String> toMap(Properties props) {
        Set<String> names = props.stringPropertyNames();
        HashMap<String, String> map = new HashMap<>(names.size());
        for (String name : names) {
            map.put(name, props.getProperty(name));
        }
        return map;
    }

    /**
     * Slurps the whole properties file into a string and replaces occasional 
     * backslashes with double ones. Keeps the backslashes that are allowed at
     * the end of the line in properties files.<br/>
     * You probably don't need this method.
     * @param is  InputStream to read from. The stream is closed.
     * @return  A StringReader wrapped around the slurped string representation of the file.
     * @throws IOException 
     */
    public static Reader preparePropertyFile(InputStream is) throws IOException {

        InputStreamReader isr = new InputStreamReader(is);
        BufferedReader reader = new BufferedReader(isr);
        StringBuilder sb = new StringBuilder();
        
        String line;
        boolean endingBackslash = false;

        while ((line = reader.readLine()) != null) {
            line = line.trim();
            if (line.endsWith("\\")) {
                sb.append(line.substring(0, line.length()-1).replace("\\", "\\\\"));
                sb.append(line).append("\\");
            } else {
                sb.append(line.replace("\\", "\\\\"));
            }
            sb.append("\n");
        }
        
        is.close();
        return new StringReader(sb.toString());
    }

    /**
     * Try to load properties from one of URLs given.
     * @return null if properties could not be obtained from any source. Getting empty property file
     * doesn't count, it will be returned as null (and other sources will be tried first).
     */
    public static Properties fetchPropertiesFromRemote(List<String> urls, int timeout) {
        Properties props = null;
        for (String url : urls) {
            try {
                Properties p = loadPropertiesRemote(URI.create(url), timeout);
                if (p == null || p.isEmpty()) {
                    log.debug("Didn't get properties from: {}", url);
                    continue;
                }
                props = p;
                log.debug("Got properties from: {}", url);
                break;
            } catch (Exception ex) {
                log.debug("Failed to get properties from: {}\nReason: {}", url, ex.getMessage());
            }
        }
        return props;
    }
}
