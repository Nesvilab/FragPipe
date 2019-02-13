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
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.StringReader;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.Set;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import umich.msfragger.exceptions.FileWritingException;
import umich.msfragger.params.PropLine;
import umich.msfragger.params.PropertyFileContent;
import umich.msfragger.params.Props.Prop;
import umich.msfragger.params.fragger.MsfraggerProps;

/**
 *
 * @author dmitriya
 */
public final class PropertiesUtils {
    private static final Logger log = LoggerFactory.getLogger(PropertiesUtils.class);

    private PropertiesUtils() {
    }

    public static Properties initProperties(List<String> urls, String propFileName, Class<?> clazz) {
        Properties props = PropertiesUtils
            .fetchPropertiesFromRemote(urls);
        if (props == null) {
            log.debug("Did not get {} from any of remote sources", propFileName);
            props = PropertiesUtils.loadPropertiesLocal(clazz, propFileName);
        }
        if (props == null) {
            throw new IllegalStateException("Could not init properties object");
        }
        return props;
    }

    public static Properties initProperties(List<String> urls) {
        Properties props = PropertiesUtils
            .fetchPropertiesFromRemote(urls);
        if (props == null) {
            log.debug("Did not get properties from any of remote sources");
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
     * Downloads a properties file from a remote URL.
     * @param uri To download .properties file from.
     * @return null in case of any errors during downloading or parsing.
     */
    public static Properties loadPropertiesRemote(URI uri) {
        try {
            String remoteText = org.apache.commons.io.IOUtils.toString(uri.toURL(), StandardCharsets.UTF_8);
            final Properties p = new Properties();
            p.load(new StringReader(remoteText));
            return p;
        } catch (Exception ex) {
            return null;
        }
        
    }
    
    public static Properties loadPropertiesRemoteOrLocal(List<URI> uris, Class<?> clazz, String propertiesFile) {
        try {
            for (URI uri : uris) {
                Properties props = loadPropertiesRemote(uri);
                if (props != null)
                    return props;
            }
        } catch (Exception e) {
            // doesn't matter
        }
        return loadPropertiesLocal(clazz, propertiesFile);
    }

    /**
     * Write the content of the property file with possible modifications to a
     * new file, keeping the formatting as close to original as possible.
     *
     * @param pfc modified contents of the file
     * @param out The stream should be connected to a file. The stream will be
     * closed after this call.
     * @throws umich.msfragger.exceptions.FileWritingException
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

    public static Map<String, String> to(Properties props) {
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
    public static Properties fetchPropertiesFromRemote(List<String> urls) {
        Properties props = null;
        for (String url : urls) {
            try {
                Properties p = loadPropertiesRemote(URI.create(url));
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
