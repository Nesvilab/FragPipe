/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package umich.msfragger.util;

import umich.msfragger.exceptions.FileWritingException;
import umich.msfragger.params.PropLine;
import umich.msfragger.params.PropertyFileContent;

import java.io.*;
import java.net.URI;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

/**
 *
 * @author dmitriya
 */
public class PropertiesUtils {

    private PropertiesUtils() {
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
            throw new IllegalStateException("Error reading msfragger.properties from the classpath");
        }
    }
    
    /**
     * Downloads a properties file from a remote URL.
     * @param uri To download .properties file from.
     * @return null in case of any errors during downloading or parsing.
     */
    public static Properties loadPropertiesRemote(URI uri) {
        try {
            String remoteText = org.apache.commons.io.IOUtils.toString(uri.toURL(), Charset.forName("UTF-8"));
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

    /**
     * Slurps the whole properties file into a string and replaces occasional 
     * backslashes with double ones. Keeps the backslashes that are allowed at
     * the end of the line in properties files.<br/>
     * You probably don't need this method, use {@link #readProperties(java.io.InputStream) }
     * instead.
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
}
