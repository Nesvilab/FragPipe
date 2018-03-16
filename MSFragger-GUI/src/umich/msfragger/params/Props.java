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
package umich.msfragger.params;

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.nio.charset.Charset;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import umich.msfragger.util.IOUtils;
import umich.msfragger.util.StringUtils;

/**
 *
 * @author Dmitry Avtonomov
 */
public class Props {
    public static final String COMMENT = "#";
    private static final Pattern DISABLED_PROP = Pattern.compile("^#\\s*([^\\s]+)\\s*=\\s*([^#]+)(?:\\s*#\\s*(.+))?.*");
    private static final Pattern ENABLED_PROP = Pattern.compile("^\\s*([^\\s]+)\\s*=\\s*([^#]+)(?:\\s*#\\s*(.+))?.*");
    private LinkedHashMap<String, Prop> map = new LinkedHashMap<>();
    private LinkedHashMap<String, String> comments = new LinkedHashMap<>();

    public Props() {
    }
    
    public Props(Map<String, String> comments) {
        this.comments.putAll(comments);
    }
    
    /**
     * Same as {@link #readProps(java.io.InputStream) }.
     * @param is
     * @throws IOException 
     */
    public void load(InputStream is) throws IOException {
        readProps(is);
    }
    
    /**
     * Same as {@link #writeProps(java.io.OutputStream) }.
     * @param os
     * @throws IOException 
     */
    public void save(OutputStream os) throws IOException {
        writeProps(os);
    }
    
    public void setProp(String name, String value) {
        setProp(name, value, true, null);
    }
    
    public void setProp(String name, String value, boolean isEnabled) {
        setProp(name, value, isEnabled, null);
    }
    
    public void setProp(String name, String value, boolean isEnabled, String comment) {
        if (StringUtils.isNullOrWhitespace(name))
            throw new IllegalArgumentException("Name can't be null or whitespace");
        if (value == null)
            value = "";
        map.put(name, new Prop(name, value, isEnabled, comment));
        if (comment != null && !StringUtils.isNullOrWhitespace(comment)) {
            comments.put(name, comment);
        }
    }
    
    public void removeProp(String name) {
        if (StringUtils.isNullOrWhitespace(name))
            throw new IllegalArgumentException("Name can't be null or whitespace");
        map.remove(name);
    }
    
    public void clearProps() {
        this.map.clear();
        this.comments.clear();
    }
    
    public void clearComments() {
        this.comments.clear();
    }
    
    public Prop getProp(String name) {
        return map.get(name);
    }
    
    public Prop getProp(String name, String defaultValue) {
        return getProp(name, defaultValue, true);
    }
    
    public Prop getProp(String name, String defaultValue, boolean defaultEnabled) {
        Prop v = map.get(name);
        return v != null ? v : new Prop(name, defaultValue, defaultEnabled, comments.get(name));
    }
    
    public String getComment(String name) {
        return comments.get(name);
    }
    
    public class Prop {
        public final String name;
        public final String value;
        public final String comment;
        public final boolean isEnabled;

        public Prop(String name, String value, boolean isEnabled) {
            this.name = name;
            this.value = value;
            this.isEnabled = isEnabled;
            this.comment = null;
        }

        public Prop(String name, String value, boolean isEnabled, String comment) {
            this.name = name;
            this.value = value;
            this.isEnabled = isEnabled;
            this.comment = comment;
        }

        @Override
        public String toString() {
            return "Prop{" + "name=" + name + ", value=" + value + ", comment=" + comment + ", isEnabled=" + isEnabled + '}';
        }
        
        
    }
    
    
    /**
     * Loads properties from a stream. Typically a FileInputStream or an input 
     * stream that you get via Class.getResourceAsStream() method.
     * @param is  Stream is closed after reading.
     * @param properties  
     * @throws IOException 
     */
    private void readProps(InputStream is) throws IOException {
        List<String> allLines = IOUtils.readAllLines(is, Charset.forName("UTF-8"));
        is.close();
        for (String line : allLines) {
            line = line.trim();
            if (StringUtils.isNullOrWhitespace(line))
                continue;
            if (line.startsWith(COMMENT)) {
                
                // can be a disabled option
                Matcher m = DISABLED_PROP.matcher(line);
                if (m.matches()) {
                    String name = m.group(1).trim();
                    String value = m.group(2).trim();
                    if (StringUtils.isNullOrWhitespace(name) || StringUtils.isNullOrWhitespace(value))
                        continue;
                    Prop p = new Prop(name, value, false, m.group(3));
                    map.put(p.name, p);
                }
                continue;
            }
            
            Matcher m = ENABLED_PROP.matcher(line);
            if (m.matches()) {
                String name = m.group(1).trim();
                String value = m.group(2).trim();
                if (StringUtils.isNullOrWhitespace(name) || StringUtils.isNullOrWhitespace(value))
                    continue;
                Prop p = new Prop(name, value, true, m.group(3));
                map.put(p.name, p);
            }
        }
    }
    
    /**
     * Writes to the stream (buffers it), includes comments after each parameter.
     * @param os  The stream is closed after writing.
     * @param properties
     * @param comments
     * @throws IOException 
     */
    private void writeProps(OutputStream os) throws IOException {
        Set<Map.Entry<String, Prop>> entries = map.entrySet();
        BufferedWriter bw = new BufferedWriter(new OutputStreamWriter(os, "UTF-8"));
        for (Map.Entry<String, Prop> e : entries) {
            Prop prop = e.getValue();
            if (!prop.isEnabled)
                bw.write(COMMENT + " ");
            bw.write(e.getKey());
            bw.write(" = ");
            bw.write(prop.value);
            if (comments != null && !comments.isEmpty()) {
                String comment = comments.get(e.getKey());
                if (!StringUtils.isNullOrWhitespace(comment)) {
                    bw.write("\t\t\t# ");
                    bw.write(comment);
                }
            } else if (!StringUtils.isNullOrWhitespace(prop.comment)) {
                bw.write("\t\t\t# ");
                bw.write(prop.comment);
            }
            bw.write("\n");
        }
        bw.write("\n");
        
        bw.flush();
        os.close();
    }
}
