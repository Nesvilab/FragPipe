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
package umich.msfragger.params;

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.nio.charset.Charset;
import java.util.ArrayList;
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
    public static final String COMMENT_SYMBOL = "#";
    private static final Pattern DISABLED_PROP = Pattern.compile("^#\\s*([^\\s]+)\\s*=([^#]+)(?:\\s*#\\s*(.+))?.*");
    private static final Pattern ENABLED_PROP = Pattern.compile("^\\s*([^\\s]+)\\s*=([^#]*)(?:\\s*#\\s*(.+))?.*");
    private LinkedHashMap<String, Prop> map = new LinkedHashMap<>();
    private ArrayList<String> propOrdering = new ArrayList<>();
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
        this.propOrdering.clear();
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
            if (StringUtils.isNullOrWhitespace(line)) {
              propOrdering.add("");
              continue;
            }

            if (line.contains("mass_tol")) {
                    int a = 1;
                }
            
            // a comment line, but also can be a disabled option
            if (line.startsWith(COMMENT_SYMBOL)) {
                Matcher m = DISABLED_PROP.matcher(line);
                if (m.matches()) {
                    String name = m.group(1).trim();
                    String value = m.group(2).trim();
                    if (StringUtils.isNullOrWhitespace(name))
                        continue;
                    if (StringUtils.isNullOrWhitespace(value))
                        value = "";
                    Prop existingProp = map.get(name);
                    if (existingProp != null && existingProp.isEnabled)
                        continue;
                    Prop p = new Prop(name, value, false, m.group(3));
                    map.put(p.name, p);
                    propOrdering.add(p.name);
                } else {
                    propOrdering.add(line);
                }
                continue;
            }

            Matcher m = ENABLED_PROP.matcher(line);
            if (m.matches()) {
                String name = m.group(1).trim();
                String value = m.group(2).trim();
                if (StringUtils.isNullOrWhitespace(name))
                    continue;
                if (StringUtils.isNullOrWhitespace(value))
                        value = "";
                Prop p = new Prop(name, value, true, m.group(3));
                map.put(p.name, p);
                propOrdering.add(p.name);
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
        for (final String name : propOrdering) {
            if (name.isEmpty() || name.startsWith(COMMENT_SYMBOL)) {
                bw.write(name + "\n");
                continue;
            }
            Prop prop = map.get(name);
            if (StringUtils.isNullOrWhitespace(prop.value)) {
              continue;
            }
            if (!prop.isEnabled)
                bw.write(COMMENT_SYMBOL + " ");
            bw.write(name);
            bw.write(" = ");
            bw.write(prop.value);
            if (comments != null && !comments.isEmpty()) {
                String comment = comments.get(name);
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
