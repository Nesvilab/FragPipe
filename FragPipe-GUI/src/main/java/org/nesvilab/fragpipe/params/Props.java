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
package org.nesvilab.fragpipe.params;

import org.nesvilab.fragpipe.tools.fragger.MsfraggerParams;
import org.nesvilab.utils.StringUtils;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.nesvilab.utils.IOUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author Dmitry Avtonomov
 */
public class Props {
    private static final Logger log = LoggerFactory.getLogger(Props.class);
    public static final String COMMENT_SYMBOL = "#";
    public static final String BLANK_LINE_MARKER = COMMENT_SYMBOL + " blank line ";
    private static final Pattern DISABLED_PROP = Pattern.compile("^#\\s*([^\\s]+)\\s*=([^#]+)(?:\\s*#\\s*(.+))?.*");
    private static final Pattern ENABLED_PROP = Pattern.compile("^\\s*([^\\s]+)\\s*=([^#]*)(?:\\s*#\\s*(.+))?.*");
    private final LinkedHashMap<String, Prop> map = new LinkedHashMap<>();
    private final ArrayList<String> propOrdering = new ArrayList<>();
    private final LinkedHashMap<String, String> comments = new LinkedHashMap<>();

    public Props() {
    }
    
    public Props(Map<String, String> comments) {
        this.comments.putAll(comments);
    }

    public Props(Props other) {
        map.putAll(other.map);
        propOrdering.addAll(other.propOrdering);
        comments.putAll(other.comments);
    }

    public Map<String, Prop> getMap() {
        return map;
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

    public void setOrdering(List<String> order) {
        this.propOrdering.clear();
        this.propOrdering.addAll(new LinkedHashSet<>(order));
    }
    
    /**
     * Loads properties from a stream. Typically a FileInputStream or an input 
     * stream that you get via Class.getResourceAsStream() method.
     * @param is  Stream is closed after reading.
     * @throws IOException 
     */
    private void readProps(InputStream is) throws IOException {
        List<String> allLines = IOUtils.readAllLines(is, Charset.forName("UTF-8"));
        is.close();
        int cnt = 0;
        for (String line : allLines) {
            line = line.trim();
            if (StringUtils.isNullOrWhitespace(line)) {
              propOrdering.add(BLANK_LINE_MARKER + (cnt++));
              continue;
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
                if (name.contentEquals("search_enzyme_cutafter")) {
                    name = MsfraggerParams.PROP_search_enzyme_cut_1;
                    Prop p = new Prop(MsfraggerParams.PROP_search_enzyme_cut_2, "", true, "");
                    map.put(p.name, p);
                    propOrdering.add(p.name);

                    Prop p2 = new Prop(MsfraggerParams.PROP_search_enzyme_sense_1, "C", true, "");
                    map.put(p2.name, p2);
                    propOrdering.add(p2.name);

                    Prop p3 = new Prop(MsfraggerParams.PROP_search_enzyme_sense_2, "C", true, "");
                    map.put(p3.name, p3);
                    propOrdering.add(p3.name);
                }
                if (name.contentEquals("search_enzyme_butnotafter")) {
                    name = MsfraggerParams.PROP_search_enzyme_nocut_1;
                    Prop p = new Prop(MsfraggerParams.PROP_search_enzyme_nocut_2, "", true, "");
                    map.put(p.name, p);
                    propOrdering.add(p.name);
                }
                if (name.contentEquals("search_enzyme_name")) {
                    name = MsfraggerParams.PROP_search_enzyme_name_1;
                    Prop p = new Prop(MsfraggerParams.PROP_search_enzyme_name_2, "", true, "");
                    map.put(p.name, p);
                    propOrdering.add(p.name);
                }
                if (name.contentEquals("allowed_missed_cleavage")) {
                    name = MsfraggerParams.PROP_allowed_missed_cleavage_1;
                    Prop p = new Prop(MsfraggerParams.PROP_allowed_missed_cleavage_2, "2", true, "");
                    map.put(p.name, p);
                    propOrdering.add(p.name);
                }
                Prop p = new Prop(name, value, true, m.group(3));
                map.put(p.name, p);
                propOrdering.add(p.name);
            }
        }
    }

    private void writeProps(Writer w) throws IOException {
        LinkedHashSet<String> orederedKeys = new LinkedHashSet<>(propOrdering);
        orederedKeys.addAll(map.keySet());

        for (final String name : orederedKeys) {
            if (name.isEmpty() || name.startsWith(COMMENT_SYMBOL)) {
                w.write((name.startsWith(BLANK_LINE_MARKER) ? "" : name) + "\n");
                continue;
            }
            Prop prop = map.get(name);
            if (prop.value == null) {
                log.warn("Property with null value encountered: {}", prop.name);
                continue;
            }
            if (!prop.isEnabled)
                w.write(COMMENT_SYMBOL + " ");
            w.write(name);
            w.write(" = ");
            w.write(prop.value);
            if (comments != null && !comments.isEmpty()) {
                String comment = comments.get(name);
                if (!StringUtils.isNullOrWhitespace(comment)) {
                    w.write("\t\t\t# ");
                    w.write(comment);
                }
            } else if (!StringUtils.isNullOrWhitespace(prop.comment)) {
                w.write("\t\t\t# ");
                w.write(prop.comment);
            }
            w.write("\n");
        }
        w.write("\n");
        w.flush();
    }

    /**
     * Writes to the stream (buffers it), includes comments after each parameter.
     * @param os  The stream is closed after writing.
     * @throws IOException 
     */
    private void writeProps(OutputStream os) throws IOException {
        try (BufferedWriter bw = new BufferedWriter(new OutputStreamWriter(os, "UTF-8"))) {
            writeProps(bw);
        }
        os.close();
    }
}
