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

import org.nesvilab.utils.CacheUtils;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author Dmitry Avtonomov
 */
public abstract class AbstractParams {
    private static final Logger log = LoggerFactory.getLogger(AbstractParams.class);

    protected Props props;

    public AbstractParams() {
        props = new Props();
    }

    public AbstractParams(AbstractParams other) {
        props = new Props(other.props);
    }

    public abstract Path tempFileName();

    public abstract void loadDefault();

    public Props getProps() {
        return props;
    }

    /**
     * Loads properties either from the default properties file stored in the jar
     * or from the temp directory.
     * @throws IOException
     */
    public void load() throws IOException {
        // first check if there is a temp file saved
        String tempFn = tempFileName().toString();
        try {
            Path path = CacheUtils.locateTempFile(tempFn);
            try (final InputStream is = Files.newInputStream(path)) {
                load(is, true);
            }
        } catch (FileNotFoundException ex) {
            loadDefault();
        }
    }

    /**
     * Clear out the properties
     * @param is
     * @param clearBeforeLoading clear up the internal properties before loading new ones.
     * @throws IOException
     */
    public void load(InputStream is, boolean clearBeforeLoading) throws IOException {
        if (clearBeforeLoading) {
            clear();
        }
        props.load(is);
    }

    public void clear() {
        this.props.clearProps();
    }

    /**
     * Saves the current properties contents to a default temp file.
     * @throws IOException
     */
    public Path save() throws IOException {
        Path temp = CacheUtils.getTempFile(tempFileName().toString());;
        if (Files.exists(temp)) {
            Files.delete(temp);
        }
        props.save(new FileOutputStream(temp.toFile()));
        return temp;
    }

    /**
     * Saves the current properties contents to a stream. With comments.
     * @param os
     * @throws IOException
     */
    public void save(OutputStream os) throws IOException {
        props.save(os);
    }

    protected int getInt(String name, String defaultVal) {
        return Integer.parseInt(props.getProp(name, defaultVal).value);
    }

    protected Integer getInt(String name) {
        Props.Prop prop = props.getProp(name);
        return prop == null ? null : Integer.parseInt(prop.value);
    }

    protected double getDouble(String name, String defaultVal) {
        return Double.parseDouble(props.getProp(name, defaultVal).value);
    }

    protected Double getDouble(String name) {
        Props.Prop prop = props.getProp(name);
        return prop == null ? null : Double.parseDouble(prop.value);
    }

    protected boolean getBoolean(String name, String defaultVal) {
        return Boolean.parseBoolean(props.getProp(name, defaultVal).value);
    }

    protected Boolean getBoolean(String name) {
        Props.Prop prop = props.getProp(name);
        return prop == null ? null : Boolean.parseBoolean(prop.value);
    }

    protected String getString(String name, String defaultVal) {
        return props.getProp(name, defaultVal).value;
    }

    protected String getString(String name) {
        Props.Prop prop = props.getProp(name);
        return prop == null ? null : prop.value;
    }

    public void setInt(String name, int val) {
        props.setProp(name, Integer.toString(val));
    }

    public void setInt(String name, String val) {
        Integer.parseInt(val);
        props.setProp(name, val);
    }

    public void setInt(String name, Integer val) {
        if (val == null)
            props.removeProp(name);
        else
            props.setProp(name, val.toString());
    }

    public void setDouble(String name, double val) {
        props.setProp(name, Double.toString(val));
    }

    public void setDouble(String name, String val) {
        Double.parseDouble(val);
        props.setProp(name, val);
    }

    public void setDouble(String name, Double val) {
        if (val == null)
            props.removeProp(name);
        else
            props.setProp(name, val.toString());
    }

    public void setString(String name, String val) {
        if (val == null)
            props.removeProp(name);
        else
            props.setProp(name, val);
    }

    public void setBool(String name, boolean val) {
        props.setProp(name, Boolean.toString(val));
    }

    public void setBool(String name, Boolean val) {
        if (val == null)
            props.removeProp(name);
        else
            props.setProp(name, Boolean.toString(val));
    }

    public void setBool(String name, String val) {
        Boolean.parseBoolean(val);
        props.setProp(name, val);
    }
}
