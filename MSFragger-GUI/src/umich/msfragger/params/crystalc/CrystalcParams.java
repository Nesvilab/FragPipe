/*
 * Copyright 2018 Dmitry Avtonomov.
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
package umich.msfragger.params.crystalc;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Properties;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import umich.msfragger.params.Props;
import umich.msfragger.params.AbstractParams;
import umich.msfragger.util.PathUtils;
import umich.msfragger.util.PropertiesUtils;

/**
 *
 * @author Dmitry Avtonomov
 */
public class CrystalcParams extends AbstractParams {
    
    public static final String PROP_Thread = "thread";
    public static final String PROP_Fasta = "fasta";
    public static final String PROP_RawDataDictionary = "raw_file_location";
    public static final String PROP_OutputFolder = "output_location";
    public static final String PROP_RawFileExtension = "raw_file_extension";
    // precursor charge range for detecting chimeric spectra
    public static final String PROP_MaxZ = "precursor_charge";
    public static final String PROP_IsoNum = "isotope_number";
    // precursor mass tolerance (unit: ppm)
    public static final String PROP_MassTol = "precursor_mass";
    public static final String PROP_PrecursorIsolationWindow = "precursor_isolation_window";
    public static final String PROP_UseAdjustedPrecursors = "UseAdjustedPrecursors";

    public static final String CACHE_FILE = "crystalc.params";

    public static Properties loadProperties() {
        return PropertiesUtils.loadPropertiesLocal(CrystalcParams.class, CACHE_FILE);
    }

    public CrystalcParams() {
        props = new Props();
    }

    @Override
    public Path tempFileName() {
        return Paths.get(CACHE_FILE);
    }

    @Override
    public void loadDefault() {
        try {
            InputStream is = CrystalcParams.class.getResourceAsStream(CACHE_FILE);
            props.load(is);
        } catch (IOException ex) {
            throw new IllegalStateException("Could not load CrystalC deafult properties from jar");
        }
    }

    public int getThread() {
        return getInt(PROP_Thread, "-1");
    }

    public String getFasta() {
        return getString(PROP_Fasta, "");
    }

    public String getRawDirectory() {
        return getString(PROP_RawDataDictionary, "");
    }

    public String getOutputFolder() {
        return getString(PROP_OutputFolder, "");
    }

    public String getRawFileExt() {
        return getString(PROP_RawFileExtension, "mzML");
    }

    public int getMaxZ() {
        String val = getString(PROP_MaxZ, "1 6");
        Pattern re = Pattern.compile("^\\d+$"); // old format, single number
        if (re.matcher(val).matches()) {
            val = "1 " + val;
        }

        Pattern re2 = Pattern.compile("^(\\d+)\\s+(\\d+)$");
        Matcher m = re2.matcher(val);
        if (!m.matches()) {
            throw new IllegalStateException("CrystalC format for charge should be two numbers separated by whitespace.");
        }
        return Integer.parseInt(m.group(2));
    }
    
    public int getIsoNum() {
        return getInt(PROP_IsoNum, "3");
    }
    
    public double getMassTol() {
        return getDouble(PROP_MassTol, "20");
    }
    
    public double getPrecursorIsolationWindow() {
        return getDouble(PROP_PrecursorIsolationWindow, "0.7");
    }
    
    public void setThread(int threads) {
        setInt(PROP_Thread, threads);
    }

    public void setFasta(String fastaPath) {
        setString(PROP_Fasta, fastaPath);
    }

    public void setRawDirectory(String rawDir) {
        setString(PROP_RawDataDictionary, rawDir);
    }
    
    public void setOutputFolder(String out) {
        setString(PROP_OutputFolder, out);
    }

    public void setRawFileExt(String rawFileExt) {
        setString(PROP_RawFileExtension, rawFileExt);
    }

    public void setMaxZ(int z) {
        setString(PROP_MaxZ, String.format("%d %d", 1, z));
    }

    public void setIsoNum(int iso) {
        setInt(PROP_IsoNum, iso);
    }

    public void setMassTol(double tolPpm) {
        setDouble(PROP_MassTol, tolPpm);
    }

    public void setPrecursorIsolationWindow(double window) {
        setDouble(PROP_PrecursorIsolationWindow, window);
    }
}
