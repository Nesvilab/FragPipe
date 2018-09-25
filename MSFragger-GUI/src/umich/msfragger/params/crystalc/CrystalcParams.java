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
import umich.msfragger.params.Props;
import umich.msfragger.params.fragger.AbstractParams;
import umich.msfragger.util.PathUtils;
import umich.msfragger.util.PropertiesUtils;

/**
 *
 * @author Dmitry Avtonomov
 */
public class CrystalcParams extends AbstractParams {
    
    public static final String PROP_Thread = "Thread";
    public static final String PROP_Fasta = "Fasta";
    public static final String PROP_RawDataDictionary = "RawDataDictionary";
    public static final String PROP_RawFileExtension = "RawFileExtension";
    public static final String PROP_MaxZ = "MaxZ";
    public static final String PROP_IsoNum = "IsoNum";
    public static final String PROP_MassTol = "IsoNum";
    public static final String PROP_PrecursorIsolationWindow = "IsoNum";
    public static final String PROP_UseAdjustedPrecursors = "UseAdjustedPrecursors";
    
    public static final String DEFAULT_FILE = "crystalc.params";
    
    public static Properties loadProperties() {
        return PropertiesUtils.loadPropertiesLocal(CrystalcParams.class, DEFAULT_FILE);
    }

    public CrystalcParams() {
        props = new Props();
    }

    @Override
    public Path tempFilePath() {
        return Paths.get(PathUtils.getTempDir().toString(), DEFAULT_FILE);
    }

    @Override
    public void loadDefault() {
        try {
            InputStream is = CrystalcParams.class.getResourceAsStream(DEFAULT_FILE);
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
    
    public String getRawFileExt() {
        return getString(PROP_RawFileExtension, "mzML");
    }
    
    public int getMaxZ() {
        return getInt(PROP_MaxZ, "6");
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

    public void setRawFileExt(String rawFileExt) {
        setString(PROP_RawFileExtension, rawFileExt);
    }

    public void setMaxZ(int z) {
        setInt(PROP_MaxZ, z);
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
