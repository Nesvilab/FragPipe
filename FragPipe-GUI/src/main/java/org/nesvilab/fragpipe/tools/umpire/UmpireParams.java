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
package org.nesvilab.fragpipe.tools.umpire;

import org.nesvilab.fragpipe.params.PropLine;
import org.nesvilab.fragpipe.params.PropertyFileContent;
import org.nesvilab.utils.CacheUtils;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 *
 * @author dattam
 */
public class UmpireParams implements PropertyFileContent {
    public static final String PROP_Thread = "Thread";
    public static final String PROP_RPmax = "RPmax";
    public static final String PROP_RFmax = "RFmax";
    public static final String PROP_CorrThreshold = "CorrThreshold";
    public static final String PROP_DeltaApex = "DeltaApex";
    public static final String PROP_RTOverlap = "RTOverlap";
    public static final String PROP_AdjustFragIntensity = "AdjustFragIntensity";
    public static final String PROP_BoostComplementaryIon = "BoostComplementaryIon";
    
    public static final String PROP_MS1PPM = "SE.MS1PPM";
    public static final String PROP_MS2PPM = "SE.MS2PPM";
    public static final String PROP_MinMSIntensity = "SE.MinMSIntensity";
    public static final String PROP_MinMSMSIntensity = "SE.MinMSMSIntensity";
    public static final String PROP_NoMissedScan = "SE.NoMissedScan";
    public static final String PROP_EstimateBG = "SE.EstimateBG";
    public static final String PROP_IsoPattern = "SE.IsoPattern";
    public static final String PROP_MassDefectFilter = "SE.MassDefectFilter";
    public static final String PROP_MassDefectOffset = "SE.MassDefectOffset";
    public static final String PROP_ExportPrecursorPeak = "ExportPrecursorPeak";
    public static final String PROP_Q1 = "Q1";
    public static final String PROP_Q2 = "Q2";
    public static final String PROP_Q3 = "Q3";
    public static final String PROP_SN = "SE.SN";
    public static final String PROP_MS2SN = "SE.MS2SN";
    public static final String PROP_WindowType = "WindowType";
    public static final String PROP_WindowSize = "WindowSize";

    public static final String ETC_PARAM_RAM = "umpire-se.jvm.ram";

    public static final String FILE_BASE_NAME = "umpire-se";
    public static final String FILE_BASE_EXT = "params";
    /** This file is in the jar, use getResourceAsStream() to get it.  */
    public static final String CACHE_FILE = "diaumpire_se.params";
    public static final String UMPIRESE_VERSION = "2.3.3";
    public static final String JAR_UMPIRESE_NAME = "DIA_Umpire_SE-" + UMPIRESE_VERSION + ".jar";
    
    Properties props = new Properties();
    protected List<String> linesInOriginalFile = new ArrayList<>();
    protected Map<Integer, PropLine> mapLines= new TreeMap<>();
    protected Map<String, Integer> mapProps = new HashMap<>();

    
    public void loadDefault() throws IOException {
        InputStream is = UmpireParams.class.getResourceAsStream(CACHE_FILE);
        this.load(is);
    }

    public boolean loadCache() throws IOException {
        Path cached = getCachePath();
        if (!Files.exists(cached))
            return false;
        try (InputStream is = Files.newInputStream(cached, StandardOpenOption.READ)) {
            load(is);
        }
        return true;
    }

    public void saveCache() throws IOException {
        Path cached = getCachePath();
        try (OutputStream os = Files.newOutputStream(cached)) {
            write(os);
        }
    }

    public void deleteCache() throws IOException {
        Path cached = getCachePath();
        Files.deleteIfExists(cached);
    }

    public Path getCachePath() {
        return CacheUtils.getTempFile(CACHE_FILE);
    }

    public UmpireParams() {
    }

    public Integer getRpMax() {
        String property = props.getProperty(PROP_RPmax);
        return Integer.parseInt(property);
    }
    
    public Integer getRfMax() {
        String property = props.getProperty(PROP_RFmax);
        return Integer.parseInt(property);
    }
    
    public Double getCorrThreshold() {
        String property = props.getProperty(PROP_CorrThreshold);
        return Double.parseDouble(property);
    }
    
    public Double getDeltaApex() {
        String property = props.getProperty(PROP_DeltaApex);
        return Double.parseDouble(property);
    }
    
    public Double getRTOverlap() {
        String property = props.getProperty(PROP_RTOverlap);
        return Double.parseDouble(property);
    }
    
    public Boolean getAdjustFragIntensity() {
        String property = props.getProperty(PROP_AdjustFragIntensity);
        return Boolean.valueOf(property);
    }
    
    public Boolean getBoostComplementaryIon() {
        String property = props.getProperty(PROP_BoostComplementaryIon);
        return Boolean.valueOf(property);
    }

    @Override
    public Properties getProps() {
        return props;
    }

    @Override
    public List<String> getLinesInOriginalFile() {
        return linesInOriginalFile;
    }

    @Override
    public Map<Integer, PropLine> getMapLines() {
        return mapLines;
    }

    @Override
    public Map<String, Integer> getMapProps() {
        return mapProps;
    }
    
    
    
    public void load(InputStream is) throws IOException {

        Pattern propRegex = Pattern.compile("^\\s*([^=]+?)\\s*=\\s*(.+?)\\s*", Pattern.CASE_INSENSITIVE);
        try (BufferedReader br = new BufferedReader(new InputStreamReader(is, StandardCharsets.UTF_8))) {
            int lineNum = 0;
            String line;
            while ((line = br.readLine()) != null) {
                lineNum++;
                this.linesInOriginalFile.add(line);
                int indexOfHash = line.indexOf('#');
                if (indexOfHash >= 0) {
                    if (indexOfHash > 2) {
                        // if it's over 2, then there is a possibility for having a
                        // property before it. A property needs at least one char for
                        // name, then '=' sign and at least one char for value
                        int indexOfEquals = line.substring(0, indexOfHash).indexOf('=');
                        if (indexOfEquals > 2) {
                            // ok, this might be a property
                            String possiblePropString = line.substring(0, indexOfHash);
                            this.addString(propRegex, possiblePropString, line, indexOfHash, lineNum);
                        }
                    } else {
                        // index of hash is small and does not allow for a property, must be a simple string
                        this.mapLines.put(lineNum, new PropLine(line, null, null, null));
                    }
                } else {
                    // it must be a pure property string or just a meaningless string
                    int indexOfEquals = line.indexOf('=');
                    if (indexOfEquals > 0) {
                        addString(propRegex, line, line, indexOfHash, lineNum);
                    } else {
                        // it's a simple line, just add it
                        this.mapLines.put(lineNum, new PropLine(line, null, null, null));
                    }
                }
            }

        }
    }
    
    private void addString(Pattern propRegex, String possiblePropString, String line, int indexOfHash, int lineNum) throws IOException {
        Matcher matcher = propRegex.matcher(possiblePropString);
        if (matcher.matches()) {
            String comment = null;
            if (indexOfHash >= 0)
                comment = line.substring(indexOfHash);
            String propName = matcher.group(1);
            String propVal =  matcher.group(2);
            if (propName.isEmpty())
                throw new IOException(String.format(Locale.ROOT, "Property on line number %d had empty name", lineNum));
            if (propVal.isEmpty())
                throw new IOException(String.format(Locale.ROOT, "Property on line number %d had empty value", lineNum));
            this.mapLines.put(lineNum, new PropLine(null, propName, propVal, comment));
            this.mapProps.put(propName, lineNum);
            this.props.put(propName, propVal);
        } else {
            // we didn't find the property here, it's just a line
            this.mapLines.put(lineNum, new PropLine(line, null, null, null));
        }
    }

    /**
     * This returns the paths to files to be created. Might be symlinks or actual file copies.
     * It does not create the files!
     */
    public static List<Path> getLcmsFilePathsInWorkdir(Path workDir, List<Path> lcmsFilePaths) {
        List<Path> result = new ArrayList<>();
        for (Path lcmsFilePath : lcmsFilePaths) {
            result.add(workDir.resolve(lcmsFilePath.getFileName()));
        }
        return result;
    }

    public static void createFileSymlinks(Path dest, List<Path> paths) throws IOException {
//        if (!Files.isDirectory(dest))
//            throw new IllegalArgumentException("Destinatino must be a directory");

        List<Path> links = UmpireParams.getLcmsFilePathsInWorkdir(dest, paths);
        for (int i = 0; i < paths.size(); i++) {
            Path lcmsPath = paths.get(i);
            Path link = links.get(i);
            if (link.equals(lcmsPath))
                return;
            if (Files.exists(link)) {
                // if that link already exists we need to make sure it points to
                // the same file
                if (!Files.isSymbolicLink(link)) {
                    throw new FileAlreadyExistsException(link.toString(), null,
                        "A file already exists and is not a symbolic link");
                }
                Path linkTarget = Files.readSymbolicLink(link);
                if(!linkTarget.equals(lcmsPath)) {
                    String msg = String.format("A symblic link to mzXML file already exists, "
                        + "but points to a different file: %s", link);
                    throw new FileAlreadyExistsException(link.toString(), null, msg);
                }
                return;
            }
            Files.createSymbolicLink(link, lcmsPath);
        }
    }
}
