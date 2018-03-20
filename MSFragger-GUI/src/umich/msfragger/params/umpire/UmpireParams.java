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
package umich.msfragger.params.umpire;

import umich.msfragger.exceptions.ParsingException;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import umich.msfragger.params.PropLine;
import umich.msfragger.params.PropertyFileContent;

/**
 *
 * @author dattam
 */
public class UmpireParams implements PropertyFileContent {
    public static final String PROP_Threads = "Thread";
    public static final String PROP_RPmax = "RPmax";
    public static final String PROP_RFmax = "RFmax";
    public static final String PROP_CorrThreshold = "CorrThreshold";
    public static final String PROP_DeltaApex = "DeltaApex";
    public static final String PROP_RTOverlap = "RTOverlap";
    public static final String PROP_AdjustFragIntensity = "AdjustFragIntensity";
    public static final String PROP_BoostComplementaryIon = "BoostComplementaryIon";
    
    public static final String PROP_MS1PPM = "SE.MS1PPM";
    public static final String PROP_MS2PPM = "SE.MS2PPM";
    public static final String PROP_SN = "SE.SN";
    public static final String PROP_MS2SN = "SE.MS2SN";
    public static final String PROP_MinMSIntensity = "SE.MinMSIntensity";
    public static final String PROP_MinMSMSIntensity = "SE.MinMSMSIntensity";
    public static final String PROP_MaxCurveRTRange = "SE.MaxCurveRTRange";
    public static final String PROP_NoMissedScan = "SE.NoMissedScan";
    public static final String PROP_MinFrag = "SE.MinFrag";
    public static final String PROP_EstimateBG = "SE.EstimateBG";
    public static final String PROP_MinNoPeakCluster = "SE.MinNoPeakCluster";
    public static final String PROP_MaxNoPeakCluster = "SE.MaxNoPeakCluster";
    
    public static final String PROP_WindowType = "WindowType";
    public static final String PROP_WindowSize = "WindowSize";
    
    public static final String FILE_BASE_NAME = "umpire-se";
    public static final String FILE_BASE_EXT = "params";
    /** This file is in the jar, use getResourceAsStream() to get it.  */
    public static final String DEFAULT_FILE = "diaumpire_se.params";
    
    Properties props = new Properties();
    protected List<String> linesInOriginalFile = new ArrayList<>();
    protected Map<Integer, PropLine> mapLines= new TreeMap<>();
    protected Map<String, Integer> mapProps = new HashMap<>();
    
    protected String binUmpire;
    protected String binMsconvert;
    
    public static UmpireParams parseDefault() throws ParsingException {
        InputStream is = UmpireParams.class.getResourceAsStream(DEFAULT_FILE);
        return UmpireParams.parse(is);
    }

    public String getBinUmpire() {
        return binUmpire;
    }

    public void setBinUmpire(String binUmpire) {
        this.binUmpire = binUmpire;
    }

    public String getBinMsconvert() {
        return binMsconvert;
    }

    public void setBinMsconvert(String binMsconvert) {
        this.binMsconvert = binMsconvert;
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
    
    
    
    public static UmpireParams parse(InputStream is) throws ParsingException {
        UmpireParams umpireParams = new UmpireParams();
        Properties properties = new Properties();

        Pattern propRegex = Pattern.compile("^\\s*([^=]+?)\\s*=\\s*(.+?)\\s*", Pattern.CASE_INSENSITIVE);


        try (BufferedReader br = new BufferedReader(new InputStreamReader(is, Charset.forName("UTF-8")))) {
            int lineNum = 0;
            String line;
            while ((line = br.readLine()) != null) {
                lineNum++;
                umpireParams.linesInOriginalFile.add(line);
                Properties props = new Properties();
                boolean hasComments = false;
                boolean hasProperty = false;
                int indexOfHash = line.indexOf('#');
                if (indexOfHash >= 0) {
                    hasComments = true;
                    if (indexOfHash > 2) {
                        // if it's over 2, then there is a possibility for having a
                        // property before it. A property needs at least one char for
                        // name, then '=' sign and at least one char for value
                        int indexOfEquals = line.substring(0, indexOfHash).indexOf('=');
                        if (indexOfEquals > 2) {
                            // ok, this might be a property
                            String possiblePropString = line.substring(0, indexOfHash);
                            addString(propRegex, possiblePropString, line, indexOfHash, umpireParams, lineNum);
                        }
                    } else {
                        // index of hash is small and does not allow for a property, must be a simple string
                        umpireParams.mapLines.put(lineNum, new PropLine(line, null, null, null));
                    }
                } else {
                    // it must be a pure property string or just a meaningless string
                    int indexOfEquals = line.indexOf('=');
                    if (indexOfEquals > 0) {
                        String possiblePropString = line;
                        addString(propRegex, possiblePropString, line, indexOfHash, umpireParams, lineNum);
                    } else {
                        // it's a simple line, just add it
                        umpireParams.mapLines.put(lineNum, new PropLine(line, null, null, null));
                    }
                }
            }

        } catch (IOException e) {
            throw new ParsingException("Error reading comet params file", e);
        }
        return umpireParams;
    }
    
    private static void addString(Pattern propRegex, String possiblePropString, String line, int indexOfHash, UmpireParams umpireParams, int lineNum) throws ParsingException {
        Matcher matcher = propRegex.matcher(possiblePropString);
        if (matcher.matches()) {
            String comment = null;
            if (indexOfHash >= 0)
                comment = line.substring(indexOfHash, line.length());
            String propName = matcher.group(1);
            String propVal =  matcher.group(2);
            if (propName.isEmpty())
                throw new ParsingException(String.format(Locale.ROOT, "Property on line number %d had empty name", lineNum));
            if (propVal.isEmpty())
                throw new ParsingException(String.format(Locale.ROOT, "Property on line number %d had empty value", lineNum));
            umpireParams.mapLines.put(lineNum, new PropLine(null, propName, propVal, comment));
            umpireParams.mapProps.put(propName, lineNum);
            umpireParams.props.put(propName, propVal);
        } else {
            // we didn't find the property here, it's just a line
            umpireParams.mapLines.put(lineNum, new PropLine(line, null, null, null));
        }
    }
    
}
