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

import org.nesvilab.fragpipe.exceptions.ParsingException;
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
import org.nesvilab.fragpipe.params.PropLine;
import org.nesvilab.fragpipe.params.PropertyFileContent;

/**
 *
 * @author Dmitry Avtonomov
 */
public class UmpireQuantParams implements PropertyFileContent {
    public static final String DEFAULT_FILE = "diaumpire_quant.params";
    public static final String FILE_BASE_NAME = "diaumpire_quant";
    public static final String FILE_BASE_EXT = "params";
    
    public static final String PROP_Thread = "Thread";
    public static final String PROP_Path = "Path";
    public static final String PROP_Fasta = "Fasta";
    public static final String PROP_DecoyPrefix = "DecoyPrefix";
    public static final String PROP_Combined_Prot = "Combined_Prot";
    public static final String PROP_InternalLibSearch = "InternalLibSearch";
//    public static final String PROP_ExternalLibSearch = "ExternalLibSearch";
    public static final String PROP_PeptideFDR = "PeptideFDR";
    public static final String PROP_ProteinFDR = "ProteinFDR";
    public static final String PROP_DataSetLevelPepFDR = "DataSetLevelPepFDR";
    public static final String PROP_FilterWeight = "FilterWeight";
    public static final String PROP_MinWeight = "MinWeight";
    public static final String PROP_TopNFrag = "TopNFrag";
    public static final String PROP_TopNPep = "TopNPep";
    public static final String PROP_Freq = "Freq";
    public static final String PROP_ExternalLibPath = "ExternalLibPath";
//    public static final String PROP_ = "";

    Properties props = new Properties();
    protected List<String> linesInOriginalFile = new ArrayList<>();
    protected Map<Integer, PropLine> mapLines= new TreeMap<>();
    protected Map<String, Integer> mapProps = new HashMap<>();
    
    
    public static UmpireQuantParams parseDefault() throws ParsingException {
        InputStream is = UmpireQuantParams.class.getResourceAsStream(DEFAULT_FILE);
        return UmpireQuantParams.parse(is);
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
    
    
    
    public static UmpireQuantParams parse(InputStream is) throws ParsingException {
        UmpireQuantParams umpireParams = new UmpireQuantParams();
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
    
    private static void addString(Pattern propRegex, String possiblePropString, String line, int indexOfHash, UmpireQuantParams umpireParams, int lineNum) throws ParsingException {
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
