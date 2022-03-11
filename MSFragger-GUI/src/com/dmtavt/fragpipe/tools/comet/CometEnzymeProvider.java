package com.dmtavt.fragpipe.tools.comet;

import com.dmtavt.fragpipe.tools.fragger.MsfraggerEnzyme;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Supplier;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class CometEnzymeProvider implements Supplier<List<MsfraggerEnzyme>> {
    private static final String PROPERTIES_FN = "enzyme-defs-comet.txt";

    @Override
    public List<MsfraggerEnzyme> get() {

        try (InputStream is = CometEnzymeProvider.class.getResourceAsStream(PROPERTIES_FN)) {
            if (is == null) {
                throw new IllegalStateException(String.format(
                        "Could not read '%s' from the classpath of '%s'", PROPERTIES_FN, CometEnzymeProvider.class.getName()));
            }
            BufferedReader br = new BufferedReader(new InputStreamReader(is));
            List<String> lines = new ArrayList<>();
            List<MsfraggerEnzyme> enzymes = new ArrayList<>();
            Pattern re = Pattern.compile("\\d+\\.\\s+([^\\s]+)\\s+([^\\s]+)\\s+([^\\s]+)\\s+([^\\s]+)");
            Map<String, String> cometEnzymeSenseToLetter = new HashMap<>();
            cometEnzymeSenseToLetter.put("1", "C");
            cometEnzymeSenseToLetter.put("0", "N");
            String line;
            while ((line = br.readLine()) != null) {
                lines.add(line);
                Matcher m = re.matcher(line);
                if (!m.find())
                    continue;
                String sense = cometEnzymeSenseToLetter.get(m.group(2));
                if (sense == null)
                    throw new IllegalStateException("Unknonwn sense for comet enzyme: " + m.group(2));
                enzymes.add(new MsfraggerEnzyme(m.group(1), m.group(3), m.group(4), sense));
            }
            return enzymes;
        } catch (IOException e) {
            throw new IllegalStateException("Error reading properties from the classpath");
        }
    }
}