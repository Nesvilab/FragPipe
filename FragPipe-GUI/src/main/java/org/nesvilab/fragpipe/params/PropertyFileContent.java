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

import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.nio.charset.StandardCharsets;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

/**
 *
 * @author dmitriya
 */
public interface PropertyFileContent {

    List<String> getLinesInOriginalFile();

    Map<Integer, PropLine> getMapLines();

    Map<String, Integer> getMapProps();

    Properties getProps();


    /**
     * Write the content of the property file with possible modifications to a new
     * file, keeping the formatting as close to original as possible.
     * @param out The stream should be connected to a file. The stream will be closed after this call.
     * @throws IOException
     */
    default void write(OutputStream out) throws IOException {
        try (PrintWriter pw = new PrintWriter(new OutputStreamWriter(out, StandardCharsets.UTF_8), true)) {

            Map<Integer, PropLine> mapLines = this.getMapLines();
            Properties props = this.getProps();
            HashSet<String> propNamesWritten = new HashSet<>();
            for (Map.Entry<Integer, PropLine> entry : mapLines.entrySet()) {
                PropLine propLine = entry.getValue();
                if (propLine.isSimpleLine()) {
                    pw.println(propLine.getJustALine());
                } else {
                    String propName = propLine.getName();
                    String propValue = props.getProperty(propName);
                    pw.print(propName + " = " + propValue);
                    propNamesWritten.add(propName);
                    if (propLine.getComment() != null) {
                        pw.print("\t\t\t" + propLine.getComment());
                    }
                    pw.println();
                }
            }
            Set<String> stringPropertyNames = props.stringPropertyNames();
            // if there was something else added on top of what was in the file
            // we will append to the end of the file
            for (String propName : stringPropertyNames) {
                if (propNamesWritten.contains(propName))
                    continue;
                pw.println(propName + " = " + props.getProperty(propName));
            }
        } finally {
            out.close();
        }

    }
}
