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

package org.nesvilab.fragpipe;

import org.nesvilab.fragpipe.api.UiTranslation;
import org.nesvilab.utils.IOUtils;
import org.nesvilab.utils.StringUtils;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;
import org.jooq.lambda.Seq;

public class WorkflowTranslator {

  public static final Map<String, List<UiTranslation>> readTranslations() {
    final String resource = "ui.translations";
    try (InputStream is = WorkflowTranslator.class.getResourceAsStream(resource)) {
      if (is == null) {
        throw new IllegalStateException(String
            .format("Could not open resource [%s] for class %s", resource,
                WorkflowTranslator.class.getSimpleName()));
      }
      List<String> mappings = Seq.seq(IOUtils.readAllLines(is))
          .filter(StringUtils::isNotBlank)
          .map(String::trim)
          .filter(Pattern.compile("^[a-zA-Z]").asPredicate())
          .toList();

//      Seq.seq(mappings)
//          .map(l -> l.split("::")).filter(split -> split.length == 2)
//          .map(split -> Seq.of(split).map(String::trim).filter(StringUtils::isNotBlank).toList())
//          .filter(pair -> pair.size() == 2)
//          .toMap()
      Map<String, List<UiTranslation>> map = new HashMap<>();
      for (String mapping : mappings) {
        String[] s1 = mapping.split("::");
        if (s1.length != 2)
          throw new IllegalStateException("Mapping lines must contain a single '::' delimiter");
        final String key = s1[0].trim();
        if (StringUtils.isBlank(key))
          throw new IllegalStateException("Blank mapping element name");
        String[] s2 = s1[1].split("<=>");
        if (s2.length != 2)
          throw new IllegalStateException("Mapping must contain exactly one '<=>' element with UI value on the left side and Config value on the right");
        final String valUi = s2[0].trim();
        final String valConf = s2[1].trim();
        if (StringUtils.isBlank(valUi) || StringUtils.isBlank(valConf))
          throw new IllegalStateException("No blank mappings allowed");
        map.computeIfAbsent(key, k -> new ArrayList<>()).add(new UiTranslation(valUi, valConf));
      }
      return map;

    } catch (IOException e) {
      throw new IllegalStateException(e);
    }

  }
}
