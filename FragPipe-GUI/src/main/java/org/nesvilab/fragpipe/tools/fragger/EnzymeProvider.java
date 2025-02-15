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

package org.nesvilab.fragpipe.tools.fragger;

import java.util.List;
import java.util.Properties;
import java.util.function.Supplier;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import org.nesvilab.utils.PropertiesUtils;

public class EnzymeProvider implements Supplier<List<MsfraggerEnzyme>> {
  private static final String PROPERTIES_FN = "enzyme-defs-supported.txt";

  @Override
  public List<MsfraggerEnzyme> get() {
    final Properties props = PropertiesUtils.initProperties(PROPERTIES_FN, EnzymeProvider.class);
    if (props == null || props.isEmpty()) {
      throw new IllegalStateException("Could not load " + PROPERTIES_FN + " for MSFragger supported enzymes");
    }
    final Pattern reCut = Pattern.compile("cut\\(([A-Z@]+)\\)");
    final Pattern reNocuts = Pattern.compile("nocuts\\(([A-Z@]+)\\)");
    final Pattern reSense = Pattern.compile("sense\\(([CN])\\)");
    List<MsfraggerEnzyme> list = props.stringPropertyNames().stream().sorted().map(name -> {
      String val = props.getProperty(name);
      String cut = "";
      Matcher mCut = reCut.matcher(val);
      if (mCut.find()) {
        cut = mCut.group(1).trim().toUpperCase();
      }
      String nocuts = "";
      Matcher mNocuts = reNocuts.matcher(val);
      if (mNocuts.find()) {
        nocuts = mNocuts.group(1).trim().toUpperCase();
      }
      Matcher mSense = reSense.matcher(val);
      final String sense = mSense.find() ? mSense.group(1).trim().toUpperCase() : null;
      return new MsfraggerEnzyme(name.trim(), cut, nocuts, sense);

    }).collect(Collectors.toList());

    return list;
  }
}
