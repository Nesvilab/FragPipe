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

package com.dmtavt.fragpipe.tools.tmtintegrator;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public class QuantLabel {

  private final String name;
  private final List<String> reagentNames;
  private final String type;
  // string to pass to --brand for labelquant (NOTE: will be converted to lower-case)
  public static final String TYPE_TMT = "TMT";
  public static final String TYPE_ITRAQ = "iTRAQ";
  public static final String TYPE_CUSTOM = "xtag";

  public static final String CUSTOM_LABEL_NAME = "develop";


  public static final List<QuantLabel> LABELS;

  static {
    List<QuantLabel> labels = new ArrayList<>();
    labels.add(new QuantLabel(TYPE_TMT, "TMT-6", Arrays.asList("126, 127N, 128C, 129N, 130C, 131N".split("[,\\s]+"))));
    labels.add(new QuantLabel(TYPE_TMT, "TMT-10", Arrays.asList("126, 127N, 127C, 128N, 128C, 129N, 129C, 130N, 130C, 131N".split("[,\\s]+"))));
    labels.add(new QuantLabel(TYPE_TMT, "TMT-11", Arrays.asList("126, 127N, 127C, 128N, 128C, 129N, 129C, 130N, 130C, 131N, 131C".split("[,\\s]+"))));
    labels.add(new QuantLabel(TYPE_TMT, "TMT-16", Arrays.asList("126, 127N, 127C, 128N, 128C, 129N, 129C, 130N, 130C, 131N, 131C, 132N, 132C, 133N, 133C, 134N".split("[,\\s]+"))));
    labels.add(new QuantLabel(TYPE_TMT, "TMT-18", Arrays.asList("126, 127N, 127C, 128N, 128C, 129N, 129C, 130N, 130C, 131N, 131C, 132N, 132C, 133N, 133C, 134N, 134C, 135N".split("[,\\s]+"))));
    labels.add(new QuantLabel(TYPE_ITRAQ, "iTRAQ-4", Arrays.asList("114, 115, 116, 117".split("[,\\s]+"))));
    labels.add(new QuantLabel(TYPE_ITRAQ, "iTRAQ-8", Arrays.asList("113, 114, 115, 116, 117, 118, 119, 121".split("[,\\s]+"))));
    labels.add(new QuantLabel(TYPE_CUSTOM, CUSTOM_LABEL_NAME, Arrays.asList("xtag1, xtag2, xtag3, xtag4, xtag5, xtag6, xtag7, xtag8, xtag9, xtag10, xtag11, xtag12, xtag13, xtag14, xtag15, xtag16, xtag17, xtag18".split("[,\\s]+"))));
    LABELS = Collections.unmodifiableList(labels);
  }

  public QuantLabel(String type, String name, List<String> reagentNames) {
    this.type = type;
    this.name = name;
    this.reagentNames = reagentNames;
  }

  // get the custom QuantLabel by searching for it
  public static QuantLabel getCustomQuantLabel(List<QuantLabel> labels) {
    for (QuantLabel label : labels) {
      if (label.name.equals(CUSTOM_LABEL_NAME)) {
        return label;
      }
    }
    // not found - error. Should always be in the list
    return null;
  }

  public String getName() {
    return name;
  }

  public List<String> getReagentNames() {
    return reagentNames;
  }

  public String getType() {
    return type;
  }
}
