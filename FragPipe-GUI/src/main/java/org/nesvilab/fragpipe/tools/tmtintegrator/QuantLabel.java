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

package org.nesvilab.fragpipe.tools.tmtintegrator;

import com.google.common.collect.HashMultimap;
import com.google.common.collect.Multimap;
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
  public static final String TYPE_SCLIP = "sCLIP";
  public static final String TYPE_CUSTOM = "xtag";
  public static final String TYPE_CUSTOM2 = "xtag2";
  public static final String TYPE_IBT = "IBT";
  public static final String TYPE_DILEU = "DiLeu";
  public static final String CUSTOM_LABEL_NAME = "develop";
  public static final String CUSTOM_LABEL_NAME2 = "develop2";


  public static final List<QuantLabel> LABELS;
  public static final Multimap<String, Float> labelModMap;

  static {
    List<QuantLabel> labels = new ArrayList<>();
    labels.add(new QuantLabel(TYPE_TMT, "TMT-0", List.of("126")));
    labels.add(new QuantLabel(TYPE_TMT, "TMT-2", List.of("126", "127C")));
    labels.add(new QuantLabel(TYPE_TMT, "TMT-6", Arrays.asList("126, 127N, 128C, 129N, 130C, 131N".split("[,\\s]+"))));
    labels.add(new QuantLabel(TYPE_TMT, "TMT-10", Arrays.asList("126, 127N, 127C, 128N, 128C, 129N, 129C, 130N, 130C, 131N".split("[,\\s]+"))));
    labels.add(new QuantLabel(TYPE_TMT, "TMT-11", Arrays.asList("126, 127N, 127C, 128N, 128C, 129N, 129C, 130N, 130C, 131N, 131C".split("[,\\s]+"))));
    labels.add(new QuantLabel(TYPE_TMT, "TMT-16", Arrays.asList("126, 127N, 127C, 128N, 128C, 129N, 129C, 130N, 130C, 131N, 131C, 132N, 132C, 133N, 133C, 134N".split("[,\\s]+"))));
    labels.add(new QuantLabel(TYPE_TMT, "TMT-18", Arrays.asList("126, 127N, 127C, 128N, 128C, 129N, 129C, 130N, 130C, 131N, 131C, 132N, 132C, 133N, 133C, 134N, 134C, 135N".split("[,\\s]+"))));
    labels.add(new QuantLabel(TYPE_TMT, "TMT-35", Arrays.asList("126, 127N, 127C, 128N, 128C, 129N, 129C, 130N, 130C, 131N, 131C, 132N, 132C, 133N, 133C, 134N, 134C, 135N, 127D, 128ND, 128CD, 129ND, 129CD, 130ND, 130CD, 131ND, 131CD, 132ND, 132CD, 133ND, 133CD, 134ND, 134CD, 135ND, 135CD".split("[,\\s]+"))));
    labels.add(new QuantLabel(TYPE_ITRAQ, "iTRAQ-4", Arrays.asList("114, 115, 116, 117".split("[,\\s]+"))));
    labels.add(new QuantLabel(TYPE_ITRAQ, "iTRAQ-8", Arrays.asList("113, 114, 115, 116, 117, 118, 119, 121".split("[,\\s]+"))));
    labels.add(new QuantLabel(TYPE_SCLIP, "sCLIP-6", Arrays.asList("300, 301N, 301C, 302N, 302O, 302C".split("[,\\s]+"))));
    labels.add(new QuantLabel(TYPE_IBT, "IBT-16", Arrays.asList("114, 115N, 115C, 116N, 116C, 117N, 117C, 118N, 118C, 119N, 119C, 120N, 120C, 121N, 121C, 122".split("[,\\s]+"))));
    labels.add(new QuantLabel(TYPE_DILEU, "DiLeu-12", Arrays.asList("115a, 115b, 116a, 116b, 116c, 117a, 117b, 117c, 118a, 118b, 118c, 118d".split("[,\\s]+"))));
    labels.add(new QuantLabel(TYPE_DILEU, "DiLeu-1", Arrays.asList("114".split("[,\\s]+"))));
    labels.add(new QuantLabel(TYPE_CUSTOM, CUSTOM_LABEL_NAME, Arrays.asList("xtag1, xtag2, xtag3, xtag4, xtag5, xtag6, xtag7, xtag8, xtag9, xtag10, xtag11, xtag12, xtag13, xtag14, xtag15, xtag16, xtag17, xtag18, xtag19, xtag20, xtag21, xtag22, xtag23, xtag24, xtag25, xtag26, xtag27, xtag28, xtag29, xtag30, xtag31, xtag32".split("[,\\s]+"))));
    labels.add(new QuantLabel(TYPE_CUSTOM2, CUSTOM_LABEL_NAME2, Arrays.asList("114, 115a, 115b, 115c, 116a, 116b, 116c, 116d, 116e, 117a, 117b, 117c, 117d, 117e, 117f, 118a, 118b, 118c, 118d, 118e, 118f, 118g, 119a, 119b, 119c, 119d, 119e, 119f, 119g".split("[,\\s]+"))));
    LABELS = Collections.unmodifiableList(labels);

    labelModMap = HashMultimap.create(LABELS.size(), 1);
    labelModMap.put("TMT-0", 295.1896f);
    labelModMap.put("TMT-0", 224.152478f);
    labelModMap.put("TMT-2", 225.155833f);
    labelModMap.put("TMT-6", 229.162932f);
    labelModMap.put("TMT-10", 229.162932f);
    labelModMap.put("TMT-11", 229.162932f);
    labelModMap.put("TMT-16", 304.2071f);
    labelModMap.put("TMT-18", 304.2071f);
    labelModMap.put("TMT-35", 304.2071f);
    labelModMap.put("iTRAQ-4", 144.102063f);
    labelModMap.put("iTRAQ-8", 304.205360f);
    labelModMap.put("sCLIP-6", 481.2f);
    labelModMap.put("IBT-16", 227.1f);
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
