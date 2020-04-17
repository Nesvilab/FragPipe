package com.dmtavt.fragpipe.params.tmtintegrator;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public class QuantLabel {

  private final String name;
  private final List<String> reagentNames;
  private final String type;

  public static final List<QuantLabel> LABELS;

  static {
    List<QuantLabel> labels = new ArrayList<>();
    labels.add(new QuantLabel("TMT", "TMT-6", Arrays.asList("126, 127, 128, 129, 130, 131".split("[,\\s]+"))));
    labels.add(new QuantLabel("TMT", "TMT-10", Arrays.asList("126, 127N, 127C, 128N, 128C, 129N, 129C, 130N, 130C, 131N".split("[,\\s]+"))));
    labels.add(new QuantLabel("TMT", "TMT-11", Arrays.asList("126, 127N, 127C, 128N, 128C, 129N, 129C, 130N, 130C, 131N, 131C".split("[,\\s]+"))));
    labels.add(new QuantLabel("TMT", "TMT-16", Arrays.asList("126, 127N, 127C, 128N, 128C, 129N, 129C, 130N, 130C, 131N, 131C, 132N, 132C, 133N, 133C, 134N".split("[,\\s]+"))));
    LABELS = Collections.unmodifiableList(labels);
  }

  public QuantLabel(String type, String name, List<String> reagentNames) {
    this.type = type;
    this.name = name;
    this.reagentNames = reagentNames;
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
