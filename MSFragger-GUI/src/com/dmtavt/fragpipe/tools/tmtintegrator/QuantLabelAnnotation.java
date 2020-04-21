package com.dmtavt.fragpipe.tools.tmtintegrator;

import java.util.StringJoiner;

public class QuantLabelAnnotation {

  private String label;
  private String sample;

  public QuantLabelAnnotation() {
  }

  public QuantLabelAnnotation(String label, String sample) {
    this.label = label;
    this.sample = sample;
  }

  public String getLabel() {
    return label;
  }

  public void setLabel(String label) {
    this.label = label;
  }

  public String getSample() {
    return sample;
  }

  public void setSample(String sample) {
    this.sample = sample;
  }

  @Override
  public String toString() {
    return new StringJoiner(", ", QuantLabelAnnotation.class.getSimpleName() + "[", "]")
        .add("label='" + label + "'")
        .add("sample='" + sample + "'")
        .toString();
  }
}
