package com.dmtavt.fragpipe.tools.tmtintegrator;

public class ComboValue implements UiRepresentableValue {
  public final String valInConfig;
  public final String valInUi;
  public final String description;

  public ComboValue(String valInConfig, String valInUi, String description) {
    this.valInConfig = valInConfig;
    this.valInUi = valInUi;
    this.description = description;
  }

  public ComboValue(String valueInFile, String valInUi) {
    this(valueInFile, valInUi, "");
  }

  @Override
  public String getValInConfig() {
    return valInConfig;
  }

  @Override
  public String getValInUi() {
    return valInUi;
  }

  public String getDescription() {
    return description;
  }
}
