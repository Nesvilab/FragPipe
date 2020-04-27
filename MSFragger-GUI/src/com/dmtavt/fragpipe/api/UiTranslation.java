package com.dmtavt.fragpipe.api;

public class UiTranslation {
  final String inUi;
  final String inConf;

  public UiTranslation(String inUi, String inConf) {
    this.inUi = inUi;
    this.inConf = inConf;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }

    UiTranslation that = (UiTranslation) o;

    if (inUi != null ? !inUi.equals(that.inUi) : that.inUi != null) {
      return false;
    }
    return inConf != null ? inConf.equals(that.inConf) : that.inConf == null;
  }

  @Override
  public int hashCode() {
    int result = inUi != null ? inUi.hashCode() : 0;
    result = 31 * result + (inConf != null ? inConf.hashCode() : 0);
    return result;
  }
}
