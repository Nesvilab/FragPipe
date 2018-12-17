package com.github.chhh.utils.swing;

import javax.swing.JSpinner;
import javax.swing.SpinnerNumberModel;

public class UiSpinnerInt extends JSpinner implements StringRepresentable {

  public UiSpinnerInt(int initVal, int minVal, int maxVal, int step) {
    super(new SpinnerNumberModel(initVal, minVal, maxVal, step));
  }

  @Override
  public String asString() {
    return ((Integer)getValue()).toString();
  }

  @Override
  public void fromString(String s) {
    try {
      setValue(Integer.parseInt(s));
    } catch (Exception e) {
      throw new IllegalArgumentException(String.format("String [%s] does not represent an integer", s), e);
    }
  }

  public int getActualValue() {
    return (Integer)getValue();
  }
}
