package com.github.chhh.utils.swing;

import javax.swing.JComponent;
import javax.swing.JFormattedTextField;
import javax.swing.JSpinner;
import javax.swing.SpinnerNumberModel;

public class UiSpinnerInt extends JSpinner implements StringRepresentable {

  public UiSpinnerInt(int initVal, int minVal, int maxVal, int step) {
    super(new SpinnerNumberModel(initVal, minVal, maxVal, step));
  }

  public UiSpinnerInt(int initVal, int minVal, int maxVal, int step, int columns) {
    super(new SpinnerNumberModel(initVal, minVal, maxVal, step));
    setCols(columns);
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

  /** Sets the number of columns the spinner can display. */
  private void setCols(int cols) {
    JFormattedTextField tf = ((DefaultEditor) getEditor()).getTextField();
    tf.setColumns(cols);
  }

  /** Sets the number of columns the spinner can display. */
  public void setColumns(int cols) {
    setCols(cols);
  }
}
