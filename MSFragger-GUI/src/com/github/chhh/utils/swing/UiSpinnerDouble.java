package com.github.chhh.utils.swing;

import java.text.DecimalFormat;
import javax.swing.JComponent;
import javax.swing.JFormattedTextField;
import javax.swing.JSpinner;
import javax.swing.SpinnerNumberModel;

public class UiSpinnerDouble extends JSpinner implements StringRepresentable {

  private final DecimalFormat format;

  public UiSpinnerDouble(double initVal, double minVal, double maxVal, double step, DecimalFormat format) {
    super(new SpinnerNumberModel(initVal, minVal, maxVal, step));
    this.format = format;
  }

  public UiSpinnerDouble(double initVal, double minVal, double maxVal, double step, int numDecimalDigits, DecimalFormat format) {
    super(new SpinnerNumberModel(initVal, minVal, maxVal, step));
    this.format = format;
    JComponent editor = getEditor();
    if (editor instanceof JSpinner.NumberEditor) {
      JSpinner.NumberEditor e = (JSpinner.NumberEditor) editor;
      e.getFormat().setMinimumFractionDigits(numDecimalDigits);
    }
  }

  public static UiSpinnerDoubleBuilder builder(double initVal, double minVal, double maxVal, double step) {
    UiSpinnerDoubleBuilder b = new UiSpinnerDoubleBuilder();
    b.setInitVal(initVal);
    b.setMinVal(minVal);
    b.setMaxVal(maxVal);
    b.setStep(step);
    return b;
  }

  @Override
  public String asString() {
    return format.format(getValue());
  }

  @Override
  public void fromString(String s) {
    try {
      setValue(Double.parseDouble(s));
    } catch (Exception e) {
      throw new IllegalArgumentException(String.format("String [%s] does not represent a double", s), e);
    }
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

  public static class UiSpinnerDoubleBuilder {

    private double initVal;
    private double minVal;
    private double maxVal;
    private double step;
    private DecimalFormat format;
    private int numDecimalDigits;

    public UiSpinnerDoubleBuilder setInitVal(double initVal) {
      this.initVal = initVal;
      return this;
    }

    public UiSpinnerDoubleBuilder setMinVal(double minVal) {
      this.minVal = minVal;
      return this;
    }

    public UiSpinnerDoubleBuilder setMaxVal(double maxVal) {
      this.maxVal = maxVal;
      return this;
    }

    public UiSpinnerDoubleBuilder setStep(double step) {
      this.step = step;
      return this;
    }

    public UiSpinnerDoubleBuilder setFormat(DecimalFormat format) {
      this.format = format;
      return this;
    }

    public UiSpinnerDoubleBuilder setNumCols(int numDecimalDigits) {
      this.numDecimalDigits = numDecimalDigits;
      return this;
    }

    public UiSpinnerDouble create() {
      UiSpinnerDouble ui = new UiSpinnerDouble(initVal, minVal, maxVal, step, format);
      ui.setColumns(numDecimalDigits);
      return ui;
    }
  }
}
