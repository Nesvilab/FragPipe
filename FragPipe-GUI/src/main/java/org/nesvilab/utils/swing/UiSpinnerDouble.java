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

package org.nesvilab.utils.swing;

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

  public static Builder builder(double initVal, double minVal, double maxVal, double step) {
    Builder b = new Builder();
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

  public static class Builder {

    private double initVal;
    private double minVal;
    private double maxVal;
    private double step;
    private DecimalFormat format;
    private int numDecimalDigits;

    public Builder setInitVal(double initVal) {
      this.initVal = initVal;
      return this;
    }

    public Builder setMinVal(double minVal) {
      this.minVal = minVal;
      return this;
    }

    public Builder setMaxVal(double maxVal) {
      this.maxVal = maxVal;
      return this;
    }

    public Builder setStep(double step) {
      this.step = step;
      return this;
    }

    public Builder setFormat(DecimalFormat format) {
      this.format = format;
      return this;
    }

    public Builder setFormat(String decimalFormatSpec) {
      this.format = new DecimalFormat(decimalFormatSpec);
      return this;
    }

    public Builder setCols(int numDecimalDigits) {
      this.numDecimalDigits = numDecimalDigits;
      return this;
    }

    public UiSpinnerDouble create() {
      if (format == null) {
        throw new IllegalStateException("You must set the format before creating the spinner");
      }
      UiSpinnerDouble ui = new UiSpinnerDouble(initVal, minVal, maxVal, step, format);
      ui.setColumns(numDecimalDigits);
      return ui;
    }
  }

  public double getActualValue() {
    return (double) getValue();
  }

}
