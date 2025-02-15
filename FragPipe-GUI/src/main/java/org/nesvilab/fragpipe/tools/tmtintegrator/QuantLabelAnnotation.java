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

import java.util.StringJoiner;
import java.util.regex.Pattern;

public class QuantLabelAnnotation {

  public static final Pattern disallowedPattern = Pattern.compile("[^0-9a-zA-Z_-]");

  private String label;
  private String sample;

  public QuantLabelAnnotation() {
  }

  public QuantLabelAnnotation(String label, String sample) {
    this.label = label.trim();
    this.sample = unifyAnnotationSampleName(sample);
  }

  public String getLabel() {
    return label;
  }

  public void setLabel(String label) {
    this.label = label.trim();
  }

  public String getSample() {
    return sample;
  }

  public void setSample(String sample) {
    this.sample = unifyAnnotationSampleName(sample);
  }

  @Override
  public String toString() {
    return new StringJoiner(", ", QuantLabelAnnotation.class.getSimpleName() + "[", "]")
        .add("label='" + label + "'")
        .add("sample='" + sample + "'")
        .toString();
  }

  public static String unifyAnnotationSampleName(String s) {
    return disallowedPattern.matcher(s.trim()).replaceAll("");
  }
}
