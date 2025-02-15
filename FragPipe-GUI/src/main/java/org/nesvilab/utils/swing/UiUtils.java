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

import org.nesvilab.utils.StringUtils;
import org.nesvilab.utils.swing.UiSpinnerDouble.Builder;
import java.awt.Component;
import java.awt.event.ActionListener;
import java.awt.event.ItemListener;
import java.util.List;
import java.util.stream.Stream;
import javax.swing.DefaultComboBoxModel;
import javax.swing.ImageIcon;
import javax.swing.JButton;

public class UiUtils {
  private UiUtils() {}

  public static void name(Component component, String name) {
    component.setName(name);
  }
  /**
   * Loads icon from current
   * @param resourcePath E.g. "/org/nesvilab/fragpipe/icons/dia-umpire-16x16.png"
   */
  public static ImageIcon loadIcon(Class<?> clazz, String resourcePath) {
    return new ImageIcon(clazz.getResource(resourcePath));
  }

  public static UiSpinnerDouble.Builder spinnerDouble(double initVal, double min, double max, double step) {
    return new Builder().setInitVal(initVal).setMinVal(min).setMaxVal(max)
        .setStep(step);
  }

  public static UiSpinnerInt.Builder spinnerInt(int init, int min, int max, int step) {
    return new UiSpinnerInt.Builder().setInit(init).setMin(min).setMax(max).setStep(step);
  }

  public static UiCheck createUiCheck(String label, boolean selected, ItemListener listener) {
    UiCheck ui = new UiCheck(label, null, selected);
    ui.addItemListener(listener);
    return ui;
  }
  public static UiCheck createUiCheck(String label, boolean selected) {
    return new UiCheck(label, null, selected);
  }

  public static JButton createButton(String text, ActionListener listener) {
    JButton b = new JButton(text);
    b.addActionListener(listener);
    return b;
  }

  public static JButton createButton(String text, String tooltip, ActionListener listener) {
    JButton b = new JButton(text);
    b.setToolTipText(tooltip);
    b.addActionListener(listener);
    return b;
  }

  public static UiText createUiText(String filteredCharsRegex) {
    UiText uiText = new UiText();
    uiText.setDocument(DocumentFilters.getFilter(filteredCharsRegex));
    return uiText;
  }

  public static UiTextBuilder uiTextBuilder() {
    return new UiTextBuilder();
  }

  public static class UiTextBuilder {
    private final UiText uiText;

    public UiTextBuilder() {
      this.uiText = new UiText();
    }

    public UiTextBuilder cols(int cols) {
      uiText.setColumns(cols);
      return this;
    }

    public UiTextBuilder ghost(String ghostText) {
      uiText.setGhostText(ghostText);
      return this;
    }

    public UiTextBuilder filter(String filterRegex, String replacement) {
      uiText.setDocument(DocumentFilters.getFilter(filterRegex, replacement));
      return this;
    }

    public UiTextBuilder filter(String filterRegex) {
      return filter(filterRegex, "");
    }

    public UiTextBuilder text(String text) {
      uiText.setText(text);
      return this;
    }

    public UiText create() {
      if (StringUtils.isNotBlank(uiText.getGhostText())) {
        GhostText.register(uiText);
      }
      return uiText;
    }
  }

  public static UiCombo createUiCombo(String[] options) {
    UiCombo uiCombo = new UiCombo();
    uiCombo.setModel(new DefaultComboBoxModel<>(options));
    return uiCombo;
  }

  public static UiCombo createUiCombo(List<String> options) {
    return createUiCombo(options.toArray(new String[0]));
  }

  /**
   * Use '#values()' method of enum to create a dropdown.
   */
  public static UiCombo createUiCombo(Enum<?>[] enumValues) {
    return createUiCombo(Stream.of(enumValues).map(Enum::name).toArray(String[]::new));
  }
}
