package com.github.chhh.utils.swing;

import java.util.List;
import javax.swing.DefaultComboBoxModel;

public class UiUtils {
  private UiUtils() {}

  public static UiText createUiText(String filteredCharsRegex) {
    UiText uiText = new UiText();
    uiText.setDocument(DocumentFilters.getFilter(filteredCharsRegex));
    return uiText;
  }

  public static UiCombo createUiCombo(String[] options) {
    UiCombo uiCombo = new UiCombo();
    uiCombo.setModel(new DefaultComboBoxModel<>(options));
    return uiCombo;
  }

  public static UiCombo createUiCombo(List<String> options) {
    return createUiCombo(options.toArray(new String[0]));
  }
}
