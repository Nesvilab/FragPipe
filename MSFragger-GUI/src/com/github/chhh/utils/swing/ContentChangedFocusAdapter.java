package com.github.chhh.utils.swing;

import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.util.Objects;
import java.util.function.BiConsumer;

public class ContentChangedFocusAdapter extends FocusAdapter {
  private final StringRepresentable component;
  private final BiConsumer<String, String> onContentChanged;
  private String before = null;
  private String after = null;

  public ContentChangedFocusAdapter(StringRepresentable component, BiConsumer<String, String> onContentChanged) {
    this.component = component;
    this.onContentChanged = onContentChanged;
  }

  @Override
  public void focusGained(FocusEvent e) {
    before = component.asString();
    after = before;
    super.focusGained(e);
  }

  @Override
  public void focusLost(FocusEvent e) {
    after = component.asString();
    if (!Objects.equals(after, before)) {
      onContentChanged.accept(before, after);
    }
    super.focusLost(e);
  }
}
