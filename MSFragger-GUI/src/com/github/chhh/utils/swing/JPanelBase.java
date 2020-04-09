package com.github.chhh.utils.swing;

import com.dmtavt.fragpipe.api.Bus;
import com.github.chhh.utils.StringUtils;
import com.github.chhh.utils.SwingUtils;
import java.awt.Component;
import java.awt.ItemSelectable;

public abstract class JPanelBase extends JPanelWithEnablement {
  protected final MigUtils mu = MigUtils.get();

  public JPanelBase() {
    init();
    initMore();
  }

  protected abstract ItemSelectable getRunCheckbox();
  protected abstract Component getEnablementToggleComponent();
  protected abstract String getComponentNamePrefix();

  protected abstract void init();
  protected void initMore() {
    if (StringUtils.isNotBlank(getComponentNamePrefix())) {
      SwingUtils.renameDeep(this, false, getComponentNamePrefix(), null);
    }
    if (getRunCheckbox() != null && getEnablementToggleComponent() != null) {
      SwingUtils.setEnablementUpdater(this, getEnablementToggleComponent(), getRunCheckbox());
    }
    Bus.registerQuietly(this);
  }
}
