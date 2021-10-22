package com.dmtavt.fragpipe.tabs;

import com.dmtavt.fragpipe.tools.diann.DiannPanel;
import com.github.chhh.utils.swing.JPanelWithEnablement;
import com.github.chhh.utils.swing.MigUtils;

public class TabDiann extends JPanelWithEnablement {
  private static MigUtils mu = MigUtils.get();
  public static final String TAB_PREFIX = "diann.";
  private DiannPanel panelDiann;

  public TabDiann() {
    init();
    initMore();
  }

  private void initMore() {
  }

  private void init() {
    mu.layout(this).fillX();

    panelDiann = new DiannPanel();

    mu.add(this, panelDiann).growX().wrap();
  }
}
