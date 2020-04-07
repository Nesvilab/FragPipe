package com.dmtavt.fragpipe.tabs;

import com.dmtavt.fragpipe.tools.pepproph.PepProphPanel;
import com.github.chhh.utils.swing.JPanelWithEnablement;
import com.github.chhh.utils.swing.MigUtils;

public class TabValidation extends JPanelWithEnablement {
  private static MigUtils mu = MigUtils.get();
  public static final String TAB_PREFIX = "validation.";
  private PepProphPanel panelPepProph;

  public TabValidation() {
    init();
    initMore();
  }

  private void initMore() {
    // TODO: Bus.register(this);
  }

  private void init() {
    mu.layout(this).fillX();

    panelPepProph = new PepProphPanel();

    mu.add(this, panelPepProph).growX().wrap();
  }


}
