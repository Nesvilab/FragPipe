package com.dmtavt.fragpipe.tabs;

import com.github.chhh.utils.swing.JPanelWithEnablement;
import com.github.chhh.utils.swing.MigUtils;
import umich.msfragger.params.ionquant.QuantPanelLabelfree;

public class TabQuantitaion extends JPanelWithEnablement {
  private static MigUtils mu = MigUtils.get();
  public static final String TAB_PREFIX = "quantitation.";
  private QuantPanelLabelfree panelLabelfree;

  public TabQuantitaion() {
    init();
    initMore();
  }

  private void initMore() {
    //Bus.registerQuietly(this);
  }

  private void init() {
    mu.layout(this).fillX();

    panelLabelfree = new QuantPanelLabelfree();

    mu.add(this, panelLabelfree).growX().wrap();
  }


}
