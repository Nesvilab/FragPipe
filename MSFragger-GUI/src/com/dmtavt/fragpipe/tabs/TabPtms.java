package com.dmtavt.fragpipe.tabs;

import com.dmtavt.fragpipe.api.Bus;
import com.github.chhh.utils.swing.JPanelWithEnablement;
import com.github.chhh.utils.swing.MigUtils;
import umich.msfragger.params.ptmshepherd.PtmshepherdPanel;

public class TabPtms extends JPanelWithEnablement {
  private static MigUtils mu = MigUtils.get();
  public static final String TAB_PREFIX = "quantitation.";
  private PtmshepherdPanel panelPtmshepherd;

  public TabPtms() {
    init();
    initMore();
  }

  private void initMore() {
    //Bus.registerQuietly(this);
  }

  private void init() {
    mu.layout(this).fillX();

    panelPtmshepherd = new PtmshepherdPanel();

    mu.add(this, panelPtmshepherd).growX().wrap();
  }
}
