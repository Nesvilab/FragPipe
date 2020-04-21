package com.dmtavt.fragpipe.tabs;

import com.github.chhh.utils.swing.JPanelWithEnablement;
import com.github.chhh.utils.swing.MigUtils;
import com.dmtavt.fragpipe.tools.speclibgen.SpeclibPanel;

public class TabSpecLib extends JPanelWithEnablement {
  private static MigUtils mu = MigUtils.get();
  public static final String TAB_PREFIX = "spectral-library.";
  private SpeclibPanel panelSpeclib;

  public TabSpecLib() {
    init();
    initMore();
  }

  private void initMore() {
    //Bus.registerQuietly(this);
  }

  private void init() {
    mu.layout(this).fillX();

    panelSpeclib = new SpeclibPanel();

    mu.add(this, panelSpeclib).growX().wrap();
  }
}
