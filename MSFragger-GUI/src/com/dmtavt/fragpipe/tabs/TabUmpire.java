package com.dmtavt.fragpipe.tabs;

import com.dmtavt.fragpipe.tools.umpire.UmpirePanel;
import com.github.chhh.utils.swing.JPanelWithEnablement;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;

public class TabUmpire extends JPanelWithEnablement {

  private UmpirePanel umpirePanel;

  public TabUmpire() {
    init();
    initMore();
  }

  private void initMore() {

  }

  private void init() {
    this.setLayout(new MigLayout(new LC().fillX()));
    umpirePanel = new UmpirePanel();
    add(umpirePanel, new CC().growX().wrap());
  }
}
