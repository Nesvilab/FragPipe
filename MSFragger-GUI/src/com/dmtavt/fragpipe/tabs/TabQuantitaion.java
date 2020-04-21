package com.dmtavt.fragpipe.tabs;

import com.github.chhh.utils.swing.JPanelWithEnablement;
import com.github.chhh.utils.swing.MigUtils;
import com.dmtavt.fragpipe.tools.ionquant.QuantPanelLabelfree;
import com.dmtavt.fragpipe.tools.tmtintegrator.TmtiPanel;

public class TabQuantitaion extends JPanelWithEnablement {
  private static MigUtils mu = MigUtils.get();
  public static final String TAB_PREFIX = "quantitation.";
  private QuantPanelLabelfree panelLabelfree;
  private TmtiPanel panelTmti;

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
    panelTmti = new TmtiPanel();

    mu.add(this, panelLabelfree).growX().wrap();
    mu.add(this, panelTmti).growX().wrap();
  }


}
