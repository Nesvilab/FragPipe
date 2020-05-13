package com.dmtavt.fragpipe.tabs;

import com.dmtavt.fragpipe.tools.tmtintegrator.TmtiPanel;
import com.github.chhh.utils.swing.JPanelWithEnablement;
import com.github.chhh.utils.swing.MigUtils;

public class TabQuantitaionLabeling extends JPanelWithEnablement {
  private static MigUtils mu = MigUtils.get();
  public static final String TAB_PREFIX = "quant-labeling.";
  private TmtiPanel panelTmti;

  public TabQuantitaionLabeling() {
    init();
    initMore();
  }

  private void initMore() {
    //Bus.registerQuietly(this);
  }

  private void init() {
    mu.layout(this).fillX();

    panelTmti = new TmtiPanel();

    mu.add(this, panelTmti).growX().wrap();
  }

}
