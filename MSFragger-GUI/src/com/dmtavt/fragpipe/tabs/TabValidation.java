package com.dmtavt.fragpipe.tabs;

import com.dmtavt.fragpipe.tools.pepproph.PepProphPanel;
import com.dmtavt.fragpipe.tools.protproph.ProtProphPanel;
import com.github.chhh.utils.swing.JPanelWithEnablement;
import com.github.chhh.utils.swing.MigUtils;
import umich.msfragger.params.crystalc.CrystalcPanel;

public class TabValidation extends JPanelWithEnablement {
  private static MigUtils mu = MigUtils.get();
  public static final String TAB_PREFIX = "validation.";
  private PepProphPanel panelPepProph;
  private ProtProphPanel panelProtProph;
  private CrystalcPanel panelCrystalc;

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
    panelProtProph = new ProtProphPanel();
    panelCrystalc = new CrystalcPanel();

    mu.add(this, panelPepProph).growX().wrap();
    mu.add(this, panelProtProph).growX().wrap();
    mu.add(this, panelCrystalc).growX().wrap();
  }


}
