package com.dmtavt.fragpipe.tabs;

import com.dmtavt.fragpipe.tools.PSMValidation;
import com.dmtavt.fragpipe.tools.crystalc.CrystalcPanel;
import com.dmtavt.fragpipe.tools.philosopher.ReportPanel;
import com.dmtavt.fragpipe.tools.protproph.ProtProphPanel;
import com.dmtavt.fragpipe.tools.ptmprophet.PtmProphetPanel;
import com.github.chhh.utils.swing.JPanelWithEnablement;
import com.github.chhh.utils.swing.MigUtils;

public class TabValidation extends JPanelWithEnablement {
  private static final MigUtils mu = MigUtils.get();

  public TabValidation() {
    init();
    initMore();
  }

  private void initMore() {
    //Bus.registerQuietly(this);
  }

  private void init() {
    mu.layout(this).fillX();

    PSMValidation psmValidation = new PSMValidation();
    CrystalcPanel panelCrystalc = new CrystalcPanel();
    PtmProphetPanel panelPtmProphet = new PtmProphetPanel();
    ProtProphPanel panelProtProph = new ProtProphPanel();
    ReportPanel panelReport = new ReportPanel();

    mu.add(this, panelCrystalc).growX().wrap();
    mu.add(this, psmValidation).growX().wrap();
    mu.add(this, panelPtmProphet).growX().wrap();
    mu.add(this, panelProtProph).growX().wrap();
    mu.add(this, panelReport).growX().wrap();
  }
}
