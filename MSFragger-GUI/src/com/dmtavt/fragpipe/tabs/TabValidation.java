/*
 * This file is part of FragPipe.
 *
 * FragPipe is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * FragPipe is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with FragPipe.  If not, see <https://www.gnu.org/licenses/>.
 */

package com.dmtavt.fragpipe.tabs;

import com.dmtavt.fragpipe.tools.PSMValidation;
import com.dmtavt.fragpipe.tools.crystalc.CrystalcPanel;
import com.dmtavt.fragpipe.tools.msbooster.MSBoosterPanel;
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
    MSBoosterPanel msboosterPanel = new MSBoosterPanel();
    PtmProphetPanel panelPtmProphet = new PtmProphetPanel();
    ProtProphPanel panelProtProph = new ProtProphPanel();
    ReportPanel panelReport = new ReportPanel();

    mu.add(this, panelCrystalc).growX().wrap();
    mu.add(this, msboosterPanel).growX().wrap();
    mu.add(this, psmValidation).growX().wrap();
    mu.add(this, panelPtmProphet).growX().wrap();
    mu.add(this, panelProtProph).growX().wrap();
    mu.add(this, panelReport).growX().wrap();
  }
}
