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
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.swing.JPanelWithEnablement;
import com.github.chhh.utils.swing.MigUtils;
import com.github.chhh.utils.swing.UiCheck;
import java.awt.BorderLayout;
import javax.swing.JPanel;
import javax.swing.border.TitledBorder;

public class TabValidation extends JPanelWithEnablement {

  private static final MigUtils mu = MigUtils.get();
  private static UiCheck checkRun;

  public TabValidation() {
    init();
  }

  public void init() {
    this.setLayout(new BorderLayout());
    this.setBorder(new TitledBorder(""));

    JPanel pTop = mu.newPanel(mu.lcFillXNoInsetsTopBottom());
    mu.borderEmpty(pTop);

    JPanel pContent = mu.newPanel(mu.lcFillXNoInsetsTopBottom());
    mu.borderEmpty(pContent);

    checkRun = new UiCheck("Run Validation Tools", null, true);
    checkRun.setName("run-validation-tab");

    PSMValidation psmValidation = new PSMValidation();
    CrystalcPanel panelCrystalc = new CrystalcPanel();
    MSBoosterPanel msboosterPanel = new MSBoosterPanel();
    PtmProphetPanel panelPtmProphet = new PtmProphetPanel();
    ProtProphPanel panelProtProph = new ProtProphPanel();
    ReportPanel panelReport = new ReportPanel();

    mu.add(pTop, checkRun).wrap();

    mu.add(pContent, panelCrystalc).growX().wrap();
    mu.add(pContent, msboosterPanel).growX().wrap();
    mu.add(pContent, psmValidation).growX().wrap();
    mu.add(pContent, panelPtmProphet).growX().wrap();
    mu.add(pContent, panelProtProph).growX().wrap();
    mu.add(pContent, panelReport).growX().wrap();

    SwingUtils.setEnablementUpdater(this, pContent, checkRun);

    this.add(pTop, BorderLayout.NORTH);
    this.add(pContent, BorderLayout.CENTER);
  }
}
