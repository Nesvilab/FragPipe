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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with FragPipe. If not, see <https://www.gnu.org/licenses/>.
 */

package org.nesvilab.fragpipe.tabs;

import static org.nesvilab.fragpipe.tabs.TabRun.mu;

import org.nesvilab.fragpipe.tools.diatracer.DiaTracerPanel;
import org.nesvilab.fragpipe.tools.umpire.UmpirePanel;
import org.nesvilab.utils.SwingUtils;
import org.nesvilab.utils.swing.JPanelWithEnablement;
import java.awt.Dimension;
import javax.swing.JEditorPane;
import javax.swing.JPanel;
import javax.swing.border.TitledBorder;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;

public class TabDiaPseudoMs2 extends JPanelWithEnablement {

  private UmpirePanel umpirePanel;
  private DiaTracerPanel diaTracerPanel;

  public TabDiaPseudoMs2() {
    init();
    initMore();
  }

  private void initMore() {

  }

  private void init() {
    this.setLayout(new MigLayout(new LC().fillX()));

    umpirePanel = new UmpirePanel();
    diaTracerPanel = new DiaTracerPanel();

    JPanel textPanel = new JPanel(new MigLayout());
    textPanel.setBorder(new TitledBorder("Notes"));
    JEditorPane epInfo = SwingUtils.createClickableHtml("<b>PTM searches:</b><br>"
        + "Change Mass Defect Filter to OFF<br><br>");
    epInfo.setPreferredSize(new Dimension(500, 50));
    textPanel.add(epInfo);

    mu.add(this, umpirePanel).growX().wrap();
    mu.add(this, diaTracerPanel).growX().wrap();
    mu.add(this, textPanel).growX().wrap();
  }
}
