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

import com.dmtavt.fragpipe.tools.ptmprophet.PtmProphetPanel;
import com.github.chhh.utils.swing.JPanelWithEnablement;
import com.github.chhh.utils.swing.MigUtils;
import com.dmtavt.fragpipe.tools.ptmshepherd.PtmshepherdPanel;

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
