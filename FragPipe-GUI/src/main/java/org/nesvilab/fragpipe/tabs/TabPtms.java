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

import org.nesvilab.utils.swing.JPanelWithEnablement;
import org.nesvilab.utils.swing.MigUtils;
import org.nesvilab.fragpipe.tools.ptmshepherd.PtmshepherdPanel;
import org.nesvilab.fragpipe.api.Bus;

public class TabPtms extends JPanelWithEnablement {
  private static MigUtils mu = MigUtils.get();
  public static final String TAB_PREFIX = "quantitation.";
  private PtmshepherdPanel panelPtmshepherd;

  public TabPtms() {
    init();
    initMore();
  }

  private void initMore() {
    Bus.registerQuietly(this);
    Bus.postSticky(this);
  }

  private void init() {
    mu.layout(this).fillX();

    panelPtmshepherd = new PtmshepherdPanel();

    mu.add(this, panelPtmshepherd).growX().wrap();
  }

  public boolean isRunShepherd() {
    return panelPtmshepherd.isRun();
  }
}
