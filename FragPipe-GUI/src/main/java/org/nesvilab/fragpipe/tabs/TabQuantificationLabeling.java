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

import org.nesvilab.fragpipe.api.Bus;
import org.nesvilab.fragpipe.tools.tmtintegrator.TmtiPanel;
import org.nesvilab.utils.swing.JPanelWithEnablement;
import org.nesvilab.utils.swing.MigUtils;

public class TabQuantificationLabeling extends JPanelWithEnablement {
  private static MigUtils mu = MigUtils.get();
  public static final String TAB_PREFIX = "quant-labeling.";
  public TmtiPanel panelTmtI;

  public TabQuantificationLabeling() {
    init();
    initMore();
  }

  private void initMore() {
    Bus.registerQuietly(this);
    Bus.postSticky(this);
  }

  private void init() {
    mu.layout(this).fillX();

    panelTmtI = new TmtiPanel();

    mu.add(this, panelTmtI).growX().wrap();
  }

}
