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

import com.dmtavt.fragpipe.tools.umpire.UmpirePanel;
import com.github.chhh.utils.swing.JPanelWithEnablement;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;

public class TabUmpire extends JPanelWithEnablement {

  private UmpirePanel umpirePanel;

  public TabUmpire() {
    init();
    initMore();
  }

  private void initMore() {

  }

  private void init() {
    this.setLayout(new MigLayout(new LC().fillX()));
    umpirePanel = new UmpirePanel();
    add(umpirePanel, new CC().growX().wrap());
  }
}
