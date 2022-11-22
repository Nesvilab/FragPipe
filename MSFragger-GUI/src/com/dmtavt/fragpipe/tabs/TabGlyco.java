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

import com.dmtavt.fragpipe.tools.opair.OPairPanel;
import com.dmtavt.fragpipe.tools.ptmshepherd.PTMSGlycanAssignPanel;
import com.github.chhh.utils.swing.*;
import com.dmtavt.fragpipe.api.Bus;


public class TabGlyco extends JPanelWithEnablement {
    private static MigUtils mu = MigUtils.get();
    private OPairPanel panelOPair;
    private PTMSGlycanAssignPanel panelGlycanAssign;

    public TabGlyco() {
        init();
        initMore();
    }

    private void init() {
        mu.layout(this).fillX();

        panelGlycanAssign = new PTMSGlycanAssignPanel();
        panelOPair = new OPairPanel();

        mu.add(this, panelGlycanAssign).spanX().growX().wrap();
        mu.add(this, panelOPair).spanX().growX().wrap();
    }

    private void initMore() {
        Bus.registerQuietly(this);
        Bus.postSticky(this);

    }

}

