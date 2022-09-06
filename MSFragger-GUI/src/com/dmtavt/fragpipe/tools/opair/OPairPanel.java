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

package com.dmtavt.fragpipe.tools.opair;

import com.dmtavt.fragpipe.tools.enums.ActivationTypes;
import com.github.chhh.utils.swing.*;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.swing.JPanelBase;

import java.awt.Component;
import java.awt.ItemSelectable;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import javax.swing.JLabel;
import javax.swing.JPanel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class OPairPanel extends JPanelBase {
    private static final Logger log = LoggerFactory.getLogger(com.dmtavt.fragpipe.tools.opair.OPairPanel.class);
    private static final String PREFIX = "opair.";
    private JPanel pTop;
    private JPanel pContent;
    private static final MigUtils mu = MigUtils.get();
    private UiCheck checkRun;


    public OPairPanel() {
        super();
    }

    @Override
    protected ItemSelectable getRunCheckbox() {
        return checkRun;
    }

    @Override
    protected Component getEnablementToggleComponent() {
        return pContent;
    }

    @Override
    protected String getComponentNamePrefix() {
        return PREFIX;
    }

    public void setRunStatus(boolean status) {
        checkRun.setSelected(status);
    }

    @Override
    protected void init() {
        mu.layout(this, mu.lcFillXNoInsetsTopBottom());
        mu.border(this, "O-glycan Localization with O-Pair");

        pTop = createPanelTop();

        pContent = mu.newPanel(null, mu.lcFillXNoInsetsTopBottom());

        mu.add(this, pTop).growX().wrap();
        mu.add(this, pContent).growX().wrap();
    }

    private JPanel createPanelTop() {

        JPanel p = mu.newPanel(null, mu.lcFillXNoInsetsTopBottom());

        checkRun = new UiCheck("Run OPair", null, false);
        checkRun.setName("run-opair");
        JLabel info = new JLabel("<html>O-glycan localization with O-Pair. Requires <b>paired scan data</b>.");

        mu.add(p, checkRun).split();
        mu.add(p, info).gapLeft("80px").wrap();
        return p;
    }

    public boolean isRun() {
        return SwingUtils.isEnabledAndChecked(checkRun);
    }

    @Override
    public void initMore() {

        updateEnabledStatus(this, true);
        super.initMore();
    }
}
