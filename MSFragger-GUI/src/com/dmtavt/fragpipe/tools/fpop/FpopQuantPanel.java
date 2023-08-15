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

package com.dmtavt.fragpipe.tools.fpop;

import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.swing.*;

import javax.swing.*;
import javax.swing.border.TitledBorder;
import java.awt.*;

public class FpopQuantPanel extends JPanelBase {

    private UiCheck checkFPOP;
    private UiCheck checkSubtractControl;
    private UiText uiTextControl;
    private UiText uiTextFPOP;
    private UiSpinnerInt uiSpinnerRegionSize;
    private static final String PREFIX = "fpop.";
    private JPanel pTop;
    private JPanel pContent;

    @Override
    protected void init() {
        this.setLayout(new BorderLayout());
        this.setBorder(new TitledBorder("FPOP Quant"));

        pTop = createPanelTop();
        pContent = createPanelFPOP();

        this.add(pTop, BorderLayout.NORTH);
        this.add(pContent, BorderLayout.CENTER);
    }

    @Override
    protected void initMore() {
        super.initMore();
    }

    @Override
    protected ItemSelectable getRunCheckbox() {
        return checkFPOP;
    }

    @Override
    protected Component getEnablementToggleComponent() {
        return pContent;
    }

    @Override
    protected String getComponentNamePrefix() {
        return PREFIX;
    }

    protected boolean isRun() {
        return SwingUtils.isEnabledAndChecked(checkFPOP);
    }

    public String getFpopControlLabel(){
        return uiTextControl.getNonGhostText();
    }
    public String getFpopFpopLabel(){
        return uiTextFPOP.getNonGhostText();
    }
    public int getFpopRegionSize(){
        return uiSpinnerRegionSize.getActualValue();
    }
    public boolean getFpopSubtractControl() { return checkSubtractControl.isSelected(); }
    public boolean isRunFpopQuant() {return isRun(); }

    private JPanel createPanelTop() {
        JPanel p = mu.newPanel(null, mu.lcFillXNoInsetsTopBottom());

        checkFPOP = UiUtils.createUiCheck("Run FPOP-specific Quant", false);
        checkFPOP.setName("fpop.run-fpop");

        mu.add(p, checkFPOP).wrap();
        return p;
    }


    private JPanel createPanelFPOP() {
        JPanel p = mu.newPanel(null, mu.lcFillXNoInsetsTopBottom());

        checkSubtractControl = UiUtils.createUiCheck("Subtract Control Oxidation", true);
        checkSubtractControl.setName("fpop.subtract-control");

        uiTextControl = UiUtils.uiTextBuilder().cols(15).create();
        FormEntry feControl = mu.feb(uiTextControl).name("fpop.label_control").label("  Control Label").tooltip("Label found in all control (non-FPOP) experiment/group names").create();
        uiTextFPOP = UiUtils.uiTextBuilder().cols(15).create();
        FormEntry feFPOP = mu.feb(uiTextFPOP).name("fpop.label_fpop").label("   FPOP Label").tooltip("Label found in all FPOP experiment/group names").create();

        uiSpinnerRegionSize = UiUtils.spinnerInt(1, 1, 1000, 1).setCols(4).create();
        FormEntry feRegionSize = mu.feb(uiSpinnerRegionSize).name("fpop.region_size").label("          Site Region Size").tooltip("Number of amino acids to consider a group/region around a modified site").create();

        mu.add(p, checkSubtractControl).split();
        mu.add(p, feControl.label(), mu.ccR());
        mu.add(p, feControl.comp);
        mu.add(p, feFPOP.label(), mu.ccR());
        mu.add(p, feFPOP.comp);
        mu.add(p, feRegionSize.label(), mu.ccR());
        mu.add(p, feRegionSize.comp);

        return p;
    }

}

