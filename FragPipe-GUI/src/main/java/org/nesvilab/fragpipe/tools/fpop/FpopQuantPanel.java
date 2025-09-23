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

package org.nesvilab.fragpipe.tools.fpop;

import static org.nesvilab.fragpipe.Version.PROGRAM_TITLE;

import org.nesvilab.utils.SwingUtils;

import javax.swing.*;
import javax.swing.border.TitledBorder;
import java.awt.*;
import org.nesvilab.utils.swing.FormEntry;
import org.nesvilab.utils.swing.JPanelBase;
import org.nesvilab.utils.swing.UiCheck;
import org.nesvilab.utils.swing.UiSpinnerInt;
import org.nesvilab.utils.swing.UiText;
import org.nesvilab.utils.swing.UiUtils;

public class FpopQuantPanel extends JPanelBase {

    private UiCheck checkFPOP;
    private UiCheck checkSubtractControl;
    private UiCheck uiCheckIsTmt;
    private UiText uiTextControl;
    private UiText uiTextFPOP;
    private UiSpinnerInt uiSpinnerRegionSize;
    private static final String PREFIX = "fpop.fragpipe.";
    private JPanel pTop;
    private JPanel pContent;

    @Override
    protected void init() {
        this.setLayout(new BorderLayout());
        this.setBorder(new TitledBorder("FPOP " + PROGRAM_TITLE + " quant"));

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

    public boolean isRun() {
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
    public boolean isFpopTmt() {return uiCheckIsTmt.isSelected(); }

    private JPanel createPanelTop() {
        JPanel p = mu.newPanel(null, mu.lcFillXNoInsetsTopBottom());

        checkFPOP = UiUtils.createUiCheck("Run FPOP-specific quantitation", false);
        checkFPOP.setName("fpop.run-fpop");
        checkFPOP.setToolTipText("Run downstream processing for FPOP analyses using the " + PROGRAM_TITLE + " method. Computes % oxidation for each peptide and site based on either LFQ or TMT quant tables.");

        mu.add(p, checkFPOP).wrap();
        return p;
    }


    private JPanel createPanelFPOP() {
        JPanel p = mu.newPanel(null, mu.lcFillXNoInsetsTopBottom());

        uiCheckIsTmt = UiUtils.createUiCheck("TMT FPOP analysis", false);
        uiCheckIsTmt.setName("fpop.fpop-tmt");
        uiCheckIsTmt.setToolTipText("Check if this FPOP data uses TMT for quantification. If unchecked, uses label free quant");

        checkSubtractControl = UiUtils.createUiCheck("Subtract control oxidation", false);
        checkSubtractControl.setName("fpop.subtract-control");
        checkSubtractControl.setToolTipText("Optional: automatically pair samples into experiment and control conditions and subtract control oxidation from experiment.\n" +
                " Requires that all LC-MS run names include an identifying string for either experiment or control conditions.");

        uiTextControl = UiUtils.uiTextBuilder().cols(15).create();
        FormEntry feControl = mu.feb(uiTextControl).name("fpop.label_control").label("  Control label").tooltip("Optional: only needed if using 'Subtract Control'. Label found in all control experiment/group names (to be subtracted from FPOP label)").create();
        uiTextFPOP = UiUtils.uiTextBuilder().cols(15).create();
        FormEntry feFPOP = mu.feb(uiTextFPOP).name("fpop.label_fpop").label("   FPOP Label").tooltip("Optional: only needed if using 'Subtract Control'. Label found in all sample experiment/group names").create();

        uiSpinnerRegionSize = UiUtils.spinnerInt(1, 1, 1000, 1).setCols(4).create();
        FormEntry feRegionSize = mu.feb(uiSpinnerRegionSize).name("fpop.region_size").label("          Site region size").tooltip("Number of amino acids to consider a group/region around a modified site").create();

        mu.add(p, uiCheckIsTmt).split();
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

