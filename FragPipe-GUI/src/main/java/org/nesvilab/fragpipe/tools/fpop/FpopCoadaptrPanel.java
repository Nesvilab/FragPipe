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

import org.nesvilab.fragpipe.Fragpipe;
import org.nesvilab.utils.SwingUtils;
import org.nesvilab.utils.swing.FormEntry;
import org.nesvilab.utils.swing.JPanelBase;
import org.nesvilab.utils.swing.UiCheck;
import org.nesvilab.utils.swing.UiText;
import org.nesvilab.utils.swing.UiUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.*;
import javax.swing.border.TitledBorder;
import java.awt.*;
import java.util.ArrayList;

public class FpopCoadaptrPanel extends JPanelBase {

    private UiCheck checkPrepCoadaptr;
    private UiText uiTextFpopMasses;
    private static final String PREFIX = "fpop.coadaptr.";
    private JPanel pCoadaptrContent;
    private static final Logger log = LoggerFactory.getLogger(FpopCoadaptrPanel.class);

    @Override
    protected void init() {
        this.setLayout(new BorderLayout());
        this.setBorder(new TitledBorder("FPOP coADAPTr Prep"));

        JPanel pCoadaptrTop = createCoadaptrPanelTop();
        pCoadaptrContent = createPanelCoadaptrContent();

        this.add(pCoadaptrTop, BorderLayout.NORTH);
        this.add(pCoadaptrContent, BorderLayout.WEST);
    }

    @Override
    protected void initMore() {
        super.initMore();
    }

    @Override
    protected ItemSelectable getRunCheckbox() {
        return checkPrepCoadaptr;
    }

    @Override
    protected Component getEnablementToggleComponent() {
        return pCoadaptrContent;
    }

    @Override
    protected String getComponentNamePrefix() {
        return PREFIX;
    }

    public boolean isRun() {
        return SwingUtils.isEnabledAndChecked(checkPrepCoadaptr);
    }

    /**
     * Read the input from the text panel and make sure all entries can be parsed as doubles. Return a String
     * of the parsed values separated by ",".
     */
    public String getFpopMods() {
        String input = uiTextFpopMasses.getNonGhostText();
        String[] splits = input.split("[\\s,;/]+");
        ArrayList<String> output = new ArrayList<>();
        for (String split : splits) {
            try {
                double test = Double.parseDouble(split);
                output.add(String.format("%.4f", test));
            } catch (NumberFormatException ex) {
                if (Fragpipe.headless) {
                    log.error(String.format("Could not read FPOP mod mass %s as a number.", split));
                } else {
                    JOptionPane.showMessageDialog(this, String.format("Could not read FPOP mod mass %s as a number.", split), "Error in FPOP mods", JOptionPane.ERROR_MESSAGE);
                }
            }
        }
        return String.join(",", output);
    }


    private JPanel createCoadaptrPanelTop() {
        JPanel pCoadaptr = mu.newPanel(null, mu.lcFillXNoInsetsTopBottom());
        checkPrepCoadaptr = UiUtils.createUiCheck("Prepare output for coADAPTr", false);
        checkPrepCoadaptr.setName("fpop.run-fpop-coadaptr");
        checkPrepCoadaptr.setToolTipText("Prepares output psm.tsv table(s) for direct input to coADAPTr");

        mu.add(pCoadaptr, checkPrepCoadaptr).wrap();
        return pCoadaptr;
    }

    private JPanel createPanelCoadaptrContent() {
        JPanel p = mu.newPanel(null, mu.lcFillXNoInsetsTopBottom());

        uiTextFpopMasses = UiUtils.uiTextBuilder().cols(100).create();
        FormEntry feFpopMasses = mu.feb(uiTextFpopMasses).name("fpop.fpop_masses").label("FPOP Masses:").tooltip("Enter the modification masses for all FPOP mods to pass to coADAPTr").create();

        mu.add(p, feFpopMasses.label(), mu.ccR());
        mu.add(p, feFpopMasses.comp).wrap();

        return p;
    }


}

