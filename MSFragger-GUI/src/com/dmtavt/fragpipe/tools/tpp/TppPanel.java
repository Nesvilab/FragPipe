/*
 * JavaPanel class = GUI for TPP section in FragPipe. Sep 27th, 2023. Carolina Rojas Ramirez.
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

package com.dmtavt.fragpipe.tools.tpp;

import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.swing.*;

import javax.swing.*;
import javax.swing.border.TitledBorder;
import java.awt.*;

/*Define class - pretty much the same name as this class file */
public class TppPanel extends JPanelBase {

    /*Initializing variables*/
    /*checkTPP, check button to tell Fragpipe that TPP analysis will be run*/
    /*It used to be checkFPOP*/
    private UiCheck checkTPP;

    /*checkpreoneDTPP, check button to tell FragPipe that data needs to get ready for 1DAnalysis*/
    private UiCheck checkONEDTPPR;

    /*checkpreoneDTPP, check button to tell FragPipe that user is now ready to run 1DTPP (configuration file has been set)*/
    private UiCheck checkONEDTPP;

    /*check2DTPP, check button to tell FragPipe that TPP analysis will be run in 2D mode*/
    /* previously private UiCheck uiCheckIsTmt;*/
    private UiCheck checkTWODTPP;

    /*can this text box be used to get the path? For now, it will be used for the string of the R_HOME path*/
    /*previously uiTextControl*/
    private UiText uiTextRHOME;
    private static final String PREFIX = "tpp.";
    private JPanel pTop;
    private JPanel pContent;

    @Override
    protected void init() {
        this.setLayout(new BorderLayout());
        /*Name for the GUI section*/
        this.setBorder(new TitledBorder("Thermal Protein Profiling (TPP) Downstream Analysis"));

        pTop = createPanelTop();
        pContent = createPanelTPP();

        this.add(pTop, BorderLayout.NORTH);
        this.add(pContent, BorderLayout.CENTER);
    }

    @Override
    protected void initMore() {
        super.initMore();
    }

    @Override
    protected ItemSelectable getRunCheckbox() {
        return checkTPP;
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
        return SwingUtils.isEnabledAndChecked(checkTPP);
    }

    /*previously getFpopControlLabel*/
    public String getRHOME(){
        return uiTextRHOME.getNonGhostText();
    }
    public boolean isRunTpp() {return isRun(); }
    public boolean isoneDTppR() {return checkONEDTPPR.isSelected(); }
    public boolean isoneDTpp() {return checkONEDTPP.isSelected(); }
    public boolean istwoDTpp() {return checkTWODTPP.isSelected(); }

    private JPanel createPanelTop() {
        JPanel p = mu.newPanel(null, mu.lcFillXNoInsetsTopBottom());

        checkTPP = UiUtils.createUiCheck("Run Thermal Protein Profiling Analysis", false);
        checkTPP.setName("tpp.run-tpp");
        checkTPP.setToolTipText("Run downstream processing for TPP data. I can handle 1DTPP and 2DTPP datasets (1DTPP needs some preprocessing");

        mu.add(p, checkTPP).wrap();
        return p;
    }


    private JPanel createPanelTPP() {
        JPanel p = mu.newPanel(null, mu.lcFillXNoInsetsTopBottom());
        /*Full 1DTPP done by TPP-R*/
        checkONEDTPPR = UiUtils.createUiCheck("1DTPP (TPP-R)", false);
        checkONEDTPPR.setName("tpp.onetppr");
        checkONEDTPPR.setToolTipText("If check, it will perform 1DTPP analysis by TPP-R.");
        /*Full 1DTPP done by TP-MAP*/
        checkONEDTPP = UiUtils.createUiCheck("1DTPP (TP-MAP)", false);
        checkONEDTPP.setName("tpp.onedtpp");
        checkONEDTPP.setToolTipText("If check, it will perform 1DTPP analysis by TP-MAP." +
                "If check runs 1DTPP analysis.");
        /*Full 2DTPP done by TP-MAP*/
        checkTWODTPP = UiUtils.createUiCheck("2DTPP (TP-MAP)", false);
        checkTWODTPP.setName("tpp.twodtpp");
        checkTWODTPP.setToolTipText("If check, it will perform 2DTPP analysis by TP-MAP.\"");
        /*Path to local installation of R*/


        uiTextRHOME = UiUtils.uiTextBuilder().cols(15).create();
        FormEntry RHOME = mu.feb(uiTextRHOME).name("fpop.RHOMEPath").label("R local path").tooltip("Local installation of R.").create();

        mu.add(p, checkONEDTPPR).split();
        mu.add(p, checkONEDTPP).split();
        mu.add(p, checkTWODTPP).split();
        mu.add(p, RHOME.label(), mu.ccR());
        mu.add(p, RHOME.comp);


        return p;
    }

}

