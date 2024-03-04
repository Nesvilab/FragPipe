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

package com.dmtavt.fragpipe.tools.diapasefscentric;

import static com.github.chhh.utils.SwingUtils.isEnabledAndChecked;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.api.Bus;
import com.dmtavt.fragpipe.messages.NoteConfigCrystalC;
import com.dmtavt.fragpipe.messages.NoteConfigPeptideProphet;
import com.dmtavt.fragpipe.messages.NoteConfigPtmProphet;
import com.dmtavt.fragpipe.messages.NoteConfigPtmShepherd;
import com.dmtavt.fragpipe.messages.NoteConfigTmtI;
import com.dmtavt.fragpipe.messages.NoteConfigUmpire;
import com.dmtavt.fragpipe.tabs.TabWorkflow;
import com.github.chhh.utils.swing.FormEntry;
import com.github.chhh.utils.swing.JPanelBase;
import com.github.chhh.utils.swing.UiCheck;
import com.github.chhh.utils.swing.UiSpinnerDouble;
import com.github.chhh.utils.swing.UiSpinnerInt;
import java.awt.Component;
import java.awt.ItemSelectable;
import java.text.DecimalFormat;
import javax.swing.JCheckBox;
import javax.swing.JPanel;
import javax.swing.border.TitledBorder;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.ThreadMode;

public class DiaPasefSCentricPanel extends JPanelBase {

  public static final String PREFIX = "diapasefscentric.";

  public JCheckBox checkRunDiaPasefSCentric;
  private JPanel pTop;
  private JPanel p;
  private UiCheck uiCheckWriteIntermediateFiles;
  private UiSpinnerDouble uiSpinnerApexIM;
  private UiSpinnerInt uiSpinnerApexRT;
  private UiCheck uiCheckMassDefectFilter;
  private UiSpinnerDouble uiSpinnerMassDefectOffset;
  private UiSpinnerDouble uiSpinnerMs1MS2Corr;
  private UiSpinnerInt uiSpinnerRFMax;

  public DiaPasefSCentricPanel() {
    Bus.postSticky(this);
  }

  @Override
  public boolean isRun() {
    return isEnabledAndChecked(checkRunDiaPasefSCentric);
  }

  @Subscribe(sticky = true, threadMode = ThreadMode.MAIN_ORDERED)
  public void on(NoteConfigUmpire m) {
    updateEnabledStatus(this, m.isValid());
  }

  protected void init() {
    this.setLayout(new MigLayout(new LC().flowY().fillX()));
    this.setBorder(new TitledBorder("diaPASEF spectrum deconvolution"));

    pTop = mu.newPanel(mu.lcFillXNoInsetsTopBottom());
    p = mu.newPanel(mu.lcFillXNoInsetsTopBottom());

    checkRunDiaPasefSCentric = new UiCheck("Run diaPASEF spectrum deconvolution", null, false);
    checkRunDiaPasefSCentric.setName(PREFIX + "run-diapasefscentric");
    pTop.add(checkRunDiaPasefSCentric, new CC().spanX().wrap());

    checkRunDiaPasefSCentric.addItemListener(e -> {
      if (isRun()) {
        Bus.post(new NoteConfigCrystalC(true));
        Bus.post(new NoteConfigPeptideProphet(true));
        Bus.post(new NoteConfigPtmProphet(true));
        Bus.post(new NoteConfigPtmShepherd(true));
      } else {
        TabWorkflow tabWorkflow = Fragpipe.getStickyStrict(TabWorkflow.class);
        if (tabWorkflow.hasDataType("DIA") || tabWorkflow.hasDataType("GPF-DIA") || tabWorkflow.hasDataType("DIA-Lib")) {
          Bus.post(new NoteConfigCrystalC(false));
          Bus.post(new NoteConfigPeptideProphet(false));
          Bus.post(new NoteConfigPtmProphet(false));
          Bus.post(new NoteConfigPtmShepherd(false));
          Bus.post(new NoteConfigTmtI(false));
        } else if (tabWorkflow.hasDataType("DDA+")) {
          Bus.post(new NoteConfigCrystalC(false));
          Bus.post(new NoteConfigPeptideProphet(false));
          Bus.post(new NoteConfigPtmProphet(true));
          Bus.post(new NoteConfigPtmShepherd(false));
          Bus.post(new NoteConfigTmtI(false));
        }
      }
    });

    uiCheckWriteIntermediateFiles = new UiCheck("Write intermediate files", null, false);
    uiSpinnerApexIM = new UiSpinnerDouble(0.01, 0.01, 10, 0.01, new DecimalFormat("0.##"));
    uiSpinnerApexRT = new UiSpinnerInt(3, 1, 100, 1);
    uiCheckMassDefectFilter = new UiCheck("Mass Defect Filter", null, true);
    uiSpinnerMassDefectOffset = new UiSpinnerDouble(0.1, 0, 10, 0.1, new DecimalFormat("0.#"));
    uiSpinnerMassDefectOffset.setColumns(3);
    uiSpinnerMs1MS2Corr = new UiSpinnerDouble(0.3, 0.0, 1.0, 0.1, new DecimalFormat("0.#"));
    uiSpinnerMs1MS2Corr.setColumns(3);
    uiSpinnerRFMax = new UiSpinnerInt(500, 1, 9999, 1, 4);

    FormEntry feWriteIntermediateFiles = mu.feb("write-intermediate-files", uiCheckWriteIntermediateFiles)
        .label("Write intermediate files")
        .create();
    FormEntry feApexIm = mu.feb("delta-apex-im", uiSpinnerApexIM)
        .label("Delta Apex IM")
        .create();
    FormEntry feApexRt = mu.feb("delta-apex-rt", uiSpinnerApexRT)
        .label("Delta Apex RT")
        .create();
    FormEntry feMassDefectFilter = mu.feb("mass-defect-filter", uiCheckMassDefectFilter)
        .label("Mass Defect Filter")
        .create();
    FormEntry feMassDefectOffset = mu.feb("mass-defect-offset", uiSpinnerMassDefectOffset)
        .label("Mass Defect Offset")
        .create();
    FormEntry feMs1MS2Corr = mu.feb("corr-threshold", uiSpinnerMs1MS2Corr)
        .label("Corr Threshold")
        .create();
    FormEntry feRFMax = mu.feb("rf-max", uiSpinnerRFMax)
        .label("RF max")
        .create();

    mu.add(p, feApexIm.label()).alignX("right").split(2);
    mu.add(p, feApexIm.comp);
    mu.add(p, feApexRt.label()).alignX("right").split(2);
    mu.add(p, feApexRt.comp);
    mu.add(p, feWriteIntermediateFiles.comp).growX().wrap();
    mu.add(p, feRFMax.label()).alignX("right").split(2);
    mu.add(p, feRFMax.comp);
    mu.add(p, feMs1MS2Corr.label()).alignX("right").split(2);
    mu.add(p, feMs1MS2Corr.comp);
    mu.add(p, feMassDefectFilter.comp).alignX("right").split(3);
    mu.add(p, feMassDefectOffset.label());
    mu.add(p, feMassDefectOffset.comp);

    mu.add(pTop, p);

    this.add(pTop, new CC().growX().wrap());
  }

  public boolean writeIntermediateFiles() {
    return uiCheckWriteIntermediateFiles.isSelected();
  }

  public float imTolerance() {
    return (float) uiSpinnerApexIM.getActualValue();
  }

  public int apexScanDeltaRange() {
    return uiSpinnerApexRT.getActualValue();
  }

  public boolean massDefectFilter() {
    return uiCheckMassDefectFilter.isSelected();
  }

  public float massDefectOffset() {
    return (float) uiSpinnerMassDefectOffset.getActualValue();
  }

  public float ms1MS2Corr() {
    return (float) uiSpinnerMs1MS2Corr.getActualValue();
  }

  public int topNPeaks() {
    return uiSpinnerRFMax.getActualValue();
  }


  @Override
  protected ItemSelectable getRunCheckbox() {
    return checkRunDiaPasefSCentric;
  }

  @Override
  protected Component getEnablementToggleComponent() {
    return p;
  }

  @Override
  protected String getComponentNamePrefix() {
    return PREFIX;
  }
}
