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
import static com.github.chhh.utils.swing.UiUtils.createUiCombo;

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
import com.github.chhh.utils.swing.UiCombo;
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
  private UiSpinnerDouble uiSpinnerImTolerance;
  private UiCombo uiComboMzToleranceUnit;
  private UiSpinnerDouble uiSpinnerMzTolerance;
  private UiSpinnerInt uiSpinnerApexScanDeltaRange;
  private UiSpinnerDouble uiSpinnerImToleranceMs1Ms2;
  private UiSpinnerInt uiSpinnerApexScanDeltaRangeMs1Ms2;

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
    uiSpinnerImTolerance = new UiSpinnerDouble(0.02, 0.01, 10, 0.01, new DecimalFormat("0.##"));
    uiComboMzToleranceUnit = createUiCombo(new String[]{"PPM", "Th"});
    uiSpinnerMzTolerance = new UiSpinnerDouble(20, 0.001, 100, 0.01, new DecimalFormat("0.##"));
    uiSpinnerApexScanDeltaRange = new UiSpinnerInt(3, 1, 100, 1);
    uiSpinnerImToleranceMs1Ms2 = new UiSpinnerDouble(0.02, 0.01, 10, 0.01, new DecimalFormat("0.##"));
    uiSpinnerApexScanDeltaRangeMs1Ms2 = new UiSpinnerInt(5, 1, 100, 1);

    FormEntry feWriteIntermediateFiles = mu.feb("write-intermediate-files", uiCheckWriteIntermediateFiles)
        .label("Write intermediate files: ")
        .create();
    FormEntry feImTolerance = mu.feb("im-tolerance", uiSpinnerImTolerance)
        .label("Ion mobility tolerance: ")
        .create();
    FormEntry feMzToleranceUnit = mu.feb("mz-tolerance-unit", uiComboMzToleranceUnit)
        .label("MZ tolerance unit: ")
        .create();
    FormEntry feMzTolerance = mu.feb("mz-tolerance", uiSpinnerMzTolerance)
        .label("MZ tolerance: ")
        .create();
    FormEntry feApexScanDeltaRange = mu.feb("apex-scan-delta-range", uiSpinnerApexScanDeltaRange)
        .label("Apex scan delta range: ")
        .create();
    FormEntry feImToleranceMs1Ms2 = mu.feb("im-tolerance-ms1-ms2", uiSpinnerImToleranceMs1Ms2)
        .label("Ion mobility tolerance for MS1 and MS2 match: ")
        .create();
    FormEntry feApexScanDeltaRangeMs1Ms2 = mu.feb("apex-scan-delta-range-ms1-ms2", uiSpinnerApexScanDeltaRangeMs1Ms2)
        .label("Apex scan delta range for MS1 and MS2 match: ")
        .create();

    mu.add(p, feImTolerance.label()).alignX("right").split(2);
    mu.add(p, feImTolerance.comp);
    mu.add(p, feApexScanDeltaRange.label()).alignX("right").split(2);
    mu.add(p, feApexScanDeltaRange.comp);
    mu.add(p, feMzTolerance.label()).alignX("right").split(3);
    mu.add(p, feMzTolerance.comp);
    mu.add(p, feMzToleranceUnit.comp).wrap();
    mu.add(p, feImToleranceMs1Ms2.label()).alignX("right").split(2);
    mu.add(p, feImToleranceMs1Ms2.comp);
    mu.add(p, feApexScanDeltaRangeMs1Ms2.label()).alignX("right").split(2);
    mu.add(p, feApexScanDeltaRangeMs1Ms2.comp);
    mu.add(p, feWriteIntermediateFiles.comp);

    mu.add(pTop, p);

    this.add(pTop, new CC().growX().wrap());
  }

  public boolean writeIntermediateFiles() {
    return uiCheckWriteIntermediateFiles.isSelected();
  }

  public float imTolerance() {
    return (float) uiSpinnerImTolerance.getActualValue();
  }

  public int mzToleranceUnit() {
    if (uiComboMzToleranceUnit.getSelectedIndex() == 1) {
      return 0;
    } else {
      return 1;
    }
  }

  public float mzTolerance() {
    return (float) uiSpinnerMzTolerance.getActualValue();
  }

  public int apexScanDeltaRange() {
    return uiSpinnerApexScanDeltaRange.getActualValue();
  }

  public float imToleranceMs1Ms2() {
    return (float) uiSpinnerImToleranceMs1Ms2.getActualValue();
  }

  public int apexScanDeltaRangeMs1Ms2() {
    return uiSpinnerApexScanDeltaRangeMs1Ms2.getActualValue();
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
