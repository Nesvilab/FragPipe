package com.dmtavt.fragpipe.tools;

import com.dmtavt.fragpipe.tools.pepproph.PepProphPanel;
import com.dmtavt.fragpipe.util.PercolatorPanel;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.swing.JPanelBase;
import com.github.chhh.utils.swing.UiCheck;
import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.ItemSelectable;
import javax.swing.ButtonGroup;
import javax.swing.JPanel;
import javax.swing.JSeparator;
import javax.swing.SwingConstants;
import javax.swing.border.TitledBorder;

public class PSMValidation extends JPanelBase {

  private UiCheck checkRun;
  private JPanel p2;

  @Override
  protected ItemSelectable getRunCheckbox() {
    return checkRun;
  }

  @Override
  protected Component getEnablementToggleComponent() {
    return p2;
  }

  @Override
  protected String getComponentNamePrefix() {
    return null;
  }

  public boolean isRun() {
    return SwingUtils.isEnabledAndChecked(checkRun);
  }

  @Override
  protected void init() {
    this.setLayout(new BorderLayout());
    this.setBorder(new TitledBorder("PSM Validation"));

    JPanel p = mu.newPanel(null, mu.lcFillX());
    checkRun = new UiCheck("Run PSM Validation", null, true);
    checkRun.setName("run-psm-validation");
    mu.add(p, checkRun).growX().wrap();

    ButtonGroup radioGroup = new ButtonGroup();
    PercolatorPanel panelPercolator = new PercolatorPanel(radioGroup, isRun());
    PepProphPanel panelPepProph = new PepProphPanel(radioGroup, isRun());

    p2 = mu.newPanel(null, mu.lcFillX());
    mu.add(p2, panelPercolator).growX().wrap();
    mu.add(p2, new JSeparator(SwingConstants.HORIZONTAL)).growX().spanX().wrap();
    mu.add(p2, panelPepProph).growX().wrap();

    this.add(p, BorderLayout.NORTH);
    this.add(p2, BorderLayout.CENTER);
  }

}
