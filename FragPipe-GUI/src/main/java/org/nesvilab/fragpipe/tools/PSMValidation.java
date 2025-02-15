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

package org.nesvilab.fragpipe.tools;

import org.nesvilab.fragpipe.messages.MessageSearchType;
import org.nesvilab.fragpipe.tools.pepproph.PepProphPanel;
import org.nesvilab.fragpipe.tools.percolator.PercolatorPanel;
import org.nesvilab.utils.SwingUtils;
import org.nesvilab.utils.swing.JPanelBase;
import org.nesvilab.utils.swing.UiCheck;
import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.ItemSelectable;
import javax.swing.ButtonGroup;
import javax.swing.JPanel;
import javax.swing.JSeparator;
import javax.swing.SwingConstants;
import javax.swing.border.TitledBorder;
import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.ThreadMode;

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

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void on(MessageSearchType m) {
    checkRun.setSelected(true);
  }

  public void setRunStatus(boolean status) {
    checkRun.setEnabled(status);
  }

  @Override
  protected void init() {
    this.setLayout(new BorderLayout());
    this.setBorder(new TitledBorder("PSM Validation"));

    JPanel p = mu.newPanel(null, mu.lcFillXNoInsetsTopBottom());
    checkRun = new UiCheck("Run PSM Validation", null, true);
    checkRun.setName("run-psm-validation");
    mu.add(p, checkRun).growX().wrap();

    ButtonGroup radioGroup = new ButtonGroup();
    PercolatorPanel panelPercolator = new PercolatorPanel(radioGroup);
    PepProphPanel panelPepProph = new PepProphPanel(radioGroup);

    p2 = mu.newPanel(null, mu.lcFillXNoInsetsTopBottom());
    mu.add(p2, panelPepProph).growX().wrap();
    mu.add(p2, new JSeparator(SwingConstants.HORIZONTAL)).growX().spanX().wrap();
    mu.add(p2, panelPercolator).growX().wrap();

    this.add(p, BorderLayout.NORTH);
    this.add(p2, BorderLayout.CENTER);
  }
}
