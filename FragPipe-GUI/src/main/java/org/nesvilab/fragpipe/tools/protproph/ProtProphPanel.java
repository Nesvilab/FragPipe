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

package org.nesvilab.fragpipe.tools.protproph;

import org.nesvilab.fragpipe.Fragpipe;
import org.nesvilab.fragpipe.messages.MessageSearchType;
import org.nesvilab.fragpipe.params.ThisAppProps;
import org.nesvilab.utils.SwingUtils;
import org.nesvilab.utils.swing.FormEntry;
import org.nesvilab.utils.swing.JPanelBase;
import org.nesvilab.utils.swing.UiCheck;
import org.nesvilab.utils.swing.UiText;
import org.nesvilab.utils.swing.UiUtils;
import java.awt.Component;
import java.awt.ItemSelectable;
import javax.swing.JButton;
import javax.swing.JPanel;
import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.ThreadMode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ProtProphPanel extends JPanelBase {
  private static final Logger log = LoggerFactory.getLogger(ProtProphPanel.class);
  public static final String PREFIX = "protein-prophet.";
  private UiCheck checkRun;
  private JButton btnAllowMassShifted;
  private JButton btnDisallowMassShifted;
  private UiText uiTextCmdOpts;
  private JPanel pTop;
  private JPanel pContent;

  public boolean isRun() {
    return SwingUtils.isEnabledAndChecked(checkRun);
  }

  public String getCmdOpts() {
    return uiTextCmdOpts.getNonGhostText().trim();
  }

  public void setRunStatus(boolean status) {
    checkRun.setEnabled(status);
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

  private void loadDefaults(String type) {
    String v = Fragpipe.getPropFix(ThisAppProps.PROP_TEXT_CMD_PROTEIN_PROPHET, type);
    if (v == null) {
      throw new IllegalStateException("No property found for key: " + ThisAppProps.PROP_TEXT_CMD_PROTEIN_PROPHET + "." + type);
    }
    uiTextCmdOpts.setText(v);
  }

  @Override
  public void init() {
    checkRun = UiUtils.createUiCheck("Run ProteinProphet", true);
    checkRun.setName("run-protein-prophet");
    // btnAllowMassShifted = UiUtils.createButton("Allow mass shifted peptides", e -> {
    //   log.debug("Clicked button " + btnAllowMassShifted.getText());
    //   loadDefaults("open");
    // });
    // btnDisallowMassShifted = UiUtils.createButton("Do NOT allow mass shifted peptides", e -> {
    //   log.debug("Clicked button " + btnDisallowMassShifted.getText());
    //   loadDefaults("tight");
    // });
    uiTextCmdOpts = UiUtils.uiTextBuilder().cols(20).text(defaultCmdOpt()).create();
    FormEntry feCmdOpts = mu.feb("cmd-opts", uiTextCmdOpts).label("Cmd line opts:").create();

    mu.layout(this, mu.lcFillXNoInsetsTopBottom());
    mu.border(this, "Protein inference");

    pTop = mu.newPanel(null, mu.lcFillXNoInsetsTopBottom());
    mu.add(pTop, checkRun).wrap();
    // mu.add(pTop, btnAllowMassShifted);
    // mu.add(pTop, btnDisallowMassShifted).wrap();

    pContent = mu.newPanel(null, mu.lcFillXNoInsetsTopBottom());
    mu.add(pContent, feCmdOpts.label()).alignX("right");
    mu.add(pContent, feCmdOpts.comp).growX().pushX().wrap();

    mu.add(this, pTop).growX().wrap();
    mu.add(this, pContent).growX().wrap();
  }

  private String defaultCmdOpt() {
    return Fragpipe.getPropFix(ThisAppProps.PROP_TEXT_CMD_PROTEIN_PROPHET, "open");
  }

  @Override
  public void initMore() {
    updateEnabledStatus(this, true); // will get enabled when Philosopher is selected
    super.initMore();
  }

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void on(MessageSearchType m) {
    checkRun.setSelected(true);
    loadDefaults(m.type.name());
  }
}
