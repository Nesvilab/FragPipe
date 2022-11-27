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

package com.dmtavt.fragpipe.tools.downstream;

import static com.dmtavt.fragpipe.Fragpipe.fe;
import static com.github.chhh.utils.SwingUtils.createClickableHtml;

import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.swing.FormEntry;
import com.github.chhh.utils.swing.HtmlStyledJEditorPane;
import com.github.chhh.utils.swing.JPanelBase;
import com.github.chhh.utils.swing.UiCheck;
import com.github.chhh.utils.swing.UiSpinnerInt;
import com.github.chhh.utils.swing.UiText;
import com.github.chhh.utils.swing.UiUtils;
import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.ItemSelectable;
import java.awt.image.BufferedImage;
import java.util.Objects;
import javax.imageio.ImageIO;
import javax.swing.ImageIcon;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.border.TitledBorder;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;


public class SaintexpressPanel extends JPanelBase {

  private static final String PREFIX = "saintexpress.";

  private JCheckBox checkRun;
  private JPanel pContent;
  private JPanel pTop;
  private UiText uiTextCmdOpts;
  private UiSpinnerInt uiSpinnerMaxReplicates;
  private UiSpinnerInt uiSpinVirtualControls;

  @Override
  protected void initMore() {
    super.initMore();
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

  private JPanel createPanelTop() {
    // setting the insets allows the top panel to be shifted left of the options panel
    JPanel p = new JPanel(new MigLayout(new LC()));
    mu.borderEmpty(p);

    checkRun = new UiCheck("Run SAINTexpress", null, false);
    checkRun.setName("run-saint-express");

    String message = "Please cite <a href=\"https://doi.org/10.1016/j.jprot.2013.10.023\">\"Teo, G., et al. SAINTexpress: improvements and additional features in Significance Analysis of INTeractome software. J Proteomics, 100:37 (2014).\"</a>";

    HtmlStyledJEditorPane messagePane = createClickableHtml(message);

    JLabel imageLabel = new JLabel();
    try {
      BufferedImage image = ImageIO.read(Objects.requireNonNull(getClass().getResource("/com/dmtavt/fragpipe/icons/icon-saint-100.png")));
      imageLabel = new JLabel(new ImageIcon(image));
    } catch (Exception ex) {
      ex.printStackTrace();
    }

    mu.add(p, checkRun).wrap();
    mu.add(p, messagePane).pushX();
    mu.add(p, imageLabel).gapRight("40").wrap();

    return p;
  }

  private JPanel createPanelContent() {
    JPanel p = new JPanel(new MigLayout(new LC().fillX()));
    p.setBorder(new TitledBorder("Options"));

    uiSpinnerMaxReplicates = UiUtils.spinnerInt(10, 0, 100, 1).setCols(5).create();
    FormEntry feMaxReplicates = mu.feb(uiSpinnerMaxReplicates).name("max-replicates").label("Max number of replicates (-R)").create();

    uiSpinVirtualControls = UiUtils.spinnerInt(100, 0, 1000, 1).setCols(5).create();
    FormEntry feVirtualControls = mu.feb(uiSpinVirtualControls).name("virtual-controls").label("Number of virtual controls (-L)").create();

    uiTextCmdOpts = UiUtils.uiTextBuilder().cols(20).text("").create();
    FormEntry feCmdOpts = fe(uiTextCmdOpts, "cmd-opts")
        .label("Cmd line opts:")
        .tooltip("These options will be passed on to SAINTexpress.\n"
            + "See output log (e.g. dry-run results) for the complete command.").create();


    mu.add(p, feMaxReplicates.label(), mu.ccL());
    mu.add(p, feMaxReplicates.comp).split(3);
    mu.add(p, feVirtualControls.label(), mu.ccL());
    mu.add(p, feVirtualControls.comp).wrap();
    mu.add(p, feCmdOpts.label(), mu.ccL());
    mu.add(p, feCmdOpts.comp).growX().pushX().wrap();

    updateEnabledStatus(p, true);

    return p;
  }

  @Override
  protected void init() {
    this.setLayout(new BorderLayout());
    this.setBorder(new TitledBorder("SAINTexpress"));

    pTop = createPanelTop();
    pContent = createPanelContent();

    this.add(pTop, BorderLayout.NORTH);
    this.add(pContent, BorderLayout.CENTER);
  }

  public boolean isRunSaintexpress() {
    return SwingUtils.isEnabledAndChecked(checkRun);
  }

  public int getMaxReplicates() {
    return uiSpinnerMaxReplicates.getActualValue();
  }

  public int getVirtualControls() {
    return uiSpinVirtualControls.getActualValue();
  }

  public String getCmdOpts() {
    return uiTextCmdOpts.getNonGhostText().trim();
  }
}