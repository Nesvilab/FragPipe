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

package org.nesvilab.fragpipe.tabs;

import org.nesvilab.fragpipe.tools.PSMValidation;
import org.nesvilab.fragpipe.tools.crystalc.CrystalcPanel;
import org.nesvilab.fragpipe.tools.msbooster.MSBoosterPanel;
import org.nesvilab.fragpipe.tools.philosopher.ReportPanel;
import org.nesvilab.fragpipe.tools.protproph.ProtProphPanel;
import org.nesvilab.fragpipe.tools.ptmprophet.PtmProphetPanel;
import org.nesvilab.utils.SwingUtils;
import org.nesvilab.utils.swing.JPanelWithEnablement;
import org.nesvilab.utils.swing.MigUtils;
import org.nesvilab.utils.swing.UiCheck;
import java.awt.BorderLayout;
import java.awt.image.BufferedImage;
import java.util.Objects;
import javax.imageio.ImageIO;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.border.TitledBorder;

public class TabValidation extends JPanelWithEnablement {

  private static final MigUtils mu = MigUtils.get();
  private static UiCheck checkRun;

  public TabValidation() {
    init();
  }

  public void init() {
    this.setLayout(new BorderLayout());
    this.setBorder(new TitledBorder(""));

    JPanel pTop = mu.newPanel(mu.lcFillXNoInsetsTopBottom());
    mu.borderEmpty(pTop);

    JPanel pContent = mu.newPanel(mu.lcFillXNoInsetsTopBottom());
    mu.borderEmpty(pContent);

    checkRun = new UiCheck("Run Validation Tools", null, true);
    checkRun.setName("run-validation-tab");

    PSMValidation psmValidation = new PSMValidation();
    CrystalcPanel panelCrystalc = new CrystalcPanel();
    MSBoosterPanel msboosterPanel = new MSBoosterPanel();
    PtmProphetPanel panelPtmProphet = new PtmProphetPanel();
    ProtProphPanel panelProtProph = new ProtProphPanel();
    ReportPanel panelReport = new ReportPanel();

    JLabel imageLabel1 = new JLabel();
    JLabel imageLabel2 = new JLabel();
    JLabel imageLabel3 = new JLabel();
    try {
      BufferedImage image1 = ImageIO.read(Objects.requireNonNull(getClass().getResource("/org/nesvilab/fragpipe/icons/msbooster.png")));
      imageLabel1 = new JLabel(new ImageIcon(image1));
      BufferedImage image2 = ImageIO.read(Objects.requireNonNull(getClass().getResource("/org/nesvilab/fragpipe/icons/percolator.png")));
      imageLabel2 = new JLabel(new ImageIcon(image2));
      BufferedImage image3 = ImageIO.read(Objects.requireNonNull(getClass().getResource("/org/nesvilab/fragpipe/icons/philosopher_logo.png")));
      imageLabel3 = new JLabel(new ImageIcon(image3));
    } catch (Exception ex) {
      ex.printStackTrace();
    }

    mu.add(pTop, checkRun);
    mu.add(pTop, imageLabel1, mu.ccR()).split(3);
    mu.add(pTop, imageLabel2, mu.ccR());
    mu.add(pTop, imageLabel3, mu.ccR());

    mu.add(pContent, panelCrystalc).growX().wrap();
    mu.add(pContent, msboosterPanel).growX().wrap();
    mu.add(pContent, psmValidation).growX().wrap();
    mu.add(pContent, panelPtmProphet).growX().wrap();
    mu.add(pContent, panelProtProph).growX().wrap();
    mu.add(pContent, panelReport).growX().wrap();

    SwingUtils.setEnablementUpdater(this, pContent, checkRun);

    this.add(pTop, BorderLayout.NORTH);
    this.add(pContent, BorderLayout.CENTER);
  }
}
