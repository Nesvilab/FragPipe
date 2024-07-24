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

package com.dmtavt.fragpipe.dialogs;

import static com.dmtavt.fragpipe.tabs.TabConfig.userEmail;
import static com.dmtavt.fragpipe.tabs.TabConfig.userInstitution;
import static com.dmtavt.fragpipe.tabs.TabConfig.userName;

import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.swing.FormEntry;
import com.github.chhh.utils.swing.MigUtils;
import com.github.chhh.utils.swing.UiCheck;
import com.github.chhh.utils.swing.UiText;
import com.github.chhh.utils.swing.UiUtils;
import java.awt.BorderLayout;
import java.awt.Dimension;
import javax.swing.JCheckBox;
import javax.swing.JEditorPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import net.miginfocom.swing.MigLayout;

public class DownloadToolsPanel extends JPanel {

  private final MigUtils mu = MigUtils.get();

  private FormEntry feName;
  private FormEntry feEmail;
  private FormEntry feInstitution;
  private JCheckBox license2;
  private JCheckBox license3;
  private JCheckBox license1;
  private JCheckBox receiveEmail;
  private UiCheck uiCheckDownloadMSFragger;
  private UiCheck uiCheckDownloadIonQuant;
  private UiCheck uiCheckDownloadDiaTracer;

  public DownloadToolsPanel() {
    initMore();
  }

  @Override
  public Dimension getPreferredSize() {
    return new Dimension(500, 550);
  }

  private void initMore() {
    JPanel panelTextboxes = new JPanel();

    panelTextboxes.setLayout(new MigLayout(mu.lcFillXNoInsetsTopBottom()));

    JEditorPane t0 = SwingUtils.createClickableHtml(
        "<br>MSFragger, IonQuant, and diaTracer software are available freely for academic research,<br>"
            + "non-commercial or educational purposes under academic license.<br><br>"
            + "Read the academic license for "
            + "<a href=\"https://msfragger-upgrader.nesvilab.org/upgrader/MSFragger-LICENSE.pdf\" target=\"blank_\">MSFragger</a>, "
            + "<a href=\"https://msfragger-upgrader.nesvilab.org/ionquant/IonQuant%20Academic%20Use%20License%2005162022.pdf\" target=\"blank_\">IonQuant</a>, and "
            + "<a href=\"https://msfragger-upgrader.nesvilab.org/diatracer/diaTracer%20UM%20%23%202024-417%20Academic%20Research%20Use%20License%2005142024.pdf\" target=\"blank_\">diaTracer</a>.<br><br>");

    feName = mu.feb(UiUtils.uiTextBuilder().cols(40).create()).label("Name:").create();
    feEmail = mu.feb(UiUtils.uiTextBuilder().cols(40).create()).label("Email:").create();
    feInstitution = mu.feb(UiUtils.uiTextBuilder().cols(40).create()).label("Institution").create();

    if (userName != null) {
      ((UiText) feName.comp).setText(userName);
    }
    if (userEmail != null) {
      ((UiText) feEmail.comp).setText(userEmail);
    }
    if(userInstitution != null) {
      ((UiText) feInstitution.comp).setText(userInstitution);
    }

    uiCheckDownloadMSFragger = new UiCheck("", null, true);
    uiCheckDownloadIonQuant = new UiCheck("", null, true);
    uiCheckDownloadDiaTracer = new UiCheck("", null, true);

    FormEntry feDownloadMSFragger = mu.feb(uiCheckDownloadMSFragger).label("Download MSFragger").create();
    FormEntry feDownloadIonQuant = mu.feb(uiCheckDownloadIonQuant).label("Download IonQuant").create();
    FormEntry feDownloadDiaTracer = mu.feb(uiCheckDownloadDiaTracer).label("Download diaTracer").create();

    JEditorPane t1 = SwingUtils.createClickableHtml("I have read the academic licenses. I understand that the licenses provide<br>"
        + "with a non-exclusive, non-transferable right to use the tools solely for<br>"
        + "academic research, non-commercial or educational purposes within the<br>"
        + "licensee’s department. If I am a non-academic user, I will contact the<br>"
        + "University of Michigan Office of Technology Transfer (Drew Bennett,<br>"
        + "andbenne@umich.edu) to obtain a commercial license to use the tools.");
    license1 = new JCheckBox();

    JEditorPane t2 = SwingUtils.createClickableHtml("I agree to the terms of <a href=\"https://msfragger-upgrader.nesvilab.org/upgrader/RawFileRdr_License_Agreement_RevA.pdf\" target=\"blank_\">Thermo (c) Raw File Reader License Agreement</a>.");
    license2 = new JCheckBox();

    JEditorPane t3 = SwingUtils.createClickableHtml("I agree to the terms of <a href=\"https://msfragger-upgrader.nesvilab.org/upgrader/EULA%20TDF-SDK.pdf\" target=\"blank_\">Bruker SDK library distribution conditions</a>.");
    license3 = new JCheckBox();

    JEditorPane t4 = SwingUtils.createClickableHtml("I would like to receive emails with updates in the future.");
    receiveEmail = new JCheckBox();

    mu.add(panelTextboxes, t0).split().spanX().wrap();

    mu.add(panelTextboxes, feName.label(), mu.ccR());
    mu.add(panelTextboxes, feName.comp).spanX().wrap();

    mu.add(panelTextboxes, feEmail.label(), mu.ccR());
    mu.add(panelTextboxes, feEmail.comp).spanX().wrap();

    mu.add(panelTextboxes, feInstitution.label(), mu.ccR());
    mu.add(panelTextboxes, feInstitution.comp).spanX().wrap();

    mu.add(panelTextboxes, feDownloadMSFragger.comp, mu.ccR());
    mu.add(panelTextboxes, feDownloadMSFragger.label()).wrap();

    mu.add(panelTextboxes, feDownloadIonQuant.comp, mu.ccR());
    mu.add(panelTextboxes, feDownloadIonQuant.label()).wrap();

    mu.add(panelTextboxes, feDownloadDiaTracer.comp, mu.ccR());
    mu.add(panelTextboxes, feDownloadDiaTracer.label()).wrap();

    mu.add(panelTextboxes, license1, mu.ccR());
    mu.add(panelTextboxes, t1).spanX().wrap();

    mu.add(panelTextboxes, license2, mu.ccR());
    mu.add(panelTextboxes, t2).spanX().wrap();

    mu.add(panelTextboxes, license3, mu.ccR());
    mu.add(panelTextboxes, t3).spanX().wrap();

    mu.add(panelTextboxes, receiveEmail, mu.ccR());
    mu.add(panelTextboxes, t4).spanX().wrap();

    JScrollPane scroll = new JScrollPane(panelTextboxes, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

    this.setLayout(new BorderLayout());
    this.add(scroll, BorderLayout.CENTER);
  }

  public String getName() {
    if (feName == null || feName.comp == null || ((UiText) feName.comp).getText() == null) {
      return null;
    } else {
      return ((UiText) feName.comp).getText().trim();
    }
  }

  public String getEmail() {
    if (feEmail == null || feEmail.comp == null || ((UiText) feEmail.comp).getText() == null) {
      return null;
    } else {
      return ((UiText) feEmail.comp).getText().trim();
    }
  }

  public String getInstitution() {
    if (feInstitution == null || feInstitution.comp == null || ((UiText) feInstitution.comp).getText() == null) {
      return null;
    } else {
      return ((UiText) feInstitution.comp).getText().trim();
    }
  }

  public boolean licensesChecked() {
    if (license1 == null || license2 == null || license3 == null) {
      return false;
    } else {
      return license1.isSelected() && license2.isSelected() && license3.isSelected();
    }
  }

  public boolean wantReceiveEmail() {
    if (receiveEmail == null) {
      return false;
    } else {
      return receiveEmail.isSelected();
    }
  }

  public boolean downloadMSFragger() {
    return uiCheckDownloadMSFragger.isSelected();
  }

  public boolean downloadIonQuant() {
    return uiCheckDownloadIonQuant.isSelected();
  }

  public boolean downloadDiaTracer() {
    return uiCheckDownloadDiaTracer.isSelected();
  }
}
