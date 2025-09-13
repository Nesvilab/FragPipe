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

package org.nesvilab.fragpipe.dialogs;

import static org.nesvilab.fragpipe.Fragpipe.WEB_DOMAIN;
import static org.nesvilab.fragpipe.tabs.TabConfig.userEmail;
import static org.nesvilab.fragpipe.tabs.TabConfig.userFirstName;
import static org.nesvilab.fragpipe.tabs.TabConfig.userInstitution;
import static org.nesvilab.fragpipe.tabs.TabConfig.userLastName;

import java.awt.BorderLayout;
import java.awt.Dimension;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JEditorPane;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import net.miginfocom.swing.MigLayout;
import org.nesvilab.fragpipe.tools.fragger.MsfraggerVersionFetcherServer;
import org.nesvilab.utils.SwingUtils;
import org.nesvilab.utils.swing.FormEntry;
import org.nesvilab.utils.swing.MigUtils;
import org.nesvilab.utils.swing.UiText;
import org.nesvilab.utils.swing.UiUtils;

public class DownloadToolsPanel extends JPanel {

  private final MigUtils mu = MigUtils.get();

  private FormEntry feFirstName;
  private FormEntry feLastName;
  private FormEntry feEmail;
  private FormEntry feInstitution;
  private JCheckBox license2;
  private JCheckBox license3;
  private JCheckBox license1;
  private UiText uiTextVerificationCode;
  private FormEntry feVerificationCode;
  private JButton btnSendRequest;

  public DownloadToolsPanel() {
    initMore();
  }

  @Override
  public Dimension getPreferredSize() {
    return new Dimension(500, 600);
  }

  private void initMore() {
    JPanel panelTextboxes = new JPanel();

    panelTextboxes.setLayout(new MigLayout(mu.lcFillXNoInsetsTopBottom()));

    JEditorPane t0 = SwingUtils.createClickableHtml(
        "<br>MSFragger, IonQuant, and diaTracer software are available freely for academic research, non-commercial<br>"
            + "or educational purposes under academic license.<br><br>"
            + "Read the <a href=\"" + WEB_DOMAIN + "upgrader/LICENSE-ACADEMIC.pdf\" target=\"blank_\">academic license</a> for MSFragger, IonQuant, and diaTracer.<br>");

    feFirstName = mu.feb(UiUtils.uiTextBuilder().cols(40).create()).label("First Name:").create();
    feLastName = mu.feb(UiUtils.uiTextBuilder().cols(40).create()).label("Last Name:").create();
    feEmail = mu.feb(UiUtils.uiTextBuilder().cols(40).create()).label("<html><div align='right'>Email:<br>(academic email)</div></html>").create();
    feInstitution = mu.feb(UiUtils.uiTextBuilder().cols(40).create()).label("<html><div align='right'>Institution:<br>(academic only)</div></html>").create();

    if (userFirstName != null) {
      ((UiText) feFirstName.comp).setText(userFirstName);
    }
    if (userLastName != null) {
      ((UiText) feLastName.comp).setText(userLastName);
    }
    if (userEmail != null) {
      ((UiText) feEmail.comp).setText(userEmail);
    }
    if (userInstitution != null) {
      ((UiText) feInstitution.comp).setText(userInstitution);
    }

    JEditorPane t1 = SwingUtils.createClickableHtml(
        "I have read the <a href=\"" + WEB_DOMAIN + "upgrader/LICENSE-ACADEMIC.pdf\" target=\"blank_\">academic license</a> and fully agree to abide by its terms.<br>"
            + "I acknowledge that it grants a non-transferable right to use MSFragger,<br>"
            + "IonQuant, and diaTracer exclusively for academic research,<br>"
            + "non-commercial, or educational purposes.<br>");
    license1 = new JCheckBox();

    JEditorPane t2 = SwingUtils.createClickableHtml("I agree to the terms of <a href=\"" + WEB_DOMAIN + "upgrader/RawFileRdr_License_Agreement_RevA.pdf\" target=\"blank_\">Thermo (c) Raw File Reader License Agreement</a>.");
    license2 = new JCheckBox();

    JEditorPane t3 = SwingUtils.createClickableHtml("I agree to the terms of <a href=\"" + WEB_DOMAIN + "upgrader/EULA%20TDF-SDK.pdf\" target=\"blank_\">Bruker SDK library distribution conditions</a>.");
    license3 = new JCheckBox();

    uiTextVerificationCode = UiUtils.uiTextBuilder().cols(40).create();
    feVerificationCode = mu.feb(uiTextVerificationCode).label("Verification Code:").create();
    feVerificationCode.comp.setVisible(true);
    feVerificationCode.label().setVisible(true);
    feVerificationCode.comp.setEnabled(false);
    feVerificationCode.label().setEnabled(false);

    btnSendRequest = new JButton("<html><b>Send Verification Email</b></html>");
    btnSendRequest.addActionListener(e -> {
      try {
        MsfraggerVersionFetcherServer fetcher = new MsfraggerVersionFetcherServer(getFirstName(), getLastName(), getEmail(), getInstitution());
        if (fetcher.sendRequest()) {
          feVerificationCode.comp.setEnabled(true);
          feVerificationCode.label().setEnabled(true);
        }
      } catch (Exception ex) {
        JOptionPane.showMessageDialog(this, "Failed to send request: " + ex.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
      }
    });

    JEditorPane lblAcademic = SwingUtils.createClickableHtml(
        "<html>Download available for academic users only. Non-academic users (including those with an existing<br>" +
        "commercial license from the University of Michigan) must visit Fragmatics at <a href=\"https://www.fragmatics.com\" target=\"blank_\">www.fragmatics.com</a> or<br>" + 
        "email <a href=\"mailto:info@fragmatics.com\">info@fragmatics.com</a> to purchase or confirm their license and access the tools.<br><br></html>");

    JEditorPane lblEmailNote = SwingUtils.createClickableHtml(
        "<html><b>NOTE:</b> If you don’t receive the verification email, check your spam folder. If it's not there<br>" + 
        "download <a href=\"" + WEB_DOMAIN + "upgrader\">MSFragger</a>, <a href=\"" + WEB_DOMAIN + "upgrader\">IonQuant</a>, and <a href=\"" + WEB_DOMAIN + "upgrader\">diaTracer</a> through your browser. Unzip them, put them<br>" + 
        "in the same folder, and enter the folder path in the 'Config' tab.<br>" +
        "</html>");

    mu.add(panelTextboxes, t0).split().spanX().wrap();

    mu.add(panelTextboxes, lblAcademic).split().spanX().wrap();

    mu.add(panelTextboxes, feFirstName.label(), mu.ccR());
    mu.add(panelTextboxes, feFirstName.comp).wrap();
    mu.add(panelTextboxes, feLastName.label(), mu.ccR());
    mu.add(panelTextboxes, feLastName.comp).wrap();

    mu.add(panelTextboxes, feEmail.label(), mu.ccR());
    mu.add(panelTextboxes, feEmail.comp).spanX().wrap();

    mu.add(panelTextboxes, feInstitution.label(), mu.ccR());
    mu.add(panelTextboxes, feInstitution.comp).spanX().wrap();

    mu.add(panelTextboxes, license1, mu.ccR());
    mu.add(panelTextboxes, t1).spanX().wrap();

    mu.add(panelTextboxes, license2, mu.ccR());
    mu.add(panelTextboxes, t2).spanX().wrap();

    mu.add(panelTextboxes, license3, mu.ccR());
    mu.add(panelTextboxes, t3).spanX().wrap();

    mu.add(panelTextboxes, btnSendRequest).spanX().alignX("center").wrap();

    mu.add(panelTextboxes, lblEmailNote).spanX().alignX("center").wrap();

    mu.add(panelTextboxes, feVerificationCode.label(), mu.ccR());
    mu.add(panelTextboxes, feVerificationCode.comp).spanX().wrap();

    JScrollPane scroll = new JScrollPane(panelTextboxes, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

    this.setLayout(new BorderLayout());
    this.add(scroll, BorderLayout.CENTER);
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

  public String getVerificationCode() {
    if (uiTextVerificationCode == null || uiTextVerificationCode.getText() == null) {
      return null;
    } else {
      return uiTextVerificationCode.getText().trim();
    }
  }

  public String getFirstName() {
    if (feFirstName == null || feFirstName.comp == null || ((UiText) feFirstName.comp).getText() == null) {
      return null;
    } else {
      return ((UiText) feFirstName.comp).getText().trim();
    }
  }

  public String getLastName() {
    if (feLastName == null || feLastName.comp == null || ((UiText) feLastName.comp).getText() == null) {
      return null;
    } else {
      return ((UiText) feLastName.comp).getText().trim();
    }
  }
}
