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

package com.dmtavt.fragpipe.dialogs;

import static com.dmtavt.fragpipe.tabs.TabConfig.userEmail;
import static com.dmtavt.fragpipe.tabs.TabConfig.userInstitution;
import static com.dmtavt.fragpipe.tabs.TabConfig.userName;

import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.swing.FormEntry;
import com.github.chhh.utils.swing.MigUtils;
import com.github.chhh.utils.swing.UiText;
import com.github.chhh.utils.swing.UiUtils;
import java.awt.BorderLayout;
import java.awt.Dimension;
import javax.swing.JCheckBox;
import javax.swing.JEditorPane;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import net.miginfocom.swing.MigLayout;

public class DownloadIonQuantPanel extends JPanel {

  private final MigUtils mu = MigUtils.get();

  private FormEntry feName;
  private FormEntry feEmail;
  private FormEntry feInstitution;
  private JCheckBox license1;
  private JCheckBox receiveEmail;

  public DownloadIonQuantPanel() {
    initMore();
  }

  @Override
  public Dimension getPreferredSize() {
    return new Dimension(500, 500);
  }

  private void initMore() {
    JPanel panelTextboxes = new JPanel();

    panelTextboxes.setLayout(new MigLayout(mu.lcFillXNoInsetsTopBottom()));

    JEditorPane t0 = SwingUtils.createClickableHtml(
        "<br>IonQuant is available freely for academic research, non-commercial or educational purposes under <br> academic license. Other uses require a commercial license after the initial 60-day evaluation <br> period that can be obtained by contacting Drew Bennett (andbenne@umich.edu) at the <br> University of Michigan Office of Tech Transfer. For questions, please contact Prof. Alexey <br> Nesvizhskii (nesvi@med.umich.edu).<br><br>");

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

    JEditorPane t1 = SwingUtils.createClickableHtml("I have read the <b>academic</b> <a href=\"https://msfragger.arsci.com/upgrader/MSFragger-LICENSE.pdf\" target=\"blank_\">license</a>. I understand that this license provides <br> with a non-exclusive, non-transferable right to use IonQuant solely for academic <br> research, non-commercial or educational purposes within the licensee’s department.");
    license1 = new JCheckBox();

    JEditorPane t4 = SwingUtils.createClickableHtml("I would like to receive emails with updates in the future.");
    receiveEmail = new JCheckBox();

    mu.add(panelTextboxes, t0).spanX().wrap();

    mu.add(panelTextboxes, feName.label(), mu.ccR());
    mu.add(panelTextboxes, feName.comp).spanX().wrap();

    mu.add(panelTextboxes, feEmail.label(), mu.ccR());
    mu.add(panelTextboxes, feEmail.comp).spanX().wrap();

    mu.add(panelTextboxes, feInstitution.label(), mu.ccR());
    mu.add(panelTextboxes, feInstitution.comp).spanX().wrap();

    mu.add(panelTextboxes, new JLabel()).wrap();

    mu.add(panelTextboxes, license1, mu.ccR());
    mu.add(panelTextboxes, t1).spanX().wrap();

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
    if (license1 == null) {
      return false;
    } else {
      return license1.isSelected();
    }
  }

  public boolean wantReceiveEmail() {
    if (receiveEmail == null) {
      return false;
    } else {
      return receiveEmail.isSelected();
    }
  }
}
