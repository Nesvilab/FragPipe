package com.dmtavt.fragpipe.dialogs;

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

public class DownloadMSFraggerPanel extends JPanel {

  private final MigUtils mu = MigUtils.get();

  private FormEntry feName;
  private FormEntry feEmail;
  private FormEntry feInstitution;
  private JCheckBox license2;
  private JCheckBox license3;
  private JCheckBox license1;
  private JCheckBox receiveEmail;

  public DownloadMSFraggerPanel() {
    initMore();
  }

  @Override
  public Dimension getPreferredSize() {
    return new Dimension(400, 400);
  }

  private void initMore() {
    JPanel panelTextboxes = new JPanel();

    panelTextboxes.setLayout(new MigLayout(mu.lcFillXNoInsetsTopBottom()));

    JEditorPane t0 = SwingUtils.createClickableHtml(
        "<br>MSFragger is available freely for academic research, non-commercial or <br>educational purposes under academic license. Other uses require a commercial <br> license after the initial 60-day evaluation period that can be obtained by <br>contacting Drew Bennett (andbenne@umich.edu) at the University of Michigan<br> Office of Tech Transfer. For questions, please contact <br>Prof. Alexey Nesvizhskii (nesvi@med.umich.edu).<br><br>");

    feName = mu.feb(UiUtils.uiTextBuilder().cols(30).create()).label("Name:").create();
    feEmail = mu.feb(UiUtils.uiTextBuilder().cols(30).create()).label("Email:").create();
    feInstitution = mu.feb(UiUtils.uiTextBuilder().cols(30).create()).label("Institution").create();

    JEditorPane t1 = SwingUtils.createClickableHtml("I have read the <b>academic</b> <a href=\"https://msfragger.arsci.com/upgrader/MSFragger-LICENSE.pdf\" target=\"blank_\">license</a>. I understand that this <br>license provides with a non-exclusive, non-transferable right <br>to use MSFragger solely for academic research, non-commercial <br>or educational purposes within the licensee’s department.");
    license1 = new JCheckBox();

    JEditorPane t2 = SwingUtils.createClickableHtml("I agree to the terms of <a href=\"https://msfragger.arsci.com/upgrader/RawFileRdr_License_Agreement_RevA.pdf\" target=\"blank_\">Thermo (c) Raw File Reader License <br>Agreement</a>.");
    license2 = new JCheckBox();

    JEditorPane t3 = SwingUtils.createClickableHtml("I agree to the terms of <a href=\"https://msfragger.arsci.com/upgrader/redist.txt\" target=\"blank_\">Bruker SDK library distribution <br>conditions</a>.");
    license3 = new JCheckBox();

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
}
