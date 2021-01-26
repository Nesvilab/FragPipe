package com.dmtavt.fragpipe.dialogs;

import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.swing.FormEntry;
import com.github.chhh.utils.swing.MigUtils;
import com.github.chhh.utils.swing.UiText;
import com.github.chhh.utils.swing.UiUtils;
import java.awt.Dimension;
import javax.swing.JCheckBox;
import javax.swing.JEditorPane;
import javax.swing.JPanel;
import net.miginfocom.swing.MigLayout;

public class DownloadMSFraggerPanel extends JPanel {

  private final MigUtils mu = MigUtils.get();

  private FormEntry feName;
  private FormEntry feEmail;
  private FormEntry feInstitution;
  private JCheckBox license2;
  private JCheckBox license3;
  private JCheckBox license1;

  public DownloadMSFraggerPanel() {
    initMore();
  }

  private void initMore() {
    JPanel panelTextboxes = new JPanel() {
      @Override
      public Dimension getPreferredSize() {
        return new Dimension(400, 300);
      }
    };

    panelTextboxes.setLayout(new MigLayout(mu.lcFillXNoInsetsTopBottom()));

    JEditorPane t0 = SwingUtils.createClickableHtml("I understand that MSFragger is available freely for academic research and educational purposes only. For any uses other than academic research, non-commercial or educational purposes, please obtain a commercial license from the <a href=\"https://umich.flintbox.com/?embed=true#technologies/a8dd00a2-c057-4125-823b-adffac748490\" target=\"blank_\">University of Michigan Office of Tech Transfer</a>. For questions, and to obtain the latest version of MSFragger for a 60-day evaluation period, please contact Alexey Nesvizhskii (nesvi at med.umich.edu).<br><br>");

    feName = mu.feb(UiUtils.uiTextBuilder().cols(40).create()).label("Name:").create();
    feEmail = mu.feb(UiUtils.uiTextBuilder().cols(40).create()).label("Email:").create();
    feInstitution = mu.feb(UiUtils.uiTextBuilder().cols(40).create()).label("Institution").create();

    JEditorPane t1 = SwingUtils.createClickableHtml("I agree to the terms of <b>ACADEMIC</b> <a href=\"https://msfragger.arsci.com/upgrader/MSFragger-LICENSE.pdf\" target=\"blank_\">license</a> for MSFragger software.");
    license1 = new JCheckBox();

    JEditorPane t2 = SwingUtils.createClickableHtml("I agree to the terms of <a href=\"https://msfragger.arsci.com/upgrader/RawFileRdr_License_Agreement_RevA.pdf\" target=\"blank_\">Thermo (c) Raw File Reader License Agreement</a>.");
    license2 = new JCheckBox();

    JEditorPane t3 = SwingUtils.createClickableHtml("I agree to the terms of <a href=\"https://msfragger.arsci.com/upgrader/redist.txt\" target=\"blank_\">Bruker SDK library distribution conditions</a>.");
    license3 = new JCheckBox();

    JPanel licensePanel1 = new JPanel();
    licensePanel1.add(license1);
    licensePanel1.add(t1);

    JPanel licensePanel2 = new JPanel();
    licensePanel2.add(license2);
    licensePanel2.add(t2);

    JPanel licensePanel3 = new JPanel();
    licensePanel3.add(license3);
    licensePanel3.add(t3);

    mu.add(panelTextboxes, t0).spanX().wrap();

    mu.add(panelTextboxes, feName.label(), mu.ccR());
    mu.add(panelTextboxes, feName.comp).spanX().wrap();

    mu.add(panelTextboxes, feEmail.label(), mu.ccR());
    mu.add(panelTextboxes, feEmail.comp).spanX().wrap();

    mu.add(panelTextboxes, feInstitution.label(), mu.ccR());
    mu.add(panelTextboxes, feInstitution.comp).spanX().wrap();

    mu.add(panelTextboxes, licensePanel1).spanX().wrap();
    mu.add(panelTextboxes, licensePanel2).spanX().wrap();
    mu.add(panelTextboxes, licensePanel3).spanX().wrap();

    this.add(panelTextboxes);
  }

  public String getName() {
    return ((UiText) feName.comp).getText().trim();
  }

  public String getEmail() {
    return ((UiText) feEmail.comp).getText().trim();
  }

  public String getInstitution() {
    return ((UiText) feInstitution.comp).getText().trim();
  }

  public boolean licensesChecked() {
    return license1.isSelected() && license2.isSelected() && license3.isSelected();
  }
}
