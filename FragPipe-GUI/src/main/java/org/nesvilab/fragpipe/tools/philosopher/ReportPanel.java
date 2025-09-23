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

package org.nesvilab.fragpipe.tools.philosopher;

import org.nesvilab.fragpipe.messages.MessageSearchType;
import org.nesvilab.fragpipe.params.ThisAppProps;
import org.nesvilab.utils.SwingUtils;
import org.nesvilab.utils.swing.FormEntry;
import org.nesvilab.utils.swing.JPanelBase;
import org.nesvilab.utils.swing.MigUtils;
import org.nesvilab.utils.swing.UiCheck;
import org.nesvilab.utils.swing.UiText;
import org.nesvilab.utils.swing.UiUtils.UiTextBuilder;
import java.awt.Component;
import java.awt.ItemSelectable;
import java.util.ArrayList;
import java.util.List;
import javax.swing.JCheckBox;
import javax.swing.JPanel;
import javax.swing.JSeparator;
import javax.swing.SwingConstants;
import javax.swing.border.EmptyBorder;
import net.java.balloontip.BalloonTip;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.ThreadMode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ReportPanel extends JPanelBase {
  private static final Logger log = LoggerFactory.getLogger(ReportPanel.class);
  private static final MigUtils mu = MigUtils.get();

  public static final String PREFIX = "phi-report.";

  private final List<BalloonTip> balloonTips = new ArrayList<>();
  private JCheckBox checkRun;
  private JPanel pTop;
  private JPanel pOptions;
  private UiText uiTextFilter;
  public UiCheck uiCheckPepSummary;
  public UiCheck uiCheckProtSummary;
  private UiCheck uiCheckPrintDecoys;
  private UiCheck uiCheckDontUseProtProphFile;
  private UiCheck uiCheckRemoveContaminants;

  @Override
  protected ItemSelectable getRunCheckbox() {
    return checkRun;
  }

  @Override
  protected Component getEnablementToggleComponent() {
    return pOptions;
  }

  @Override
  protected String getComponentNamePrefix() {
    return PREFIX;
  }

  @Override
  protected void init() {
    mu.layout(this, mu.lcFillXNoInsetsTopBottom());
    mu.border(this, "FDR filter and report");

    pTop = createPanelTop();
    pOptions = createPanelOptions();

    mu.add(this, pTop).wrap();
    mu.add(this, pOptions).wrap();
  }

  @Override
  protected void initMore() {
    updateEnabledStatus(this, true);
    super.initMore();
  }

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void on(MessageSearchType m) {
    String key = PREFIX + "filter." + m.type.name();
    String val = ThisAppProps.getLocalProperties().getProperty(key);
    if (val == null) {
      throw new IllegalStateException("No Report Filter defaults found in bundle for key: " + key);
    }
    uiTextFilter.setText(val);
    uiCheckPepSummary.setSelected(false);
    uiCheckProtSummary.setSelected(true);
    checkRun.setSelected(true);
  }

  private JPanel createPanelOptions() {
    JPanel p = new JPanel(new MigLayout(new LC()));//.debug()));
    p.setBorder(new EmptyBorder(0, 0, 0, 0));

    uiTextFilter = new UiTextBuilder().cols(5).text("--sequential --prot 0.01").create();
    FormEntry feFilter = new FormEntry("filter", "Filter", uiTextFilter,
        "<html>A custom algorithm for MS/MS data filtering and multi-level false discovery rate estimation.<br/>\n"
            + "See: https://github.com/Nesvilab/philosopher/wiki/Filter<br/><br/>\n" +
            "philosopher filter [flags]<br>\n" +
            "Flags:<br/>\n" +
            "<ul>\n" +
            "<li>--ion float        peptide ion FDR level (default 0.01)</li>\n" +
            "<li>--mapmods          map modifications aquired by an open search</li>\n" +
            "<li>--models           print model distribution</li>\n" +
            "<li>--pep float        peptide FDR level (default 0.01)</li>\n" +
            "<li>--pepProb float    top peptide probability treshold for the FDR filtering (default 0.7)</li>\n"
            +
            "<li>--pepxml string    pepXML file or directory containing a set of pepXML files</li>\n"
            +
            "<li>--picked           apply the picked FDR algorithm before the protein scoring</li>\n"
            +
            "<li>--prot float       protein FDR level (default 0.01)</li>\n" +
            "<li>--protProb float   protein probability treshold for the FDR filtering (not used with the razor algorithm) (default 0.5)</li>\n"
            +
            "<li>--protxml string   protXML file path</li>\n" +
            "<li>--psm float        psm FDR level (default 0.01)</li>\n" +
            "<li>--sequential       alternative algorithm that estimates FDR using both filtered PSM and Protein lists</li>\n"
            +
            "<li>--tag string       decoy tag (default \"rev_\")</li>\n" +
            "<li>--weight float     threshold for defining peptide uniqueness (default 1)</li>\n"
            +
            "</ul>");


    uiCheckPepSummary = new UiCheck("Generate peptide-level summary", null, false);
    FormEntry feCheckPepSummary = new FormEntry("pep-level-summary", "not-shown", uiCheckPepSummary, "<html>Let Philosopher generate combined_peptide.tsv file.<br><b>Uncheck</b> it if analyzing large dataset because Philosopher needs lots of memory to run.<br>It is <b>disabled</b> if IonQuant is enabled because IonQuant will generate the same file.");

    uiCheckProtSummary = new UiCheck("Generate protein-level summary", null, true);
    FormEntry feCheckProtSummary = new FormEntry("prot-level-summary", "not-shown", uiCheckProtSummary, "<html>Let Philosopher generate combined_protein.tsv file.<br><b>Uncheck</b> it if analyzing large dataset because Philosopher needs lots of memory to run.<br>It is <b>disabled</b> if IonQuant is enabled because IonQuant will generate the same file.");

    uiCheckRemoveContaminants = new UiCheck("Remove contaminants", null, false);
    FormEntry feCheckRemoveContaminants = new FormEntry("remove-contaminants", "not-shown", uiCheckRemoveContaminants, "<html>Remove contaminant proteins from the tsv files.");

    uiCheckPrintDecoys = new UiCheck("Print decoys", null, false);
    FormEntry feCheckPrintDecoys = new FormEntry("print-decoys", "not-shown",
        uiCheckPrintDecoys,
        "<html>Option to show decoy entries in report tables.");

    uiCheckDontUseProtProphFile = new UiCheck("Do not use ProteinProphet file", null, false);
    FormEntry feCheckDontUseProtProphFile = new FormEntry(
        "dont-use-prot-proph-file", "not-shown", uiCheckDontUseProtProphFile,
        "<html>Only to be used in rare cases.<br/>\n" +
            "Consider unchecking 'Run ProteinProphet' above instead of using this checkbox.");

    mu.add(p, feFilter.label()).spanX().split();
    mu.add(p, feFilter.comp).growX().pushX().wrap();
    mu.add(p, feCheckDontUseProtProphFile.comp).wrap();
    mu.add(p, new JSeparator(SwingConstants.HORIZONTAL)).growX().spanX().wrap();
    mu.add(p, feCheckRemoveContaminants.comp);
    mu.add(p, feCheckPrintDecoys.comp);
    mu.add(p, feCheckPepSummary.comp);
    mu.add(p, feCheckProtSummary.comp).wrap();

    return p;
  }

  private JPanel createPanelTop() {
    // setting the insets allows the top panel to be shifted left of the options panel
    JPanel p = new JPanel(new MigLayout(new LC().insetsAll("0px")));

    checkRun = new UiCheck("Generate reports", null, true);
    checkRun.setName("run-report");
    checkRun.addActionListener(e -> {
      final boolean isSelected = checkRun.isSelected();
      enablementMapping.put(pOptions, isSelected);
      updateEnabledStatus(pOptions, isSelected);
    });
    p.add(checkRun, new CC().alignX("left"));

    p.setBorder(new EmptyBorder(0, 0, 0, 0));
    return p;
  }

  @Override
  public boolean isRun() {
    return SwingUtils.isEnabledAndChecked(checkRun);
  }

  public boolean isPepSummary() {
    return uiCheckPepSummary.isSelected();
  }

  public boolean isProtSummary() {
    return uiCheckProtSummary.isSelected();
  }

  public boolean isNoProtXml() {
    return uiCheckDontUseProtProphFile.isSelected();
  }

  public String getFilterCmdText() {
    return uiTextFilter.getNonGhostText();
  }

  public void setPrintDecoys(boolean printDecoys) {
    uiCheckPrintDecoys.setSelected(printDecoys);
  }

  public boolean isPrintDecoys() {
    return uiCheckPrintDecoys.isSelected();
  }

  public boolean isRemoveContaminants() {
    return uiCheckRemoveContaminants.isSelected();
  }

  private void clearBalloonTips() {
    for (BalloonTip balloonTip : balloonTips) {
      if (balloonTip != null) {
        try {
          balloonTip.closeBalloon();
        } catch (Exception ignore) {
        }
      }
    }
    balloonTips.clear();
  }

}
