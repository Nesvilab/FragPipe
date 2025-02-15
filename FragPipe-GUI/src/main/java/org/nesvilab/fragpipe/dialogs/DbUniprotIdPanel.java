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

import org.nesvilab.fragpipe.Fragpipe;
import org.nesvilab.fragpipe.tabs.TabDatabase;
import org.nesvilab.utils.StringUtils;
import java.awt.BorderLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.swing.*;
import javax.swing.border.TitledBorder;
import org.nesvilab.fragpipe.params.ThisAppProps;
import org.nesvilab.utils.swing.FormEntry;
import org.nesvilab.utils.swing.MigUtils;
import org.nesvilab.utils.swing.UiText;

public class DbUniprotIdPanel extends JPanel {

  private ButtonGroup btnGroupUniprotIds;
  private List<JRadioButton> radios;
  private JRadioButton radioOther;
  private UiText textSpikeInFASTA;
  private JTextField textOther;
  private JCheckBox checkIsReviewed;
  private JCheckBox checkAddContaminants;
  private JCheckBox checkAddIsoforms;
  private JCheckBox checkAddDecoys;
  private JCheckBox checkAddIrt;

  private static final String PROP_UNIPROT_IDS = "database.uniprot.ids";
  public static final Pattern RE_UNIPROT_ID = Pattern.compile("Uniprot ID:.*?\\b(.+?)\\b", Pattern.CASE_INSENSITIVE);

  public DbUniprotIdPanel() {
    initMore();
  }

  private void initMore() {
    JPanel container = new JPanel();
//    container.setLayout(new BoxLayout(container, BoxLayout.Y_AXIS));
    container.setLayout(new GridBagLayout());

    // add external spike-in sequences from an existing FASTA file
    textSpikeInFASTA = new UiText();

    FormEntry feDbPath = Fragpipe.fe(textSpikeInFASTA, "spike-in-db-path").label("FASTA file path").create();
    JButton btnBrowse = feDbPath.browseButton("Browse", "optional",
            () -> TabDatabase.createFilechooserFasta(textSpikeInFASTA),
            paths -> textSpikeInFASTA.setText(paths.get(0).toString()));

    final MigUtils mu = MigUtils.get();
    JPanel panelAddSpikeIn = mu.newPanel("Spike-in sequences (do not include decoys)", true);
    mu.add(panelAddSpikeIn, feDbPath.label()).split();
    mu.add(panelAddSpikeIn, feDbPath.comp).growX();
    mu.add(panelAddSpikeIn, btnBrowse);

    // option checkboxes
    JPanel panelCheckboxes = new JPanel();
    panelCheckboxes.setLayout(new BoxLayout(panelCheckboxes, BoxLayout.Y_AXIS));
    panelCheckboxes.setBorder(new TitledBorder("Options"));
    checkIsReviewed = new JCheckBox("Reviewed sequences only", true);
    checkAddDecoys = new JCheckBox("Add decoys", true);
    checkAddContaminants = new JCheckBox("Add common contaminants", true);
    checkAddIsoforms = new JCheckBox("Add isoforms", false);
    checkAddIrt = new JCheckBox("Add iRT sequences", false);
    checkAddDecoys.setToolTipText("<html>Decoy sequences will be generated based on the downloaded database.<br/>\n" +
            "Decoys are required for FDR estimation.");
    panelCheckboxes.add(checkIsReviewed);
    panelCheckboxes.add(checkAddDecoys);
    panelCheckboxes.add(checkAddContaminants);
    panelCheckboxes.add(checkAddIsoforms);
    panelCheckboxes.add(checkAddIrt);


    // organism selection radio buttons
    JPanel panelRadios = new JPanel();
    panelRadios.setLayout(new BoxLayout(panelRadios, BoxLayout.Y_AXIS));
    panelRadios.setBorder(new TitledBorder("Select organism / Input proteome ID"));

    radios = new ArrayList<>();
    btnGroupUniprotIds = new ButtonGroup();
    String ids = null;
    try {
      ids = ThisAppProps.getLocalProperties().getProperty(PROP_UNIPROT_IDS);
    } catch (IllegalStateException ignored) {}
    if (!StringUtils.isNullOrWhitespace(ids)) {
      Arrays.stream(ids.split(";;")).map(String::trim).forEach(id -> radios.add(new JRadioButton(id)));
    } else {
      radios.add(new JRadioButton("Human - Uniprot ID: UP000005640"));
      radios.add(new JRadioButton("Mouse - Uniprot ID: UP000000589"));
    }
    radioOther = new JRadioButton("Other:");
    radios.add(radioOther);
    if (radios.isEmpty())
      throw new IllegalStateException("no radio buttons");
    radios.get(0).setSelected(true);
    radios.forEach(r -> btnGroupUniprotIds.add(r));

    for (JRadioButton radio : radios) {
      panelRadios.add(radio);
    }
    textOther = new JTextField();
    textOther.setToolTipText("<html>Uniprot ID, similar to: UP000005640<br/>See http://www.uniprot.org/proteomes");
    panelRadios.add(textOther);

    // pack the main panel
    GridBagConstraints cc = new GridBagConstraints();
    cc.gridx = 0;
    cc.gridy = 0;
    cc.fill = GridBagConstraints.HORIZONTAL;
    container.add(panelRadios, cc);
    cc.gridx = 0;
    cc.gridy = 1;
    cc.fill = GridBagConstraints.HORIZONTAL;
    container.add(panelCheckboxes, cc);
    GridBagConstraints cr = new GridBagConstraints();
    cr.gridx = 0;
    cr.gridy = 2;
    cr.fill = GridBagConstraints.BOTH;
    container.add(panelAddSpikeIn, cr);

    JScrollPane scroll = new JScrollPane(container);
    this.setLayout(new BorderLayout());
    this.add(scroll, BorderLayout.CENTER);
  }

  /**
   * Only to be called if user clicked OK on dialog.
   */
  public String getSelectedUniprotId() {
    for (JRadioButton radio : radios) {
      if (!radio.isSelected())
        continue;
      if (radioOther == radio) {
        // selected manual selection
        final String text = textOther.getText().trim();
        return StringUtils.isNullOrWhitespace(text) ? null : text;
      } else {
        final String text = radio.getText();
        Matcher m = RE_UNIPROT_ID.matcher(text);
        if (!m.find())
          throw new IllegalStateException("Could not match radio button text to regular expression.");
        return m.group(1);
      }
    }
    return null;
  }

  public boolean isReviewed() {
    return checkIsReviewed.isSelected();
  }

  public boolean isAddContaminants() {
    return checkAddContaminants.isSelected();
  }

  public boolean isAddIsoforms() {
    return checkAddIsoforms.isSelected();
  }

  public boolean isAddDecoys() {
    return checkAddDecoys.isSelected();
  }

  public boolean isAddIrt() {
    return checkAddIrt.isSelected();
  }

  public Path addSpikeIn() {
    final String text = textSpikeInFASTA.getNonGhostText().trim();
    if (StringUtils.isNullOrWhitespace(text))
      return null;
    return Paths.get(text);
  }
}
