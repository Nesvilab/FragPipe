/*
 * Copyright 2018 Dmitry Avtonomov.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package umich.msfragger.params.fragger;

import com.github.chhh.utils.swing.DocumentFilters;
import com.github.chhh.utils.swing.UiCheck;
import com.github.chhh.utils.swing.UiCombo;
import com.github.chhh.utils.swing.UiSpinnerDouble;
import com.github.chhh.utils.swing.UiSpinnerInt;
import com.github.chhh.utils.swing.UiText;
import com.github.chhh.utils.swing.UiUtils;
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.text.DecimalFormat;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import javax.swing.DefaultComboBoxModel;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFormattedTextField;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSpinner;
import javax.swing.JSpinner.DefaultEditor;
import javax.swing.border.EmptyBorder;
import javax.swing.border.TitledBorder;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.greenrobot.eventbus.EventBus;
import umich.msfragger.gui.api.SearchTypeProp;
import umich.msfragger.messages.MessageSearchType;
import umich.msfragger.params.enums.CleavageType;
import umich.msfragger.params.enums.MassTolUnits;
import umich.msfragger.util.swing.FormEntry;

/**
 *
 * @author Dmitry Avtonomov
 */
public class FraggerMigPanel extends JPanel {

  private ImageIcon icon;
  private JCheckBox checkRun;
  private JScrollPane scroll;

  private static final String PROP_adjust_precurosr_mass = "misc.adjust-precursor-mass";
  private static final String PROP_slice_db = "misc.slice-db";
  private static final String PROP_ram = "misc.ram";
  private static final String PROP_digest_mass_lo = "misc.digest-mass-lo";
  private static final String PROP_digest_mass_hi = "misc.digest-mass-hi";
  private static String[] PROPS_MISC = {PROP_adjust_precurosr_mass, PROP_slice_db, PROP_ram};

  public FraggerMigPanel() {
    initMore();
  }

  private void initMore() {
    icon = new ImageIcon(
        getClass().getResource("/umich/msfragger/gui/icons/bolt-16.png"));

    this.setLayout(new BorderLayout());

    // Top panel with checkbox, buttons and RAM+Threads spinners
    {
      JPanel pTop = new JPanel(new MigLayout(new LC()));
      checkRun = new JCheckBox("Run MSFragger");
      JButton closed = new JButton("Closed Search");
      closed.addActionListener(e -> {
        EventBus.getDefault().post(new MessageSearchType(SearchTypeProp.closed));
      });
      JButton open = new JButton("Open Search");
      open.addActionListener(e -> {
        EventBus.getDefault().post(new MessageSearchType(SearchTypeProp.open));
      });
      JButton nonspecific = new JButton("Non-specific Search");
      open.addActionListener(e -> {
        EventBus.getDefault().post(new MessageSearchType(SearchTypeProp.nonspecific));
      });

      pTop.add(checkRun);
      pTop.add(new JLabel("Load defaults:"), new CC().gapLeft("15px"));
      pTop.add(closed, new CC().gapLeft("1px"));
      pTop.add(open, new CC().gapLeft("1px"));
      pTop.add(nonspecific, new CC().gapLeft("1px").wrap());

      JButton save = new JButton("Save Options");
      JButton load = new JButton("Load Options");
      UiSpinnerInt spinnerRam = new UiSpinnerInt(0, 0, 1024, 1);
      UiSpinnerInt spinnerThreads = new UiSpinnerInt(0, 0, 128, 1);
      FormEntry feRam = new FormEntry(PROP_ram, "RAM", spinnerRam);
      FormEntry feThreads = new FormEntry(MsfraggerParams.PROP_num_threads, "Threads", spinnerThreads);
      pTop.add(save, new CC().split(6).spanX());
      pTop.add(load, new CC());
      pTop.add(feRam.label(), new CC());
      pTop.add(feRam.comp, new CC());
      pTop.add(feThreads.label(), new CC());
      pTop.add(feThreads.comp, new CC());



      this.add(pTop, BorderLayout.NORTH);
    }

    JPanel pContent = new JPanel();
    pContent.setLayout(new MigLayout(new LC().fillX()));
    scroll = new JScrollPane(pContent);
    scroll.setBorder(new EmptyBorder(0,0,0,0));
    scroll.getVerticalScrollBar().setUnitIncrement(16);

    // Panel with all the basic options
    {
      JPanel pBase = new JPanel(new MigLayout(new LC().fillX()));
      pBase.setBorder(new TitledBorder("Common Options"));

      JPanel pPeakMatch = new JPanel(new MigLayout(new LC()));
      pPeakMatch.setBorder(new TitledBorder("Peak Matching"));

      // precursor mass tolerance
      UiCombo comboPrecTolUnits = new UiCombo();
      comboPrecTolUnits.setModel(new DefaultComboBoxModel<>(
          Arrays.stream(MassTolUnits.values()).map(MassTolUnits::name).toArray(String[]::new)));
      FormEntry fePrecTolUnits = new FormEntry("precursor_mass_units", "Precursor mass tolerance",
          comboPrecTolUnits);
      UiSpinnerDouble uiSpinnerPrecTolLo = new UiSpinnerDouble(-10, -10000, 10000, 1,
          new DecimalFormat("0.#"));
      FormEntry feSpinnerPrecTolLo = new FormEntry(MsfraggerParams.PROP_precursor_mass_lower,
          "not-shown", uiSpinnerPrecTolLo);
      UiSpinnerDouble uiSpinnerPrecTolHi = new UiSpinnerDouble(+10, -10000, 10000, 1,
          new DecimalFormat("0.#"));
      FormEntry feSpinnerPrecTolHi = new FormEntry(MsfraggerParams.PROP_precursor_mass_upper,
          "not-shown", uiSpinnerPrecTolHi);
      FormEntry feAdjustPrecMass = new FormEntry(PROP_adjust_precurosr_mass, "<html><i>Adjust precursor mass",
          new UiCheck("<html><i>Adjust precursor mass", null),
          "<html>Run a separate program to trace MS1 peaks <br/>over LC time and use obtained averaged masses.");
      pPeakMatch.add(fePrecTolUnits.label(), new CC().alignX("right"));
      pPeakMatch.add(fePrecTolUnits.comp, new CC());
      pPeakMatch.add(feSpinnerPrecTolLo.comp, new CC().minWidth("45px"));
      pPeakMatch.add(new JLabel("-"), new CC().span(2));
      pPeakMatch.add(feSpinnerPrecTolHi.comp, new CC().minWidth("45px"));
      pPeakMatch.add(feAdjustPrecMass.comp, new CC().gapLeft("5px").wrap());

      // fragment mass tolerance
      UiCombo comboFragTolUnits = new UiCombo();
      comboFragTolUnits.setModel(new DefaultComboBoxModel<>(
          Arrays.stream(MassTolUnits.values()).map(MassTolUnits::name).toArray(String[]::new)));
      FormEntry feFragTolUnits = new FormEntry(MsfraggerParams.PROP_fragment_mass_units,
          "Fragment mass tolerance", comboFragTolUnits);
      UiSpinnerDouble uiSpinnerFragTol = new UiSpinnerDouble(10, 0, 10000, 1,
          new DecimalFormat("0.#"));
      FormEntry feFragTol = new FormEntry(MsfraggerParams.PROP_fragment_mass_tolerance, "not-shown",
          uiSpinnerFragTol);
      pPeakMatch.add(feFragTolUnits.label(), new CC().alignX("right"));
      pPeakMatch.add(feFragTolUnits.comp, new CC());
      pPeakMatch.add(feFragTol.comp, new CC().minWidth("45px").maxWidth("100px").growX().wrap());

      UiText uiTextIsoErr = new UiText();
      uiTextIsoErr.setDocument(DocumentFilters.getFilter("[^\\d/-]+"));
      uiTextIsoErr.setText("-1/0/1/2");
      FormEntry feIsotopeError = new FormEntry(MsfraggerParams.PROP_isotope_error, "Isotope error", uiTextIsoErr,
          "<html>String of the form -1/0/1/2 indicating which isotopic<br/>peak selection errors MSFragger will try to correct.");
      pPeakMatch.add(feIsotopeError.label(), new CC().alignX("right"));
      pPeakMatch.add(feIsotopeError.comp, new CC().minWidth("45px").span(2).growX().wrap());

      FormEntry feShiftedIonsCheck = new FormEntry(MsfraggerParams.PROP_shifted_ions, "not-shown",
          new UiCheck("<html>Use shifted ion series", null),
          "<html>Shifted ion series are the same as regular b/y ions,<br/>"
              + "but with the addition of the mass shift of the precursor.<br/>"
              + "Regular ion series will still be used.");
      UiText uiTextShiftedIonsExclusion = new UiText();
      uiTextShiftedIonsExclusion.setDocument(DocumentFilters.getFilter("[A-Za-z]"));
      uiTextShiftedIonsExclusion.setText("(-1.5,3.5)");
      FormEntry feShiftedIonsExclusion = new FormEntry(MsfraggerParams.PROP_shifted_ions_exclude_ranges, "Shifted ions exclusion ranges",
          uiTextShiftedIonsExclusion, "<html>Ranges expressed like: (-1.5,3.5)");
      pPeakMatch.add(feShiftedIonsCheck.comp, new CC().alignX("right"));
      pPeakMatch.add(feShiftedIonsExclusion.label(), new CC().split(2).spanX());
      pPeakMatch.add(feShiftedIonsExclusion.comp, new CC().growX());

      // Digest panel
      JPanel pDigest = new JPanel(new MigLayout(new LC().debug()));
      pDigest.setBorder(new TitledBorder("Protein Digestion"));

      FormEntry feEnzymeName = new FormEntry(MsfraggerParams.PROP_search_enzyme_name, "Enzyme name", new UiText());
      FormEntry feCutAfter = new FormEntry(MsfraggerParams.PROP_search_enzyme_cutafter, "Cut after",
          UiUtils.createUiText("[^A-Z]"), "Capital letters for amino acids after which the enzyme cuts.");
      FormEntry feButNotBefore = new FormEntry(MsfraggerParams.PROP_search_enzyme_butnotafter, "But not before",
          UiUtils.createUiText("[^A-Z]"), "Amino acids before which the enzyme won't cut.");
      pDigest.add(feEnzymeName.label(), new CC().alignX("right"));
      pDigest.add(feEnzymeName.comp, new CC().minWidth("120px").growX());
      pDigest.add(feCutAfter.label(), new CC().split(2).gapLeft("5px"));
      pDigest.add(feCutAfter.comp, new CC().minWidth("45px"));
      pDigest.add(feButNotBefore.label(), new CC().split(2).gapLeft("5px"));
      pDigest.add(feButNotBefore.comp, new CC().minWidth("45px").wrap());

      List<String> cleavageTypeNames = Arrays.stream(CleavageType.values()).map(Enum::name)
          .collect(Collectors.toList());
      FormEntry feCleavageType = new FormEntry(MsfraggerParams.PROP_num_enzyme_termini, "Cleavage", UiUtils.createUiCombo(cleavageTypeNames));
      UiSpinnerInt uiSpinnerMissedCleavages = new UiSpinnerInt(1, 0, 1000, 1);
      uiSpinnerMissedCleavages.setColumns(6);
      FormEntry feMissedCleavages = new FormEntry(MsfraggerParams.PROP_allowed_missed_cleavage, "Missed cleavages", uiSpinnerMissedCleavages);
      pDigest.add(feCleavageType.label(), new CC().alignX("right"));
      pDigest.add(feCleavageType.comp, new CC().minWidth("120px").growX());
      pDigest.add(feMissedCleavages.label(), new CC().alignX("right"));
      pDigest.add(feMissedCleavages.comp, new CC().minWidth("45px").wrap());

      FormEntry fePepLenMin = new FormEntry(MsfraggerParams.PROP_digest_min_length, "Peptide length", new UiSpinnerInt(7, 0, 1000, 1, 3));
      FormEntry fePepLenMax = new FormEntry(MsfraggerParams.PROP_digest_max_length, "not-shown", new UiSpinnerInt(50, 0, 1000, 1, 3));
      UiSpinnerDouble uiSpinnerDigestMassLo = new UiSpinnerDouble(200, 0, 50000, 100, new DecimalFormat("0.#"));
      uiSpinnerDigestMassLo.setColumns(6);
      FormEntry fePepMassLo = new FormEntry(PROP_digest_mass_lo, "Peptide mass range", uiSpinnerDigestMassLo);
      UiSpinnerDouble uiSpinnerDigestMassHi = new UiSpinnerDouble(5000, 0, 50000, 100, new DecimalFormat("0.#"));
      uiSpinnerDigestMassHi.setColumns(6);
      FormEntry fePepMassHi = new FormEntry(PROP_digest_mass_hi, "not-shown", uiSpinnerDigestMassHi);
      pDigest.add(fePepLenMin.label(), new CC().alignX("right"));
      pDigest.add(fePepLenMin.comp, new CC().split(3).growX());
      pDigest.add(new JLabel("-"));
      pDigest.add(fePepLenMax.comp, new CC());
      pDigest.add(fePepMassLo.label(), new CC().alignX("right"));
      pDigest.add(fePepMassLo.comp, new CC().split(3).spanX());
      pDigest.add(new JLabel("-"));
      pDigest.add(fePepMassHi.comp, new CC().wrap());


      pBase.add(pPeakMatch, new CC().wrap().growX());
      pBase.add(pDigest, new CC().wrap().growX());
      pContent.add(pBase, new CC().wrap().growX());
    }

    // Panel with all the advanced options
    {
      JPanel pAdvanced = new JPanel(new MigLayout(new LC()));
      pAdvanced.setBorder(new TitledBorder("Advanced Options"));
      pAdvanced.add(new JLabel("Advanced Options panel"), new CC().wrap());
      pAdvanced.add(new JLabel("Advanced Options panel"), new CC().wrap());
      pAdvanced.add(new JLabel("Advanced Options panel"), new CC().wrap());
      pAdvanced.add(new JLabel("Advanced Options panel"), new CC().wrap());
      pAdvanced.add(new JLabel("Advanced Options panel"), new CC().wrap());
      pAdvanced.add(new JLabel("Advanced Options panel"), new CC().wrap());
      pAdvanced.add(new JLabel("Advanced Options panel"), new CC().wrap());
      pAdvanced.add(new JLabel("Advanced Options panel"), new CC().wrap());
      pAdvanced.add(new JLabel("Advanced Options panel"), new CC().wrap());
      pAdvanced.add(new JLabel("Advanced Options panel"), new CC().wrap());
      pAdvanced.add(new JLabel("Advanced Options panel"), new CC().wrap());
      pAdvanced.add(new JLabel("Advanced Options panel"), new CC().wrap());
      pAdvanced.add(new JLabel("Advanced Options panel"), new CC().wrap());
      pAdvanced.add(new JLabel("Advanced Options panel"), new CC().wrap());
      pAdvanced.add(new JLabel("Advanced Options panel"), new CC().wrap());
      pAdvanced.add(new JLabel("Advanced Options panel"), new CC().wrap());
      pAdvanced.add(new JLabel("Advanced Options panel"), new CC().wrap());
      pAdvanced.add(new JLabel("Advanced Options panel"), new CC().wrap());
      pAdvanced.add(new JLabel("Advanced Options panel"), new CC().wrap());
      pAdvanced.add(new JLabel("Advanced Options panel"), new CC().wrap());
      pAdvanced.add(new JLabel("Advanced Options panel"), new CC().wrap());
      pAdvanced.add(new JLabel("Advanced Options panel"), new CC().wrap());
      pAdvanced.add(new JLabel("Advanced Options panel"), new CC().wrap());
      pAdvanced.add(new JLabel("Advanced Options panel"), new CC().wrap());
      pAdvanced.add(new JLabel("Advanced Options panel"), new CC().wrap());

      pContent.add(pAdvanced, new CC().wrap().growX());
    }

    this.add(scroll, BorderLayout.CENTER);
  }
}
