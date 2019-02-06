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
import java.awt.Component;
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
import javax.swing.JTable;
import javax.swing.SwingUtilities;
import javax.swing.border.EmptyBorder;
import javax.swing.border.TitledBorder;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableModel;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.greenrobot.eventbus.EventBus;
import umich.msfragger.gui.ModificationsTableModel;
import umich.msfragger.gui.api.SearchTypeProp;
import umich.msfragger.gui.renderers.TableCellDoubleRenderer;
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

  private static final String[] TABLE_VAR_MODS_COL_NAMES = {"Enabled", "Site (editable)", "Mass Delta (editable)"};
  private ModificationsTableModel tableModelVarMods;
  private static final String[] TABLE_ADD_MODS_COL_NAMES = {"Enabled", "Site", "Mass Delta (editable)"};
  private ModificationsTableModel tableModelAddMods;
  private javax.swing.JTable tableVarMods;
  private javax.swing.JTable tableAddMods;

  private static final String PROP_adjust_precurosr_mass = "misc.adjust-precursor-mass";
  private static final String PROP_ram = "misc.ram";
  private static final String PROP_digest_mass_lo = "misc.digest-mass-lo";
  private static final String PROP_digest_mass_hi = "misc.digest-mass-hi";
  private static final String PROP_slice_db = "misc.slice-db";
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
      FormEntry feAdjustPrecMass = new FormEntry(PROP_adjust_precurosr_mass, "not-shown",
          new UiCheck("<html><i>Adjust precursor mass", null),
          "<html>Correct monoisotopic mass determination erros.<br/>Requires MSFragger 20180924+.");
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
          UiUtils.uiTextBuilder().cols(6).filter("[^A-Z]").text("KR").create(), "Capital letters for amino acids after which the enzyme cuts.");
      FormEntry feButNotBefore = new FormEntry(MsfraggerParams.PROP_search_enzyme_butnotafter, "But not before",
          UiUtils.uiTextBuilder().cols(6).filter("[^A-Z]").text("P").create(), "Amino acids before which the enzyme won't cut.");
      pDigest.add(feEnzymeName.label(), new CC().alignX("right"));
      pDigest.add(feEnzymeName.comp, new CC().minWidth("120px").growX());
      pDigest.add(feCutAfter.label(), new CC().split(4).spanX().gapLeft("5px"));
      pDigest.add(feCutAfter.comp);//, new CC().minWidth("45px"));
      pDigest.add(feButNotBefore.label());//, new CC().split(2).spanX().gapLeft("5px"));
      pDigest.add(feButNotBefore.comp, new CC().wrap());

      List<String> cleavageTypeNames = Arrays.stream(CleavageType.values()).map(Enum::name)
          .collect(Collectors.toList());
      FormEntry feCleavageType = new FormEntry(MsfraggerParams.PROP_num_enzyme_termini, "Cleavage", UiUtils.createUiCombo(cleavageTypeNames));
      UiSpinnerInt uiSpinnerMissedCleavages = new UiSpinnerInt(1, 0, 1000, 1);
      uiSpinnerMissedCleavages.setColumns(6);
      FormEntry feMissedCleavages = new FormEntry(MsfraggerParams.PROP_allowed_missed_cleavage, "Missed cleavages", uiSpinnerMissedCleavages);
      FormEntry feClipM = new FormEntry(MsfraggerParams.PROP_clip_nTerm_M, "not-shown", new UiCheck("Clip N-term M", null),
          "Trim protein N-terminal Methionine as a variable modification");
      pDigest.add(feCleavageType.label(), new CC().alignX("right"));
      pDigest.add(feCleavageType.comp, new CC().minWidth("120px").growX());
      pDigest.add(feMissedCleavages.label(), new CC().alignX("right"));
      pDigest.add(feMissedCleavages.comp, new CC());
      pDigest.add(feClipM.comp, new CC().gapLeft("5px").wrap());

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

      FormEntry feMaxFragCharge = new FormEntry(MsfraggerParams.PROP_max_fragment_charge, "Max fragment charge", new UiSpinnerInt(2, 0, 20, 1, 2));
      FormEntry feSliceDb = new FormEntry(PROP_slice_db, "<html><i>Slice up database", new UiSpinnerInt(1, 1, 99, 1, 2),
          "<html>Split database into smaller chunks.<br/>Only use for very large databases (200MB+) or<br/>non-specific digestion.");
      pDigest.add(feMaxFragCharge.label(), new CC().split(2).span(2).alignX("right"));
      pDigest.add(feMaxFragCharge.comp);
      pDigest.add(feSliceDb.label(), new CC().alignX("right"));
      pDigest.add(feSliceDb.comp, new CC().spanX().wrap());

      pBase.add(pPeakMatch, new CC().wrap().growX());
      pBase.add(pDigest, new CC().wrap().growX());

      pContent.add(pBase, new CC().wrap().growX());
    }

    // Panel with modifications
    {
      JPanel pMods = new JPanel(new MigLayout(new LC().fillX()));
      pMods.setBorder(new TitledBorder("Modifications"));

      JPanel pVarmods = new JPanel(new MigLayout(new LC()));
      pVarmods.setBorder(new TitledBorder("Variable modifications"));

      FormEntry feMaxVarmodsPerMod = new FormEntry(MsfraggerParams.PROP_max_variable_mods_per_mod, "Max variable mods per mod",
          new UiSpinnerInt(3, 0, 100, 1, 4));
      FormEntry feMaxCombos = new FormEntry(MsfraggerParams.PROP_max_variable_mods_combinations, "Max combinations",
          new UiSpinnerInt(5000, 0, 100000, 500, 4));
      FormEntry feMultipleVarModsOnResidue = new FormEntry(MsfraggerParams.PROP_allow_multiple_variable_mods_on_residue,
          "not-shown", new UiCheck("Allow multiple variable mods on residue", null));
      pVarmods.add(feMaxVarmodsPerMod.label(), new CC().alignX("right"));
      pVarmods.add(feMaxVarmodsPerMod.comp);
      pVarmods.add(feMaxCombos.label(), new CC().alignX("right"));
      pVarmods.add(feMaxCombos.comp);
      pVarmods.add(feMultipleVarModsOnResidue.comp, new CC().wrap());

      tableVarMods = new JTable();
      tableVarMods.setModel(getDefaultVarModTableModel());
      tableVarMods.setToolTipText("<html>Variable Modifications.<br/>\nValues:<br/>\n<ul>\n<li>A-Z amino acid codes</li>\n<li>*​ ​is​ ​used​ ​to​ ​represent​ ​any​ ​amino​ ​acid</li>\n<li>^​ ​is​ ​used​ ​to​ ​represent​ ​a​ ​terminus</li>\n<li>[​ ​is​ ​a​ ​modifier​ ​for​ ​protein​ ​N-terminal</li>\n<li>]​ ​is​ ​a​ ​modifier​ ​for​ ​protein​ ​C-terminal</li>\n<li>n​ ​is​ ​a​ ​modifier​ ​for​ ​peptide​ ​N-terminal</li>\n<li>c​ ​is​ ​a​ ​modifier​ ​for​ ​peptide​ ​C-terminal</li>\n</ul>\nSyntax​ ​Examples:\n<ul>\n<li>15.9949​ ​M​ ​(for​ ​oxidation​ ​on​ ​methionine)</li>\n<li>79.66331​ ​STY​ ​(for​ ​phosphorylation)</li>\n<li>-17.0265​ ​nQnC​ ​(for​ ​pyro-Glu​ ​or​ ​loss​ ​of​ ​ammonia​ ​at peptide​ ​N-terminal)</li>\n</ul>\nExample​ ​(M​ ​oxidation​ ​and​ ​N-terminal​ ​acetylation):\n<ul>\n<li>variable_mod_01​ ​=​ ​15.9949​ ​M</li>\n<li>variable_mod_02​ ​=​ ​42.0106​ ​[^</li>\n</ul>");
      tableVarMods.setDefaultRenderer(Double.class, new TableCellDoubleRenderer());
      tableVarMods.setFillsViewportHeight(true);
      SwingUtilities.invokeLater(() -> {
        tableVarMods.getColumnModel().getColumn(0).setMaxWidth(150);
        tableVarMods.getColumnModel().getColumn(0).setMinWidth(20);
        tableVarMods.getColumnModel().getColumn(0).setPreferredWidth(50);
      });

      JScrollPane tableScrollVarMods = new JScrollPane(tableVarMods, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
      //tableScrollVarMods.setPreferredSize(new Dimension(tableScrollVarMods.getPreferredSize().width, 140));
      pVarmods.add(tableScrollVarMods, new CC().minHeight("100px").maxHeight("150px").growX().spanX().wrap());

      JPanel pFixmods = new JPanel(new MigLayout(new LC()));
      pFixmods.setBorder(new TitledBorder("Fixed modifications"));

      pMods.add(pVarmods, new CC().wrap().growX());
      pMods.add(pFixmods, new CC().wrap().growX());
      pContent.add(pMods, new CC().wrap().growX());
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

  private synchronized TableModel getDefaultVarModTableModel() {
    if (tableModelVarMods != null)
      return tableModelVarMods;
    int cols = 3;
    Object[][] data = new Object[MsfraggerParams.VAR_MOD_COUNT_MAX][cols];
    for (int i = 0; i < data.length; i++) {
      data[i][0] = false;
      data[i][1] = null;
      data[i][2] = null;
    }

    tableModelVarMods = new ModificationsTableModel(
        TABLE_VAR_MODS_COL_NAMES,
        new Class<?>[] { Boolean.class, String.class, Double.class },
        new boolean[] {true, true, true},
        new int[] {0, 1, 2},
        data);

    return tableModelVarMods;
  }

  private synchronized TableModel getDefaultAddonTableModel() {
    if (tableModelAddMods != null)
      return tableModelAddMods;

    int cols = 3;
    Object[][] data = new Object[MsfraggerParams.ADDONS_HUMAN_READABLE.length][cols];
    for (int i = 0; i < data.length; i++) {
      data[i][0] = false;
      data[i][1] = MsfraggerParams.ADDONS_HUMAN_READABLE[i];
      data[i][2] = 0.0;
    }

    tableModelAddMods = new ModificationsTableModel(
        TABLE_ADD_MODS_COL_NAMES,
        new Class<?>[] {Boolean.class, String.class, Double.class},
        new boolean[] {true, false, true},
        new int[] {0, 1, 2},
        data);

    return tableModelAddMods;
  }

  private void updateRowHeights(JTable table) {
    for (int row = 0; row < table.getRowCount(); row++) {
      int rowHeight = table.getRowHeight();

      for (int column = 0; column < table.getColumnCount(); column++) {
        Component comp = table.prepareRenderer(table.getCellRenderer(row, column), row, column);
        rowHeight = Math.max(rowHeight, comp.getPreferredSize().height);
      }

      table.setRowHeight(row, rowHeight);
    }
  }
}
