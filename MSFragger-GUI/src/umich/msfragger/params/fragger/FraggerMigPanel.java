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
import com.github.chhh.utils.swing.UiSpinnerDouble;
import com.github.chhh.utils.swing.UiSpinnerInt;
import com.github.chhh.utils.swing.UiText;
import com.github.chhh.utils.swing.UiUtils;
import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Container;
import java.io.File;
import java.io.FileInputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.DecimalFormat;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.stream.Collectors;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.SwingUtilities;
import javax.swing.border.EmptyBorder;
import javax.swing.border.TitledBorder;
import javax.swing.filechooser.FileNameExtensionFilter;
import javax.swing.table.TableModel;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.greenrobot.eventbus.EventBus;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import umich.msfragger.gui.ModificationsTableModel;
import umich.msfragger.gui.api.SearchTypeProp;
import umich.msfragger.gui.renderers.TableCellDoubleRenderer;
import umich.msfragger.messages.MessageSearchType;
import umich.msfragger.params.Props.Prop;
import umich.msfragger.params.ThisAppProps;
import umich.msfragger.params.enums.CleavageType;
import umich.msfragger.params.enums.FraggerOutputType;
import umich.msfragger.params.enums.FraggerPrecursorMassMode;
import umich.msfragger.params.enums.MassTolUnits;
import umich.msfragger.util.SwingUtils;
import umich.msfragger.util.swing.FormEntry;

/**
 * @author Dmitry Avtonomov
 */
public class FraggerMigPanel extends JPanel {

  private static final Logger log = LoggerFactory.getLogger(FraggerMigPanel.class);

  private ImageIcon icon;
  private JCheckBox checkRun;
  private JScrollPane scroll;
  private JPanel pContent;

  private static final String[] TABLE_VAR_MODS_COL_NAMES = {"Enabled", "Site (editable)",
      "Mass Delta (editable)"};
  private ModificationsTableModel tableModelVarMods;
  private javax.swing.JTable tableVarMods;
  private static final String[] TABLE_ADD_MODS_COL_NAMES = {"Enabled", "Site",
      "Mass Delta (editable)"};
  private ModificationsTableModel tableModelFixMods;
  private javax.swing.JTable tableFixMods;

  private static final String PROP_misc_adjust_precurosr_mass = "misc.adjust-precursor-mass";
  private static final String PROP_misc_slice_db = "misc.slice-db";
  private static final String PROP_misc_ram = "misc.ram";
  private static final String PROP_misc_digest_mass_lo = "misc.digest-mass-lo";
  private static final String PROP_misc_digest_mass_hi = "misc.digest-mass-hi";
  private static final String PROP_misc_clear_mz_lo = "misc.clear-mz-lo";
  private static final String PROP_misc_clear_mz_hi = "misc.clear-mz-hi";
  private static final String PROP_misc_precursor_charge_lo = "misc.precursor-charge-lo";
  private static final String PROP_misc_precursor_charge_hi = "misc.precursor-charge-hi";

  private static String[] PROPS_MISC = {
      PROP_misc_adjust_precurosr_mass,
      PROP_misc_slice_db,
      PROP_misc_ram,
      PROP_misc_digest_mass_lo,
      PROP_misc_digest_mass_hi,
      PROP_misc_clear_mz_lo,
      PROP_misc_clear_mz_hi,
      PROP_misc_precursor_charge_lo,
      PROP_misc_precursor_charge_hi
  };

  private static final Set<String> PROPS_MISC_NAMES;

  static {
    PROPS_MISC_NAMES = new HashSet<>(Arrays.asList(PROPS_MISC));
  }

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
      load.addActionListener(e -> {
        JFileChooser fc = new JFileChooser();
        fc.setApproveButtonText("Load");
        fc.setApproveButtonToolTipText("Load into the form");
        fc.setDialogTitle("Select saved file");
        fc.setMultiSelectionEnabled(false);

        fc.setAcceptAllFileFilterUsed(true);
        FileNameExtensionFilter filter = new FileNameExtensionFilter("Properties/Params",
            "properties", "params", "para", "conf", "txt");
        fc.setFileFilter(filter);

        final String propName = ThisAppProps.PROP_FRAGGER_PARAMS_FILE_IN;
        ThisAppProps.load(propName, fc);

        Component parent = SwingUtils.findParentFrameForDialog(this);
        int saveResult = fc.showOpenDialog(parent);
        if (JFileChooser.APPROVE_OPTION == saveResult) {
          File selectedFile = fc.getSelectedFile();
          Path path = Paths.get(selectedFile.getAbsolutePath());
          ThisAppProps.save(propName, path.toString());

          if (Files.exists(path)) {
            try {
              Map<String, String> collect = formToMap();
              MsfraggerParams params = mapToParams(collect);
              params.load(new FileInputStream(selectedFile), true);
              Map<String, String> paramsAsMap = paramsToMap(params);
              formFromMap(paramsAsMap);
              params.save();
            } catch (Exception ex) {
              JOptionPane
                  .showMessageDialog(parent, "<html>Could not load the saved file: <br/>" + ex.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
            }
          } else {
            JOptionPane.showMessageDialog(parent, "<html>This is strange,<br/> "
                + "but the file you chose to load doesn't exist anymore.", "Strange", JOptionPane.ERROR_MESSAGE);
          }
        }
      });
      FormEntry feRam = new FormEntry(PROP_misc_ram, "RAM (GB)", new UiSpinnerInt(0, 0, 1024, 1, 3));
      FormEntry feThreads = new FormEntry(MsfraggerParams.PROP_num_threads, "Threads",
          new UiSpinnerInt(0, 0, 128, 3));
      pTop.add(save, new CC().split(6).spanX());
      pTop.add(load, new CC());
      pTop.add(feRam.label(), new CC());
      pTop.add(feRam.comp, new CC());
      pTop.add(feThreads.label(), new CC());
      pTop.add(feThreads.comp, new CC());

      this.add(pTop, BorderLayout.NORTH);
    }

    pContent = new JPanel();
    pContent.setLayout(new MigLayout(new LC().fillX()));
    scroll = new JScrollPane(pContent);
    scroll.setBorder(new EmptyBorder(0, 0, 0, 0));
    scroll.getVerticalScrollBar().setUnitIncrement(16);

    // Panel with all the basic options
    {
      JPanel pBase = new JPanel(new MigLayout(new LC().fillX()));
      pBase.setBorder(
          new TitledBorder("Common Options (Advanced Options are at the end of the page)"));

      JPanel pPeakMatch = new JPanel(new MigLayout(new LC()));
      pPeakMatch.setBorder(new TitledBorder("Peak Matching"));

      // precursor mass tolerance
      FormEntry fePrecTolUnits = new FormEntry("precursor_mass_units", "Precursor mass tolerance",
          UiUtils.createUiCombo(MassTolUnits.values()));
      UiSpinnerDouble uiSpinnerPrecTolLo = new UiSpinnerDouble(-10, -10000, 10000, 1,
          new DecimalFormat("0.#"));
      uiSpinnerPrecTolLo.setColumns(4);
      FormEntry feSpinnerPrecTolLo = new FormEntry(MsfraggerParams.PROP_precursor_mass_lower,
          "not-shown", uiSpinnerPrecTolLo);
      UiSpinnerDouble uiSpinnerPrecTolHi = new UiSpinnerDouble(+10, -10000, 10000, 1,
          new DecimalFormat("0.#"));
      uiSpinnerPrecTolHi.setColumns(4);
      FormEntry feSpinnerPrecTolHi = new FormEntry(MsfraggerParams.PROP_precursor_mass_upper,
          "not-shown", uiSpinnerPrecTolHi);
      FormEntry feAdjustPrecMass = new FormEntry(PROP_misc_adjust_precurosr_mass, "not-shown",
          new UiCheck("<html><i>Adjust precursor mass", null),
          "<html>Correct monoisotopic mass determination erros.<br/>Requires MSFragger 20180924+.");
      pPeakMatch.add(fePrecTolUnits.label(), new CC().alignX("right"));
      pPeakMatch.add(fePrecTolUnits.comp, new CC());
      pPeakMatch.add(feSpinnerPrecTolLo.comp, new CC());
      pPeakMatch.add(new JLabel("-"), new CC().span(2));
      pPeakMatch.add(feSpinnerPrecTolHi.comp, new CC());
      pPeakMatch.add(feAdjustPrecMass.comp, new CC().gapLeft("5px").wrap());

      // fragment mass tolerance
      FormEntry feFragTolUnits = new FormEntry(MsfraggerParams.PROP_fragment_mass_units,
          "Fragment mass tolerance", UiUtils.createUiCombo(MassTolUnits.values()));
      UiSpinnerDouble uiSpinnerFragTol = new UiSpinnerDouble(10, 0, 10000, 1,
          new DecimalFormat("0.#"));
      uiSpinnerFragTol.setColumns(4);
      FormEntry feFragTol = new FormEntry(MsfraggerParams.PROP_fragment_mass_tolerance, "not-shown",
          uiSpinnerFragTol);
      pPeakMatch.add(feFragTolUnits.label(), new CC().alignX("right"));
      pPeakMatch.add(feFragTolUnits.comp, new CC());
      pPeakMatch.add(feFragTol.comp, new CC().wrap());

      UiText uiTextIsoErr = new UiText();
      uiTextIsoErr.setDocument(DocumentFilters.getFilter("[^\\d/-]+"));
      uiTextIsoErr.setText("-1/0/1/2");
      FormEntry feIsotopeError = new FormEntry(MsfraggerParams.PROP_isotope_error, "Isotope error",
          uiTextIsoErr,
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
      FormEntry feShiftedIonsExclusion = new FormEntry(
          MsfraggerParams.PROP_shifted_ions_exclude_ranges, "Shifted ions exclusion ranges",
          uiTextShiftedIonsExclusion, "<html>Ranges expressed like: (-1.5,3.5)");
      pPeakMatch.add(feShiftedIonsCheck.comp, new CC().alignX("right"));
      pPeakMatch.add(feShiftedIonsExclusion.label(), new CC().split(2).spanX());
      pPeakMatch.add(feShiftedIonsExclusion.comp, new CC().growX());

      // Digest panel
      JPanel pDigest = new JPanel(new MigLayout(new LC()));
      pDigest.setBorder(new TitledBorder("Protein Digestion"));

      FormEntry feEnzymeName = new FormEntry(MsfraggerParams.PROP_search_enzyme_name, "Enzyme name",
          new UiText());
      FormEntry feCutAfter = new FormEntry(MsfraggerParams.PROP_search_enzyme_cutafter, "Cut after",
          UiUtils.uiTextBuilder().cols(6).filter("[^A-Z]").text("KR").create(),
          "Capital letters for amino acids after which the enzyme cuts.");
      FormEntry feButNotBefore = new FormEntry(MsfraggerParams.PROP_search_enzyme_butnotafter,
          "But not before",
          UiUtils.uiTextBuilder().cols(6).filter("[^A-Z]").text("P").create(),
          "Amino acids before which the enzyme won't cut.");
      pDigest.add(feEnzymeName.label(), new CC().alignX("right"));
      pDigest.add(feEnzymeName.comp, new CC().minWidth("120px").growX());
      pDigest.add(feCutAfter.label(), new CC().split(4).spanX().gapLeft("5px"));
      pDigest.add(feCutAfter.comp);//, new CC().minWidth("45px"));
      pDigest.add(feButNotBefore.label());//, new CC().split(2).spanX().gapLeft("5px"));
      pDigest.add(feButNotBefore.comp, new CC().wrap());

      List<String> cleavageTypeNames = Arrays.stream(CleavageType.values()).map(Enum::name)
          .collect(Collectors.toList());
      FormEntry feCleavageType = new FormEntry(MsfraggerParams.PROP_num_enzyme_termini, "Cleavage",
          UiUtils.createUiCombo(cleavageTypeNames));
      UiSpinnerInt uiSpinnerMissedCleavages = new UiSpinnerInt(1, 0, 1000, 1);
      uiSpinnerMissedCleavages.setColumns(6);
      FormEntry feMissedCleavages = new FormEntry(MsfraggerParams.PROP_allowed_missed_cleavage,
          "Missed cleavages", uiSpinnerMissedCleavages);
      FormEntry feClipM = new FormEntry(MsfraggerParams.PROP_clip_nTerm_M, "not-shown",
          new UiCheck("Clip N-term M", null),
          "Trim protein N-terminal Methionine as a variable modification");
      pDigest.add(feCleavageType.label(), new CC().alignX("right"));
      pDigest.add(feCleavageType.comp, new CC().minWidth("120px").growX());
      pDigest.add(feMissedCleavages.label(), new CC().alignX("right"));
      pDigest.add(feMissedCleavages.comp, new CC());
      pDigest.add(feClipM.comp, new CC().gapLeft("5px").wrap());

      FormEntry fePepLenMin = new FormEntry(MsfraggerParams.PROP_digest_min_length,
          "Peptide length", new UiSpinnerInt(7, 0, 1000, 1, 3));
      FormEntry fePepLenMax = new FormEntry(MsfraggerParams.PROP_digest_max_length, "not-shown",
          new UiSpinnerInt(50, 0, 1000, 1, 3));
      UiSpinnerDouble uiSpinnerDigestMassLo = new UiSpinnerDouble(200, 0, 50000, 100,
          new DecimalFormat("0.#"));
      uiSpinnerDigestMassLo.setColumns(6);
      FormEntry fePepMassLo = new FormEntry(PROP_misc_digest_mass_lo, "Peptide mass range",
          uiSpinnerDigestMassLo);
      UiSpinnerDouble uiSpinnerDigestMassHi = new UiSpinnerDouble(5000, 0, 50000, 100,
          new DecimalFormat("0.#"));
      uiSpinnerDigestMassHi.setColumns(6);
      FormEntry fePepMassHi = new FormEntry(PROP_misc_digest_mass_hi, "not-shown",
          uiSpinnerDigestMassHi);
      pDigest.add(fePepLenMin.label(), new CC().alignX("right"));
      pDigest.add(fePepLenMin.comp, new CC().split(3).growX());
      pDigest.add(new JLabel("-"));
      pDigest.add(fePepLenMax.comp, new CC());
      pDigest.add(fePepMassLo.label(), new CC().alignX("right"));
      pDigest.add(fePepMassLo.comp, new CC().split(3).spanX());
      pDigest.add(new JLabel("-"));
      pDigest.add(fePepMassHi.comp, new CC().wrap());

      FormEntry feMaxFragCharge = new FormEntry(MsfraggerParams.PROP_max_fragment_charge,
          "Max fragment charge", new UiSpinnerInt(2, 0, 20, 1, 2));
      FormEntry feSliceDb = new FormEntry(PROP_misc_slice_db, "<html><i>Slice up database",
          new UiSpinnerInt(1, 1, 99, 1, 2),
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

      FormEntry feMaxVarmodsPerMod = new FormEntry(MsfraggerParams.PROP_max_variable_mods_per_mod,
          "Max variable mods per mod",
          new UiSpinnerInt(3, 0, 100, 1, 4));
      FormEntry feMaxCombos = new FormEntry(MsfraggerParams.PROP_max_variable_mods_combinations,
          "Max combinations",
          new UiSpinnerInt(5000, 0, 100000, 500, 4));
      FormEntry feMultipleVarModsOnResidue = new FormEntry(
          MsfraggerParams.PROP_allow_multiple_variable_mods_on_residue,
          "not-shown", new UiCheck("Allow multiple variable mods on residue", null));
      tableVarMods = new JTable();
      tableVarMods.setModel(getDefaultVarModTableModel());
      tableVarMods.setToolTipText(
          "<html>Variable Modifications.<br/>\nValues:<br/>\n<ul>\n<li>A-Z amino acid codes</li>\n<li>*​ ​is​ ​used​ ​to​ ​represent​ ​any​ ​amino​ ​acid</li>\n<li>^​ ​is​ ​used​ ​to​ ​represent​ ​a​ ​terminus</li>\n<li>[​ ​is​ ​a​ ​modifier​ ​for​ ​protein​ ​N-terminal</li>\n<li>]​ ​is​ ​a​ ​modifier​ ​for​ ​protein​ ​C-terminal</li>\n<li>n​ ​is​ ​a​ ​modifier​ ​for​ ​peptide​ ​N-terminal</li>\n<li>c​ ​is​ ​a​ ​modifier​ ​for​ ​peptide​ ​C-terminal</li>\n</ul>\nSyntax​ ​Examples:\n<ul>\n<li>15.9949​ ​M​ ​(for​ ​oxidation​ ​on​ ​methionine)</li>\n<li>79.66331​ ​STY​ ​(for​ ​phosphorylation)</li>\n<li>-17.0265​ ​nQnC​ ​(for​ ​pyro-Glu​ ​or​ ​loss​ ​of​ ​ammonia​ ​at peptide​ ​N-terminal)</li>\n</ul>\nExample​ ​(M​ ​oxidation​ ​and​ ​N-terminal​ ​acetylation):\n<ul>\n<li>variable_mod_01​ ​=​ ​15.9949​ ​M</li>\n<li>variable_mod_02​ ​=​ ​42.0106​ ​[^</li>\n</ul>");
      tableVarMods.setDefaultRenderer(Double.class, new TableCellDoubleRenderer());
      tableVarMods.setFillsViewportHeight(true);
      SwingUtilities.invokeLater(() -> {
        tableVarMods.getColumnModel().getColumn(0).setMaxWidth(150);
        tableVarMods.getColumnModel().getColumn(0).setMinWidth(20);
        tableVarMods.getColumnModel().getColumn(0).setPreferredWidth(50);
      });
      JScrollPane tableScrollVarMods = new JScrollPane(tableVarMods,
          JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
      //tableScrollVarMods.setPreferredSize(new Dimension(tableScrollVarMods.getPreferredSize().width, 140));

      pVarmods.add(feMaxVarmodsPerMod.label(), new CC().alignX("right"));
      pVarmods.add(feMaxVarmodsPerMod.comp);
      pVarmods.add(feMaxCombos.label(), new CC().alignX("right"));
      pVarmods.add(feMaxCombos.comp);
      pVarmods.add(feMultipleVarModsOnResidue.comp, new CC().wrap());
      pVarmods
          .add(tableScrollVarMods, new CC().minHeight("100px").maxHeight("150px").spanX().wrap());

      JPanel pFixmods = new JPanel(new MigLayout(new LC()));
      pFixmods.setBorder(new TitledBorder("Fixed modifications"));

      tableFixMods = new JTable();
      tableFixMods.setModel(getDefaultFixModTableModel());
      tableFixMods.setToolTipText(
          "<html>Fixed Modifications.<br/>Act as if the mass of aminoacids/termini was permanently changed.");
      tableFixMods.setDefaultRenderer(Double.class, new TableCellDoubleRenderer());
      tableFixMods.setFillsViewportHeight(true);
      SwingUtilities.invokeLater(() -> {
        tableFixMods.getColumnModel().getColumn(0).setMaxWidth(150);
        tableFixMods.getColumnModel().getColumn(0).setMinWidth(20);
        tableFixMods.getColumnModel().getColumn(0).setPreferredWidth(50);
      });
      JScrollPane tableScrollFixMods = new JScrollPane(tableFixMods,
          JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
      pFixmods.add(tableScrollFixMods,
          new CC().minHeight("100px").maxHeight("200px").growX().spanX().wrap());

      pMods.add(pVarmods, new CC().wrap().growX());
      pMods.add(pFixmods, new CC().wrap().growX());
      pContent.add(pMods, new CC().wrap().growX());
    }

    // Panel with all the advanced options
    {
      JPanel pAdvanced = new JPanel(new MigLayout(new LC()));
      pAdvanced.setBorder(new TitledBorder("Advanced Options"));

      CC alignRight = new CC().alignX("right");
      CC wrap = new CC().wrap();

      {
        JPanel pOpenSearch = new JPanel(new MigLayout(new LC()));
        pOpenSearch.setBorder(new TitledBorder("Open Search Options"));

        FormEntry feTrackZeroTopN = new FormEntry(MsfraggerParams.PROP_track_zero_topN,
            "Track zero top N",
            new UiSpinnerInt(0, 0, 1000, 5, 3));
        FormEntry feAddTopNComplementary = new FormEntry(
            MsfraggerParams.PROP_add_topN_complementary, "Add top N complementary",
            new UiSpinnerInt(0, 0, 1000, 2, 3));
        UiSpinnerDouble spinnerZeroBinAcceptExpect = new UiSpinnerDouble(0, 0, Double.MAX_VALUE,
            0.1, 1,
            new DecimalFormat("0.00"));
        spinnerZeroBinAcceptExpect.setColumns(3);
        FormEntry feZeroBinAcceptExpect = new FormEntry(MsfraggerParams.PROP_zero_bin_accept_expect,
            "Zero bin accept expect", spinnerZeroBinAcceptExpect);
        UiSpinnerDouble spinnerZeroBinMultExpect = new UiSpinnerDouble(1, 0, 1, 0.05, 2,
            new DecimalFormat("0.00"));
        spinnerZeroBinMultExpect.setColumns(3);
        FormEntry feZeroBinMultExpect = new FormEntry(MsfraggerParams.PROP_zero_bin_mult_expect,
            "Zero bin multiply expect",
            spinnerZeroBinMultExpect);

        pOpenSearch.add(feTrackZeroTopN.label(), alignRight);
        pOpenSearch.add(feTrackZeroTopN.comp);
        pOpenSearch.add(feAddTopNComplementary.label(), alignRight);
        pOpenSearch.add(feAddTopNComplementary.comp, wrap);
        pOpenSearch.add(feZeroBinAcceptExpect.label(), alignRight);
        pOpenSearch.add(feZeroBinAcceptExpect.comp);
        pOpenSearch.add(feZeroBinMultExpect.label(), alignRight);
        pOpenSearch.add(feZeroBinMultExpect.comp, wrap);

        pAdvanced.add(pOpenSearch, new CC().wrap().growX());
      }

      {
        JPanel pSpectral = new JPanel(new MigLayout(new LC()));
        pSpectral.setBorder(new TitledBorder("Spectral Processing"));

        FormEntry feMinPeaks = new FormEntry(MsfraggerParams.PROP_minimum_peaks, "Min peaks",
            new UiSpinnerInt(15, 0, 1000, 1, 4));
        FormEntry feUseTopN = new FormEntry(MsfraggerParams.PROP_use_topN_peaks, "Use top N peaks",
            new UiSpinnerInt(100, 0, 1000000, 10, 4));
        FormEntry feMinFragsModeling = new FormEntry(MsfraggerParams.PROP_min_fragments_modelling,
            "Min frags modeling", new UiSpinnerInt(3, 0, 1000, 1, 4));
        FormEntry feMinMatchedFrags = new FormEntry(MsfraggerParams.PROP_min_matched_fragments,
            "Min matched frags", new UiSpinnerInt(4, 0, 1000, 1, 4));
        UiSpinnerDouble spinnerMinRatio = new UiSpinnerDouble(0.01, 0, Double.MAX_VALUE, 0.1, 2,
            new DecimalFormat("0.00"));
        spinnerMinRatio.setColumns(4);
        FormEntry feMinRatio = new FormEntry(MsfraggerParams.PROP_minimum_ratio, "Min ratio",
            spinnerMinRatio);
        FormEntry feClearRangeMzLo = new FormEntry(PROP_misc_clear_mz_lo, "Clear m/z range",
            new UiSpinnerInt(0, 0, 100000, 10, 4));
        FormEntry feClearRangeMzHi = new FormEntry(PROP_misc_clear_mz_hi, "not-shown",
            new UiSpinnerInt(0, 0, 100000, 10, 4));

        pSpectral.add(feMinPeaks.label(), alignRight);
        pSpectral.add(feMinPeaks.comp);
        pSpectral.add(feUseTopN.label(), alignRight);
        pSpectral.add(feUseTopN.comp, wrap);
        pSpectral.add(feMinFragsModeling.label(), alignRight);
        pSpectral.add(feMinFragsModeling.comp);
        pSpectral.add(feMinMatchedFrags.label(), alignRight);
        pSpectral.add(feMinMatchedFrags.comp);
        pSpectral.add(feMinRatio.label(), alignRight);
        pSpectral.add(feMinRatio.comp, wrap);
        pSpectral.add(feClearRangeMzLo.label(), alignRight);
        pSpectral.add(feClearRangeMzLo.comp, new CC().split(3).spanX());
        pSpectral.add(new JLabel("-"));
        pSpectral.add(feClearRangeMzHi.comp, new CC().wrap());

        pAdvanced.add(pSpectral, new CC().wrap().growX());
      }

      // Advanced peak matching panel
      {
        JPanel pPeakMatch = new JPanel(new MigLayout(new LC()));
        pPeakMatch.setBorder(new TitledBorder("Peak Matching Advanced Options"));

        FormEntry feTrueTolUnits = new FormEntry(MsfraggerParams.PROP_precursor_true_units,
            "Precursor true tolerance", UiUtils.createUiCombo(MassTolUnits.values()));
        UiSpinnerDouble uiSpinnerTrueTol = new UiSpinnerDouble(10, 0, 100000, 5,
            new DecimalFormat("0.#"));
        uiSpinnerTrueTol.setColumns(4);
        FormEntry feTrueTol = new FormEntry(MsfraggerParams.PROP_precursor_true_tolerance,
            "not-shown", uiSpinnerTrueTol, "<html>True precursor mass tolerance <br>\n"
            + "should be set to your instrument's \n"
            + "precursor mass accuracy <br>\n"
            + "(window is +/- this value).  This value is used \n"
            + "for tie breaking <br>\n"
            + "of results and boosting of unmodified peptides in open \n"
            + "search.<br>");
        FormEntry feReportTopN = new FormEntry(MsfraggerParams.PROP_output_report_topN,
            "Report top N", new UiSpinnerInt(1, 1, 10000, 1, 4),
            "Report top N PSMs per input spectrum.");
        UiSpinnerDouble uiSpinnerOutputMaxExpect = new UiSpinnerDouble(50, 0, Double.MAX_VALUE, 1,
            new DecimalFormat("0.#"));
        uiSpinnerOutputMaxExpect.setColumns(4);
        FormEntry feOutputMaxExpect = new FormEntry(MsfraggerParams.PROP_output_max_expect,
            "Output max expect", uiSpinnerOutputMaxExpect,
            "<html>Suppresses reporting of PSM if top hit has<br> expectation greater "
                + "than this threshold");
        String tooltipMassOffsets = "<html>Mass_offsets in MSFragger creates multiple precursor \n"
            + "tolerance windows with<br>\n"
            + "specified mass offsets. These values are multiplexed \n"
            + "with the isotope error option. <br><br>\n"
            + "\n"
            + "For example, mass_offsets = 0/79.966 \n"
            + "can be used as a restricted \"open\" search that <br>\n"
            + "looks for unmodified and \n"
            + "phosphorylated peptides (on any residue).<br><br>\n"
            + "\n"
            + "Setting isotope_error to \n"
            + "0/1/2 in combination with this example will create <br>\n"
            + "search windows around \n"
            + "(0,1,2,79.966, 80.966, 81.966).";
        FormEntry feMassOffsets = new FormEntry(MsfraggerParams.PROP_mass_offsets, "Mass offsets",
            UiUtils.uiTextBuilder().filter("[^\\(\\)\\.,\\d ]").text("0").create(),
            tooltipMassOffsets);

        FormEntry feOutputType = new FormEntry(MsfraggerParams.PROP_output_format, "Output format",
            UiUtils.createUiCombo(FraggerOutputType.values()),
            "<html>How the search results are to be reported.<br>\n" +
                "Downstream tools only support PepXML format.<br><br>\n" +
                "Only use TSV (tab delimited file) if you want to process <br>\n" +
                "search resutls yourself for easier import into other software.<br>");
        FormEntry fePrecursorMassMode = new FormEntry(MsfraggerParams.PROP_precursor_mass_mode,
            "Precursor mass mode",
            UiUtils.createUiCombo(FraggerPrecursorMassMode.values()),
            "<html>Determines which entry from mzML files will be<br/>"
                + "used as the precursor's mass. 'Selected' or 'Isolated' ion.)");
        String tooltipPrecursorCHarge =
            "<html>Assume range of potential precursor charge states.<br>\n" +
                "Only relevant when override_charge is set to 1.<br>\n" +
                "Specified as space separated range of integers.<br>";
        FormEntry fePrecursorChargeLo = new FormEntry(PROP_misc_precursor_charge_lo, "with precursor charge",
            new UiSpinnerInt(1, 0, 30, 1, 2), tooltipPrecursorCHarge);
        FormEntry fePrecursorChargeHi = new FormEntry(PROP_misc_precursor_charge_hi, "not-shown",
            new UiSpinnerInt(4, 0, 30, 1, 2), tooltipPrecursorCHarge);
        FormEntry feOverrideCharge = new FormEntry(MsfraggerParams.PROP_override_charge,
            "not-shown", new UiCheck("Override charge", null),
            "<html>Ignores precursor charge and uses charge state<br>\n" +
                "specified in precursor_charge range.<br>");

        pPeakMatch.add(feTrueTolUnits.label(), alignRight);
        pPeakMatch.add(feTrueTolUnits.comp, new CC().split(2));
        pPeakMatch.add(feTrueTol.comp);
        pPeakMatch.add(feMassOffsets.label(), alignRight);
        pPeakMatch.add(feMassOffsets.comp, new CC().minWidth("45px").growX().wrap());

        pPeakMatch.add(feOverrideCharge.comp, alignRight);
        pPeakMatch.add(fePrecursorChargeLo.label(), new CC().split(4).spanX());
        pPeakMatch.add(fePrecursorChargeLo.comp);
        pPeakMatch.add(new JLabel("-"));
        pPeakMatch.add(fePrecursorChargeHi.comp, wrap);
        pPeakMatch.add(feReportTopN.label(), alignRight);
        pPeakMatch.add(feReportTopN.comp);
        pPeakMatch.add(feOutputMaxExpect.label(), alignRight);
        pPeakMatch.add(feOutputMaxExpect.comp, wrap);
        pPeakMatch.add(fePrecursorMassMode.label(), alignRight);
        pPeakMatch.add(fePrecursorMassMode.comp);
        pPeakMatch.add(feOutputType.label(), alignRight);
        pPeakMatch.add(feOutputType.comp, wrap);

        pAdvanced.add(pPeakMatch, new CC().wrap().growX());
      }

      pContent.add(pAdvanced, new CC().wrap().growX());
    }

    this.add(scroll, BorderLayout.CENTER);
  }

  private synchronized TableModel getDefaultVarModTableModel() {
    if (tableModelVarMods != null) {
      return tableModelVarMods;
    }
    int cols = 3;
    Object[][] data = new Object[MsfraggerParams.VAR_MOD_COUNT_MAX][cols];
    for (int i = 0; i < data.length; i++) {
      data[i][0] = false;
      data[i][1] = null;
      data[i][2] = null;
    }

    tableModelVarMods = new ModificationsTableModel(
        TABLE_VAR_MODS_COL_NAMES,
        new Class<?>[]{Boolean.class, String.class, Double.class},
        new boolean[]{true, true, true},
        new int[]{0, 1, 2},
        data);

    return tableModelVarMods;
  }

  private synchronized TableModel getDefaultFixModTableModel() {
    if (tableModelFixMods != null) {
      return tableModelFixMods;
    }

    int cols = 3;
    Object[][] data = new Object[MsfraggerParams.ADDONS_HUMAN_READABLE.length][cols];
    for (int i = 0; i < data.length; i++) {
      data[i][0] = false;
      data[i][1] = MsfraggerParams.ADDONS_HUMAN_READABLE[i];
      data[i][2] = 0.0;
    }

    tableModelFixMods = new ModificationsTableModel(
        TABLE_ADD_MODS_COL_NAMES,
        new Class<?>[]{Boolean.class, String.class, Double.class},
        new boolean[]{true, false, true},
        new int[]{0, 1, 2},
        data);

    return tableModelFixMods;
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

  private void enablePanels(boolean enabled) {
    SwingUtilities.invokeLater(() -> {
      for (Container c : Arrays.asList(pContent)) {
        SwingUtils.enableComponents(c, enabled);
      }
    });
  }

  private void formFromMap(Map<String, String> map) {
    SwingUtilities.invokeLater(() -> SwingUtils.valuesFromMap(pContent, map));
  }

  private Map<String, String> formToMap() {
    return SwingUtils.valuesToMap(pContent);
  }

  private MsfraggerParams mapToParams(Map<String, String> map) {
    MsfraggerParams p = new MsfraggerParams();
    for (Entry<String, String> e : map.entrySet()) {
      final String k = e.getKey();
      final String v = e.getValue();
      if (MsfraggerParams.PROP_NAMES_SET.contains(k)) {
        p.getProps().setProp(k, v);
      } else {
        // unknown prop, it better should be from the "misc" category we added in this panel
        if (PROPS_MISC_NAMES.contains(k) || k.startsWith("misc.")) {
          log.debug("Found misc option: {}={}", k, v);
        } else {
          // we don't know what this option is, someone probably forgot to add it to the list of
          // known ones
          log.debug("Unknown prop name in fragger panel: [{}] with value [{}]", k, v);
        }
      }
    }
    return p;
  }

  private Map<String, String> paramsToMap(MsfraggerParams params) {
    HashMap<String, String> map = new HashMap<>();
    for (Entry<String, Prop> e : params.getProps().getMap().entrySet()) {
      if (e.getValue().isEnabled) {
        map.put(e.getKey(), e.getValue().value);
      }
    }
    return map;
  }
}
