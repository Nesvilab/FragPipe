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
import com.github.chhh.utils.swing.StringRepresentable;
import com.github.chhh.utils.swing.UiCheck;
import com.github.chhh.utils.swing.UiCombo;
import com.github.chhh.utils.swing.UiSpinnerDouble;
import com.github.chhh.utils.swing.UiSpinnerInt;
import com.github.chhh.utils.swing.UiText;
import com.github.chhh.utils.swing.UiUtils;
import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Container;
import java.awt.event.ActionEvent;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.awt.event.ItemEvent;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.DecimalFormat;
import java.util.ArrayDeque;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.Properties;
import java.util.Set;
import java.util.concurrent.ForkJoinPool;
import java.util.function.Function;
import java.util.stream.Collectors;
import javax.swing.DefaultComboBoxModel;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
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
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableModel;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.greenrobot.eventbus.EventBus;
import org.greenrobot.eventbus.Subscribe;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import umich.msfragger.gui.ModificationsTableModel;
import umich.msfragger.gui.api.SearchTypeProp;
import umich.msfragger.gui.renderers.TableCellDoubleRenderer;
import umich.msfragger.messages.MessageMsfraggerParamsUpdate;
import umich.msfragger.messages.MessagePrecursorSelectionMode;
import umich.msfragger.messages.MessageRun;
import umich.msfragger.messages.MessageSaveCache;
import umich.msfragger.messages.MessageSearchType;
import umich.msfragger.messages.MessageValidityFragger;
import umich.msfragger.messages.MessageValidityMassCalibration;
import umich.msfragger.messages.MessageValidityMsadjuster;
import umich.msfragger.params.Props.Prop;
import umich.msfragger.params.ThisAppProps;
import umich.msfragger.params.dbslice.DbSlice;
import umich.msfragger.params.enums.CleavageType;
import umich.msfragger.params.enums.FraggerOutputType;
import umich.msfragger.params.enums.FraggerPrecursorMassMode;
import umich.msfragger.params.enums.IntensityTransform;
import umich.msfragger.params.enums.MassTolUnits;
import umich.msfragger.params.enums.RemovePrecursorPeak;
import umich.msfragger.util.CacheUtils;
import umich.msfragger.util.PropertiesUtils;
import umich.msfragger.util.StringUtils;
import umich.msfragger.util.SwingUtils;
import umich.msfragger.util.swing.FormEntry;

/**
 * @author Dmitry Avtonomov
 */
public class FraggerMigPanel extends JPanel {

  public static final String PROP_FILECHOOSER_LAST_PATH = "msfragger.filechooser.path";
  private static final Logger log = LoggerFactory.getLogger(FraggerMigPanel.class);
  public static final String CACHE_FORM = "msfragger-form" + ThisAppProps.TEMP_FILE_EXT;
  public static final String CACHE_PROPS = "msfragger-props" + ThisAppProps.TEMP_FILE_EXT;
  private static final String[] TABLE_VAR_MODS_COL_NAMES = {"Enabled", "Site (editable)",
      "Mass Delta (editable)"};
  private static final String[] TABLE_FIX_MODS_COL_NAMES = {"Enabled", "Site",
      "Mass Delta (editable)"};
  private static final String PROP_misc_adjust_precurosr_mass = "misc.adjust-precursor-mass";
  private static final String PROP_misc_slice_db = "misc.slice-db";
  private static final String PROP_misc_ram = "misc.ram";
  private static final String PROP_misc_fragger_remove_precursor_range_lo = "misc.fragger.remove-precursor-range-lo";
  private static final String PROP_misc_fragger_remove_precursor_range_hi = "misc.fragger.remove-precursor-range-hi";
  private static final String PROP_misc_fragger_digest_mass_lo = "misc.fragger.digest-mass-lo";
  private static final String PROP_misc_fragger_digest_mass_hi = "misc.fragger.digest-mass-hi";
  private static final String PROP_misc_fragger_clear_mz_lo = "misc.fragger.clear-mz-lo";
  private static final String PROP_misc_fragger_clear_mz_hi = "misc.fragger.clear-mz-hi";
  private static final String PROP_misc_fragger_precursor_charge_lo = "misc.fragger.precursor-charge-lo";
  private static final String PROP_misc_fragger_precursor_charge_hi = "misc.fragger.precursor-charge-hi";
  private static final String PROP_misc_fragger_enzyme_dropdown = "misc.fragger.enzyme-dropdown";
  private static final Set<String> PROPS_MISC_NAMES;
  private static final Map<String, Function<String, String>> CONVERT_TO_FILE;
  private static final Map<String, Function<String, String>> CONVERT_TO_GUI;
  private static final String[] CALIBRATE_LABELS = {"Off", "On", "On and find optimal parameters"};
  private static final List<MsfraggerEnzyme> ENZYMES = new EnzymeProvider().get();
  //public static FileNameExtensionFilter fnExtFilter = new FileNameExtensionFilter("LCMS files (mzML/mzXML/mgf/raw/d)", "mzml", "mzxml", "mgf", "raw", "d");
  private static String[] PROPS_MISC = {
      PROP_misc_adjust_precurosr_mass,
      PROP_misc_slice_db,
      PROP_misc_ram,
      PROP_misc_fragger_digest_mass_lo,
      PROP_misc_fragger_digest_mass_hi,
      PROP_misc_fragger_remove_precursor_range_lo,
      PROP_misc_fragger_remove_precursor_range_hi,
      PROP_misc_fragger_clear_mz_lo,
      PROP_misc_fragger_clear_mz_hi,
      PROP_misc_fragger_precursor_charge_lo,
      PROP_misc_fragger_precursor_charge_hi,
  };

  static {
    PROPS_MISC_NAMES = new HashSet<>(Arrays.asList(PROPS_MISC));
    CONVERT_TO_FILE = new HashMap<>();
    CONVERT_TO_GUI = new HashMap<>();

    CONVERT_TO_FILE.put(MsfraggerParams.PROP_precursor_mass_units, s -> Integer.toString(MassTolUnits.valueOf(s).valueInParamsFile()));
    CONVERT_TO_FILE.put(MsfraggerParams.PROP_fragment_mass_units, s -> Integer.toString(MassTolUnits.valueOf(s).valueInParamsFile()));
    CONVERT_TO_FILE.put(MsfraggerParams.PROP_precursor_true_units, s -> Integer.toString(MassTolUnits.valueOf(s).valueInParamsFile()));
    CONVERT_TO_FILE.put(MsfraggerParams.PROP_calibrate_mass, s -> Integer.toString(Arrays.asList(CALIBRATE_LABELS).indexOf(s)));
    CONVERT_TO_FILE.put(MsfraggerParams.PROP_num_enzyme_termini, s -> Integer.toString(CleavageType.valueOf(s).valueInParamsFile()));
    CONVERT_TO_FILE.put(MsfraggerParams.PROP_remove_precursor_peak, s -> Integer.toString(RemovePrecursorPeak.get(s)));
    CONVERT_TO_FILE.put(MsfraggerParams.PROP_intensity_transform, s -> Integer.toString(IntensityTransform.get(s)));
    CONVERT_TO_FILE.put(MsfraggerParams.PROP_localize_delta_mass, s -> Integer.toString(Boolean.valueOf(s) ? 1 : 0));
    CONVERT_TO_FILE.put(MsfraggerParams.PROP_clip_nTerm_M, s -> Integer.toString(Boolean.valueOf(s) ? 1 : 0));
    CONVERT_TO_FILE.put(MsfraggerParams.PROP_allow_multiple_variable_mods_on_residue, s -> Integer.toString(Boolean.valueOf(s) ? 1 : 0));
    CONVERT_TO_FILE.put(MsfraggerParams.PROP_override_charge, s -> Integer.toString(Boolean.valueOf(s) ? 1 : 0));
    CONVERT_TO_FILE.put(MsfraggerParams.PROP_output_format, s -> FraggerOutputType.valueOf(s).valueInParamsFile());
    CONVERT_TO_FILE.put(MsfraggerParams.PROP_report_alternative_proteins, s -> Integer.toString(Boolean.valueOf(s) ? 1 : 0));
    CONVERT_TO_FILE.put(MsfraggerParams.PROP_fragment_ion_series, ionStr -> ionStr.trim().replaceAll("[\\s,;]+",","));
    CONVERT_TO_FILE.put(MsfraggerParams.PROP_ion_series_definitions, defStr -> defStr.trim().replaceAll("\\s*[,;]+\\s*",", "));

    CONVERT_TO_GUI.put(MsfraggerParams.PROP_precursor_mass_units, s -> MassTolUnits.fromParamsFileRepresentation(s).name());
    CONVERT_TO_GUI.put(MsfraggerParams.PROP_fragment_mass_units, s -> MassTolUnits.fromParamsFileRepresentation(s).name());
    CONVERT_TO_GUI.put(MsfraggerParams.PROP_precursor_true_units, s -> MassTolUnits.fromParamsFileRepresentation(s).name());
    CONVERT_TO_GUI.put(MsfraggerParams.PROP_calibrate_mass, s -> CALIBRATE_LABELS[Integer.parseInt(s)]);
    CONVERT_TO_GUI.put(MsfraggerParams.PROP_num_enzyme_termini, s -> CleavageType.fromValueInParamsFile(s).name());
    CONVERT_TO_GUI.put(MsfraggerParams.PROP_remove_precursor_peak, s -> RemovePrecursorPeak.get(Integer.parseInt(s)));
    CONVERT_TO_GUI.put(MsfraggerParams.PROP_intensity_transform, s -> IntensityTransform.get(Integer.parseInt(s)));
    CONVERT_TO_GUI.put(MsfraggerParams.PROP_localize_delta_mass, s -> Boolean.toString(Integer.parseInt(s) > 0));
    CONVERT_TO_GUI.put(MsfraggerParams.PROP_clip_nTerm_M, s -> Boolean.toString(Integer.parseInt(s) > 0));
    CONVERT_TO_GUI.put(MsfraggerParams.PROP_allow_multiple_variable_mods_on_residue, s -> Boolean.toString(Integer.parseInt(s) > 0));
    CONVERT_TO_GUI.put(MsfraggerParams.PROP_override_charge, s -> Boolean.toString(Integer.parseInt(s) > 0));
    CONVERT_TO_GUI.put(MsfraggerParams.PROP_output_format, s -> FraggerOutputType.fromValueInParamsFile(s).name());
    CONVERT_TO_GUI.put(MsfraggerParams.PROP_report_alternative_proteins, s -> Boolean.toString(Integer.parseInt(s) > 0));
  }

  private ImageIcon icon;
  private JCheckBox checkRun;
  private JScrollPane scroll;
  private JPanel pContent;
  private ModificationsTableModel tableModelVarMods;
  private javax.swing.JTable tableVarMods;
  private ModificationsTableModel tableModelFixMods;
  private javax.swing.JTable tableFixMods;
  private UiSpinnerInt uiSpinnerRam;
  private UiSpinnerInt uiSpinnerThreads;
  private UiCombo uiComboMassCalibrate;
  private UiCombo uiComboOutputType;
  private UiCombo uiComboMassMode;
  private UiSpinnerInt uiSpinnerDbslice;
  private UiCheck uiCheckShiftedIons;
  private UiText uiTextCustomIonSeries;
  private JLabel labelCustomIonSeries;
  private Map<Component, Boolean> enablementMapping = new HashMap<>();
  private UiCombo uiComboEnzymes;
  private UiText uiTextCuts;
  private UiText uiTextNocuts;
  private UiText uiTextEnzymeName;
  private UiCombo uiComboCleavage;

  public FraggerMigPanel() {
    initMore();
    initPostCreation();
    // register on the bus only after all the components have been created to avoid NPEs
    EventBus.getDefault().register(this);
  }

  private void onClickDefautlsNonspecific(ActionEvent e) {
    if (loadDefaults(SearchTypeProp.nonspecific, true)) {
      postSearchTypeUpdate(SearchTypeProp.nonspecific, true);
    }
  }

  private void onClickDefaultsOpen(ActionEvent e) {
    if (loadDefaults(SearchTypeProp.open, true)) {
      postSearchTypeUpdate(SearchTypeProp.open, true);
    }
  }

  private void onClickDefaultsClosed(ActionEvent e) {
    if (loadDefaults(SearchTypeProp.closed, true)) {
      postSearchTypeUpdate(SearchTypeProp.closed, true);
    }
  }

  private static void onChangeMassMode(ItemEvent e) {
    if (e.getStateChange() == ItemEvent.SELECTED) {

      final Object item = e.getItem();
      if (!(item instanceof String)) {
        return;
      }
      try {
        FraggerPrecursorMassMode mode = FraggerPrecursorMassMode.valueOf((String) item);
        EventBus.getDefault().post(new MessagePrecursorSelectionMode(mode));

      } catch (IllegalArgumentException ex) {
        log.debug("Value [{}] not in FraggerPrecursorMassMode enum", item);
      }
    }
  }

  private void initPostCreation() {
    ForkJoinPool.commonPool().execute(this::cacheLoad);

    // TODO: ACHTUNG: temporary fix, disabling "Define custom ion series field"
    // Remove when custom ion series work properly in msfragger
    updateEnabledStatus(uiTextCustomIonSeries, false);
    updateEnabledStatus(labelCustomIonSeries, false);
  }

  private void initMore() {
    icon = new ImageIcon(
        getClass().getResource("/umich/msfragger/gui/icons/bolt-16.png"));

    this.setLayout(new BorderLayout());

    DecimalFormat df1 = new DecimalFormat("0.#");
    DecimalFormat df2 = new DecimalFormat("0.##");
    DecimalFormat df3 = new DecimalFormat("0.###");
    DecimalFormat df4 = new DecimalFormat("0.####");

    // Top panel with checkbox, buttons and RAM+Threads spinners
    {
      JPanel pTop = new JPanel(new MigLayout(new LC()));
      checkRun = new JCheckBox("Run MSFragger", true);
      checkRun.addActionListener(e -> {
        final boolean isSelected = checkRun.isSelected();
        updateEnabledStatus(pContent, isSelected);
      });
      JButton btnDefaultsClosed = new JButton("Closed Search");
      btnDefaultsClosed.addActionListener(this::onClickDefaultsClosed);
      JButton btnDefaultsOpen = new JButton("Open Search");
      btnDefaultsOpen.addActionListener(this::onClickDefaultsOpen);
      JButton btnDefaultsNonspecific = new JButton("Non-specific Search");
      btnDefaultsNonspecific.addActionListener(this::onClickDefautlsNonspecific);

      pTop.add(checkRun);
      pTop.add(new JLabel("Load defaults:"), new CC().gapLeft("15px"));
      pTop.add(btnDefaultsClosed, new CC().gapLeft("1px"));
      pTop.add(btnDefaultsOpen, new CC().gapLeft("1px"));
      pTop.add(btnDefaultsNonspecific, new CC().gapLeft("1px").wrap());

      JButton save = new JButton("Save Options");
      save.addActionListener(this::onClickSave);
      JButton load = new JButton("Load Options");
      load.addActionListener(this::onClickLoad);

      uiSpinnerRam = new UiSpinnerInt(0, 0, 1024, 1, 3);
      FormEntry feRam = new FormEntry(PROP_misc_ram, "RAM (GB)", uiSpinnerRam);
      uiSpinnerThreads = new UiSpinnerInt(Runtime.getRuntime().availableProcessors() - 1, 0, 128, 1);
      FormEntry feThreads = new FormEntry(MsfraggerParams.PROP_num_threads, "Threads",
          uiSpinnerThreads);

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
      FormEntry fePrecTolUnits = new FormEntry(MsfraggerParams.PROP_precursor_mass_units, "Precursor mass tolerance",
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
      FormEntry feDeisotope = new FormEntry(MsfraggerParams.PROP_deisotope,
          "Deisotope", new UiSpinnerInt(0, 0, 1, 1, 4),
          "<html>0 = deisotoping off<br/>\n"
              + "1 = deisotoping on");

      pPeakMatch.add(fePrecTolUnits.label(), new CC().alignX("right"));
      pPeakMatch.add(fePrecTolUnits.comp, new CC().split(4));
      pPeakMatch.add(feSpinnerPrecTolLo.comp);
      pPeakMatch.add(new JLabel("-"), new CC());
      pPeakMatch.add(feSpinnerPrecTolHi.comp, new CC());
      pPeakMatch.add(feDeisotope.label(), new CC().alignX("right"));
      pPeakMatch.add(feDeisotope.comp, new CC().wrap());

      // fragment mass tolerance
      FormEntry feFragTolUnits = new FormEntry(MsfraggerParams.PROP_fragment_mass_units,
          "Fragment mass tolerance", UiUtils.createUiCombo(MassTolUnits.values()));
      UiSpinnerDouble uiSpinnerFragTol = new UiSpinnerDouble(10, 0, 10000, 1,
          new DecimalFormat("0.###"));
      uiSpinnerFragTol.setColumns(4);
      FormEntry feFragTol = new FormEntry(MsfraggerParams.PROP_fragment_mass_tolerance, "not-shown",
          uiSpinnerFragTol);
      pPeakMatch.add(feFragTolUnits.label(), new CC().alignX("right"));
      pPeakMatch.add(feFragTolUnits.comp, new CC().split(2));
      pPeakMatch.add(feFragTol.comp, new CC().wrap());

      // mass calibrate
      uiComboMassCalibrate = UiUtils.createUiCombo(CALIBRATE_LABELS);
      String minFraggerVer = MsfraggerProps.getProperties().getProperty(MsfraggerProps.PROP_MIN_VERSION_FRAGGER_MASS_CALIBRATE, "201904");
      FormEntry feCalibrate = new FormEntry(MsfraggerParams.PROP_calibrate_mass, "<html>Calibrate masses",
          uiComboMassCalibrate, String.format("<html>Requires MSFragger %s+.", minFraggerVer));
      pPeakMatch.add(feCalibrate.label(), new CC().alignX("right"));
      pPeakMatch.add(feCalibrate.comp, new CC());

      UiText uiTextIsoErr = new UiText();
      uiTextIsoErr.setDocument(DocumentFilters.getFilter("[^\\d/-]+"));
      uiTextIsoErr.setText("-1/0/1/2");
      uiTextIsoErr.setColumns(10);
      FormEntry feIsotopeError = new FormEntry(MsfraggerParams.PROP_isotope_error, "Isotope error",
          uiTextIsoErr,
          "<html>String of the form -1/0/1/2 indicating which isotopic<br/>peak selection errors MSFragger will try to correct.");

      pPeakMatch.add(feIsotopeError.label(), new CC().alignX("right"));
      pPeakMatch.add(feIsotopeError.comp, new CC().wrap());

      // Digest panel
      JPanel pDigest = new JPanel(new MigLayout(new LC()));
      pDigest.setBorder(new TitledBorder("Protein Digestion"));

      uiComboEnzymes = UiUtils
          .createUiCombo(ENZYMES.stream().map(msfe -> msfe.name)
              //.filter(name -> !"custom".equals(name))
              .collect(Collectors.toList()));
      Optional<MsfraggerEnzyme> trypsin = ENZYMES.stream()
          .filter(e -> e.name.toLowerCase().startsWith("trypsin"))
          .min(Comparator.comparing(o -> o.name));
      trypsin.ifPresent(msfraggerEnzyme -> uiComboEnzymes.setSelectedItem(msfraggerEnzyme.name));
      FormEntry feEnzymeList = new FormEntry(PROP_misc_fragger_enzyme_dropdown, "Load rules",
          uiComboEnzymes, "<html>Load one of default definitions of enzyme cleavage rules.<br/>\n"
          + "You can still edit the name and rules manually after loading.");
      uiComboEnzymes.addItemListener(event -> {
        if (event.getStateChange() == ItemEvent.SELECTED) {
          Object item = event.getItem();
          log.debug("User selected enzyme: {}, of class {}", item, item.getClass());
          if (!(item instanceof String)) {
            throw new IllegalStateException("Ui Combo Boxes should just contain strings");
          }
          String name = (String) item;
          MsfraggerEnzyme enzyme = ENZYMES.stream()
              .filter(msfe -> msfe.name.equals(name)).findFirst()
              .orElseThrow(() -> new IllegalStateException(
                  "Enzymes list should have contained the name from dropdown"));
          uiTextEnzymeName.setText(enzyme.name);
          uiTextCuts.setText(enzyme.cut);
          uiTextNocuts.setText(enzyme.nocuts);
          uiComboCleavage.setSelectedItem("nonspecific".equals(item) ? CleavageType.NON_SPECIFIC.name() : CleavageType.ENZYMATIC.name());
        }
      });


      FocusAdapter enzymeSpecFocusListener = new FocusAdapter() {
        @Override
        public void focusLost(FocusEvent evt) {
          super.focusLost(evt);
          final String cuts = StringUtils.sortedChars(uiTextCuts.getNonGhostText());
          final String nocuts = StringUtils.sortedChars(uiTextNocuts.getNonGhostText());
          List<MsfraggerEnzyme> enzymes = ENZYMES.stream()
              .map(e -> new MsfraggerEnzyme(e.name, StringUtils.sortedChars(e.cut),
                  StringUtils.sortedChars(e.nocuts)))
              .collect(Collectors.toList());
          List<String> matching = enzymes.stream()
              .filter(e -> e.cut.equals(cuts) && e.nocuts.equals(nocuts))
              .map(e -> e.name).collect(Collectors.toList());
          log.warn("Found matching enzymes: {}", matching);
          if (matching.contains("nonspecific")) {
            trySelectEnzymeDropdown("nonspecific");
          } else if (!matching.isEmpty()) {
            trySelectEnzymeDropdown(matching.get(0));
          } else {
            trySelectEnzymeDropdown("custom");
          }
//          if (matching.size() > 0) {
//            if (cuts.isEmpty() && nocuts.isEmpty()) {
//              uiTextEnzymeName.setText("nonspecific");
//            } else {
//              uiTextEnzymeName.setText(matching.get(0).name);
//            }
//          } else {
//            uiTextEnzymeName.setText("custom");
//            try {
//              // need to either disable the action listener on dropdown or just restore the
//              // user-input values
//              String saveCuts = uiTextCuts.getNonGhostText();
//              String saveNouts = uiTextNocuts.getNonGhostText();
//              uiComboEnzymes.setSelectedItem("custom");
//              uiTextCuts.setText(saveCuts);
//              uiTextNocuts.setText(saveNouts);
//            } catch (Exception ignored) {}
//          }
        }
      };

      uiTextEnzymeName = new UiText();
      FormEntry feEnzymeName = new FormEntry(MsfraggerParams.PROP_search_enzyme_name, "Enzyme name",
          uiTextEnzymeName);
      uiTextCuts = UiUtils.uiTextBuilder().cols(6).filter("[^A-Z]").text("KR").create();
      uiTextCuts.addFocusListener(enzymeSpecFocusListener);
      FormEntry feCuts = new FormEntry(MsfraggerParams.PROP_search_enzyme_cutafter, "Cut after",
          uiTextCuts, "Capital letters for amino acids after which the enzyme cuts.");
      uiTextNocuts = UiUtils.uiTextBuilder().cols(6).filter("[^A-Z]").text("P").create();
      uiTextNocuts.addFocusListener(enzymeSpecFocusListener);
      FormEntry feNocuts = new FormEntry(MsfraggerParams.PROP_search_enzyme_butnotafter,
          "But not before", uiTextNocuts, "Amino acids before which the enzyme won't cut.");
      pDigest.add(feEnzymeList.label(), new CC().span(2).split(2).alignX("right"));
      pDigest.add(feEnzymeList.comp, new CC());
      pDigest.add(feEnzymeName.label(), new CC().alignX("right"));
      pDigest.add(feEnzymeName.comp, new CC().minWidth("120px").split().spanX());
      pDigest.add(feCuts.label(), new CC().gapLeft("5px"));
      pDigest.add(feCuts.comp);//, new CC().minWidth("45px"));
      pDigest.add(feNocuts.label());//, new CC().split(2).spanX().gapLeft("5px"));
      pDigest.add(feNocuts.comp, new CC().wrap());

      List<String> cleavageTypeNames = Arrays.stream(CleavageType.values()).map(Enum::name)
          .collect(Collectors.toList());
      uiComboCleavage = UiUtils.createUiCombo(cleavageTypeNames);
      FormEntry feCleavageType = new FormEntry(MsfraggerParams.PROP_num_enzyme_termini, "Cleavage",
          uiComboCleavage);
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
      FormEntry fePepMassLo = new FormEntry(PROP_misc_fragger_digest_mass_lo, "Peptide mass range",
          uiSpinnerDigestMassLo);
      UiSpinnerDouble uiSpinnerDigestMassHi = new UiSpinnerDouble(5000, 0, 50000, 100,
          new DecimalFormat("0.#"));
      uiSpinnerDigestMassHi.setColumns(6);
      FormEntry fePepMassHi = new FormEntry(PROP_misc_fragger_digest_mass_hi, "not-shown",
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
      uiSpinnerDbslice = new UiSpinnerInt(1, 1, 99, 1, 2);
      FormEntry feSliceDb = new FormEntry(PROP_misc_slice_db, "<html>Split database", uiSpinnerDbslice,
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
          "not-shown", new UiCheck("Multiple var mods on residue", null));
      tableVarMods = new JTable();
      tableVarMods.setModel(getDefaultVarModTableModel());
      tableVarMods.setToolTipText(
          "<html>Variable Modifications.<br/>\nValues:<br/>\n<ul>\n<li>A-Z amino acid codes</li>\n<li>*​ ​is​ ​used​ ​to​ ​represent​ ​any​ ​amino​ ​acid</li>\n<li>^​ ​is​ ​used​ ​to​ ​represent​ ​a​ ​terminus</li>\n<li>[​ ​is​ ​a​ ​modifier​ ​for​ ​protein​ ​N-terminal</li>\n<li>]​ ​is​ ​a​ ​modifier​ ​for​ ​protein​ ​C-terminal</li>\n<li>n​ ​is​ ​a​ ​modifier​ ​for​ ​peptide​ ​N-terminal</li>\n<li>c​ ​is​ ​a​ ​modifier​ ​for​ ​peptide​ ​C-terminal</li>\n</ul>\nSyntax​ ​Examples:\n<ul>\n<li>15.9949​ ​M​ ​(for​ ​oxidation​ ​on​ ​methionine)</li>\n<li>79.66331​ ​STY​ ​(for​ ​phosphorylation)</li>\n<li>-17.0265​ ​nQnC​ ​(for​ ​pyro-Glu​ ​or​ ​loss​ ​of​ ​ammonia​ ​at peptide​ ​N-terminal)</li>\n</ul>\nExample​ ​(M​ ​oxidation​ ​and​ ​N-terminal​ ​acetylation):\n<ul>\n<li>variable_mod_01​ ​=​ ​15.9949​ ​M</li>\n<li>variable_mod_02​ ​=​ ​42.0106​ ​[^</li>\n</ul>");
      tableVarMods.setDefaultRenderer(Double.class, new TableCellDoubleRenderer());
      tableVarMods.setFillsViewportHeight(true);
      SwingUtilities.invokeLater(() -> {
        setJTableColSize(tableVarMods, 0, 20, 150, 50);
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
        setJTableColSize(tableFixMods, 0, 20, 150, 50);
      });
      JScrollPane tableScrollFixMods = new JScrollPane(tableFixMods,
          JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
      pFixmods.add(tableScrollFixMods,
          new CC().minHeight("100px").maxHeight("200px").growX().spanX().wrap());

      pMods.add(pVarmods, new CC().wrap().growX());
      pMods.add(pFixmods, new CC().wrap().growX());

      // mass offsets text field separately
      String tooltipMassOffsets = "<html>Creates multiple precursor tolerance windows with<br>\n"
          + "specified mass offsets. These values are multiplexed<br>\n"
          + "with the isotope error option.<br><br>\n\n"
          + "For example, value \"0/79.966\" can be used<br>\n"
          + "as a restricted open search that looks for unmodified<br>\n"
          + "and phosphorylated peptides (on any residue).<br><br>\n\n"
          + "Setting isotope_error to 0/1/2 in combination<br>\n"
          + "with this example will create search windows around<br>\n"
          + "(0,1,2,79.966, 80.966, 81.966).";
      FormEntry feMassOffsets = new FormEntry(MsfraggerParams.PROP_mass_offsets,
          "User defined variable mass shifts (on any aminoacid)",
          UiUtils.uiTextBuilder().filter("[^-\\(\\)\\./,\\d ]").text("0").create(),
          tooltipMassOffsets);
      pMods.add(feMassOffsets.label(), new CC().split(2));
      pMods.add(feMassOffsets.comp, new CC().alignX("left").growX().wrap());

      pContent.add(pMods, new CC().wrap().growX());
    }

    // Panel with all the advanced options
    {
      JPanel pAdvanced = new JPanel(new MigLayout(new LC()));
      pAdvanced.setBorder(new TitledBorder("Advanced Options"));

      CC alignRight = new CC().alignX("right");
      CC wrap = new CC().wrap();

      JPanel pOpenSearch = new JPanel(new MigLayout(new LC()));
      {

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

        uiCheckShiftedIons = new UiCheck("<html>Localize delta mass", null);
        FormEntry feShiftedIonsCheck = new FormEntry(MsfraggerParams.PROP_localize_delta_mass, "not-shown",
            uiCheckShiftedIons, "<html>Shifted ion series are the same as regular b/y ions,<br/>"
            + "but with the addition of the mass shift of the precursor.<br/>"
            + "Regular ion series will still be used.<br/>"
            + "This option is </b>incompatible</b> with database splitting.");
        UiText uiTextShiftedIonsExclusion = new UiText();
        uiTextShiftedIonsExclusion.setDocument(DocumentFilters.getFilter("[A-Za-z]"));
        uiTextShiftedIonsExclusion.setText("(-1.5,3.5)");
        FormEntry feShiftedIonsExclusion = new FormEntry(
            MsfraggerParams.PROP_delta_mass_exclude_ranges, "Delta mass exclude ranges",
            uiTextShiftedIonsExclusion, "<html>Ranges expressed like: (-1.5,3.5)");
        pOpenSearch.add(feShiftedIonsCheck.comp, new CC().alignX("right"));
        pOpenSearch.add(feShiftedIonsExclusion.label(), new CC().split(2).spanX().gapLeft("25px"));
        pOpenSearch.add(feShiftedIonsExclusion.comp, new CC().growX());

        uiCheckShiftedIons.addActionListener(e -> {
          final boolean selected = uiCheckShiftedIons.isSelected();
          final int dbSlicing = uiSpinnerDbslice.getActualValue();
          if (selected && dbSlicing > 1) {
            JOptionPane.showMessageDialog(FraggerMigPanel.this,
                "<html>This option is incompatible with DB Splitting.<br/>"
                    + "Please either turn it off, or turn off DB Splitting by setting<br/>"
                    + "it to 1.", "Incompatible options", JOptionPane.WARNING_MESSAGE);
          }
        });

        uiSpinnerDbslice.addChangeListener(e -> {
          final boolean selected = uiCheckShiftedIons.isSelected();
          final int dbSlicing = uiSpinnerDbslice.getActualValue();
          if (selected && dbSlicing > 1) {
            JOptionPane.showMessageDialog(FraggerMigPanel.this,
                "<html><code>DB Slicing<code> is incompatible with <code>Localize delta mass</code> option.<br/>"
                    + "Please either set <code>DB Slicing<code> to 1, or uncheck <code>Localize delta mass</code> checkbox<br/>"
                    + "at the end of this form.",
                "Incompatible options", JOptionPane.WARNING_MESSAGE);
          }
        });

      }

      JPanel pSpectral = new JPanel(new MigLayout(new LC()));
      {
        pSpectral.setBorder(new TitledBorder("Spectral Processing"));

        FormEntry feMinPeaks = new FormEntry(MsfraggerParams.PROP_minimum_peaks, "Min peaks",
            new UiSpinnerInt(15, 0, 1000, 1, 4));
        FormEntry feUseTopN = new FormEntry(MsfraggerParams.PROP_use_topN_peaks, "Use top N peaks",
            new UiSpinnerInt(100, 0, 1000000, 10, 4));
        UiSpinnerDouble spinnerMinRatio = new UiSpinnerDouble(0.01, 0, Double.MAX_VALUE, 0.1, 2,
            new DecimalFormat("0.00"));
        spinnerMinRatio.setColumns(4);
        FormEntry feMinRatio = new FormEntry(MsfraggerParams.PROP_minimum_ratio, "Min ratio",
            spinnerMinRatio);
        FormEntry feClearRangeMzLo = new FormEntry(PROP_misc_fragger_clear_mz_lo, "Clear m/z range",
            new UiSpinnerInt(0, 0, 100000, 10, 4));
        FormEntry feClearRangeMzHi = new FormEntry(PROP_misc_fragger_clear_mz_hi, "not-shown",
            new UiSpinnerInt(0, 0, 100000, 10, 4));

        uiComboMassMode = new UiCombo(); // UiUtils.createUiCombo(FraggerPrecursorMassMode.values());
        uiComboMassMode.setModel(new DefaultComboBoxModel<>(new String[] {
            FraggerPrecursorMassMode.selected.name(),
            FraggerPrecursorMassMode.isolated.name(),
            FraggerPrecursorMassMode.recalculated.name(),
        }));
        uiComboMassMode.addItemListener(FraggerMigPanel::onChangeMassMode);
        FormEntry fePrecursorMassMode = new FormEntry(MsfraggerParams.PROP_precursor_mass_mode,
            "Precursor mass mode", uiComboMassMode,
            "<html>Determines which entry from mzML files will be<br/>"
                + "used as the precursor's mass - 'Selected' or 'Isolated' ion.<br/>"
                + "'Recalculated' option runs a separate MSAdjuster tool to<br/>"
                + "perform mono-isotopic mass correction");

        FormEntry feRemovePrecPeak = new FormEntry(MsfraggerParams.PROP_remove_precursor_peak,
            "Remove precursor peak", UiUtils.createUiCombo(RemovePrecursorPeak.getNames()));
        FormEntry fePrecRemoveRangeLo = new FormEntry(PROP_misc_fragger_remove_precursor_range_lo, "removal m/z range",
            UiSpinnerDouble.builder(-1.5, -1000.0, 1000.0, 0.1).setNumCols(5).setFormat(df1).create());
        FormEntry fePrecRemoveRangeHi = new FormEntry(PROP_misc_fragger_remove_precursor_range_hi, "not-shown",
            UiSpinnerDouble.builder(+1.5, -1000.0, 1000.0, 0.1).setNumCols(5).setFormat(df1).create());
        FormEntry feIntensityTransform = new FormEntry(MsfraggerParams.PROP_intensity_transform,
            "Intensity transform", UiUtils.createUiCombo(IntensityTransform.getNames()));

        pSpectral.add(fePrecursorMassMode.label(), new CC().alignX("right"));
        pSpectral.add(fePrecursorMassMode.comp, new CC().wrap());

        pSpectral.add(feMinPeaks.label(), alignRight);
        pSpectral.add(feMinPeaks.comp, new CC().split(5).spanX());
        pSpectral.add(feUseTopN.label(), new CC().gapBefore("20px"));
        pSpectral.add(feUseTopN.comp, new CC());
        pSpectral.add(feMinRatio.label(), new CC().gapBefore("20px"));
        pSpectral.add(feMinRatio.comp, wrap);

        pSpectral.add(feClearRangeMzLo.label(), alignRight);
        pSpectral.add(feClearRangeMzLo.comp, new CC().split(3).spanX());
        pSpectral.add(new JLabel("-"));
        pSpectral.add(feClearRangeMzHi.comp, new CC().wrap());

        pSpectral.add(feRemovePrecPeak.label(), alignRight);
        pSpectral.add(feRemovePrecPeak.comp, new CC().split(5).spanX());
        pSpectral.add(fePrecRemoveRangeLo.label(), new CC());
        pSpectral.add(fePrecRemoveRangeLo.comp, new CC());
        pSpectral.add(new JLabel("-"));
        pSpectral.add(fePrecRemoveRangeHi.comp, new CC().wrap());
        pSpectral.add(feIntensityTransform.label(), alignRight);
        pSpectral.add(feIntensityTransform.comp, new CC().wrap());
      }

      // Advanced peak matching panel
      JPanel pPeakMatch = new JPanel(new MigLayout(new LC()));
      {
        pPeakMatch.setBorder(new TitledBorder("Peak Matching and Output Advanced Options"));

        FormEntry feMinFragsModeling = new FormEntry(MsfraggerParams.PROP_min_fragments_modelling,
            "Min frags modeling", new UiSpinnerInt(2, 0, 1000, 1, 4));
        FormEntry feMinMatchedFrags = new FormEntry(MsfraggerParams.PROP_min_matched_fragments,
            "Min matched frags", new UiSpinnerInt(4, 0, 1000, 1, 4));

        FormEntry feIonSeries = new FormEntry(MsfraggerParams.PROP_fragment_ion_series,
            "Fragment ion series", new UiText(10), ""
            + "<html>Which peptide ion series to check against.<br/>\n"
            + "<b>Use spaces, commas or semicolons as delimiters</b>, e.g. \"b,y\"<br/>\n"
            + "This mostly depends on fragmentation method.<br/>\n"
            + "Typically \"b,y\" are used for CID and \"c,z\" for ECD.<br/>\n"
            + "MSFragger can generate \"a,b,c,x,y,z\" ion series by default,<br/>\n"
            + "but <b>you can define your own in 'Define custom ion series' field</b>.<br/>\n"
            + "If you define custom series, you will need to include the name you<br/>\n"
            + "gave it here.");
        uiTextCustomIonSeries = new UiText(10);
        String tooltipCustomIonSeriesDisabled = "This feature is currently disabled";
        String tooltipCustomIonSeriesOriginal = "<html>Custom ion series allow specification of arbitrary mass gains/losses<br/>\n"
            + "for N- and C-terminal ions. Separate multiple definitions by commas or semicolons.<br/>\n"
            + "<b>Format:</b> name terminus mass-delta<br/>\n"
            + "Example definition string:<br/>\n"
            + "b* N -17.026548; b0 N -18.010565<br/>\n"
            + "This would define two new ion types named <i>b*</i> and <i>b0</i>,<br/>\n"
            + "you can name them whatever you fancy. <i>b*</i> is the equivalent of an<br/>\n"
            + "N terminal b-ion with ammonia loss, <i>b0</i> is the same with water loss.<br/>\n";
        FormEntry feCustomSeries = new FormEntry(MsfraggerParams.PROP_ion_series_definitions,
            "Define custom ion series", uiTextCustomIonSeries, tooltipCustomIonSeriesDisabled);
        labelCustomIonSeries = feCustomSeries.label();

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


        uiComboOutputType = UiUtils.createUiCombo(FraggerOutputType.values());
        FormEntry feOutputType = new FormEntry(MsfraggerParams.PROP_output_format, "Output format",
            uiComboOutputType,
            "<html>How the search results are to be reported.<br>\n" +
                "Downstream tools only support PepXML format.<br><br>\n" +
                "Only use TSV (tab delimited file) if you want to process <br>\n" +
                "search resutls yourself for easier import into other software.<br>");

        String tooltipPrecursorCHarge =
            "<html>Assume range of potential precursor charge states.<br>\n" +
                "Only relevant when override_charge is set to 1.<br>\n" +
                "Specified as space separated range of integers.<br>";
        FormEntry fePrecursorChargeLo = new FormEntry(PROP_misc_fragger_precursor_charge_lo,
            "",
            new UiSpinnerInt(1, 0, 30, 1, 2), tooltipPrecursorCHarge);
        FormEntry fePrecursorChargeHi = new FormEntry(PROP_misc_fragger_precursor_charge_hi,
            "not-shown",
            new UiSpinnerInt(4, 0, 30, 1, 2), tooltipPrecursorCHarge);
        FormEntry feOverrideCharge = new FormEntry(MsfraggerParams.PROP_override_charge,
            "not-shown", new UiCheck("Override charge with precursor charge", null),
            "<html>Ignores precursor charge and uses charge state<br>\n" +
                "specified in precursor_charge range.<br>");
        FormEntry feReportAltProts = new FormEntry(MsfraggerParams.PROP_report_alternative_proteins,
            "not-shown", new UiCheck("Report alternative proteins", null, false));

        pPeakMatch.add(feMinFragsModeling.label(), alignRight);
        pPeakMatch.add(feMinFragsModeling.comp);
        pPeakMatch.add(feMinMatchedFrags.label(), alignRight);
        pPeakMatch.add(feMinMatchedFrags.comp, new CC().wrap());

        pPeakMatch.add(feIonSeries.label(), alignRight);
        pPeakMatch.add(feIonSeries.comp, new CC().growX());
        pPeakMatch.add(labelCustomIonSeries, new CC().split(2).spanX());
        pPeakMatch.add(feCustomSeries.comp, new CC().growX().wrap());

        pPeakMatch.add(feTrueTolUnits.label(), alignRight);
        pPeakMatch.add(feTrueTolUnits.comp, new CC().split(2));
        pPeakMatch.add(feTrueTol.comp, new CC().growX());

        pPeakMatch.add(feOverrideCharge.comp, new CC().split(4).spanX());
        pPeakMatch.add(fePrecursorChargeLo.comp);
        pPeakMatch.add(new JLabel("-"));
        pPeakMatch.add(fePrecursorChargeHi.comp, wrap);
        pPeakMatch.add(feReportTopN.label(), alignRight);
        pPeakMatch.add(feReportTopN.comp, new CC().growX());
        pPeakMatch.add(feReportAltProts.comp, new CC().alignX("left").spanX().wrap());
        pPeakMatch.add(feOutputType.label(), alignRight);
        pPeakMatch.add(feOutputType.comp);
        pPeakMatch.add(feOutputMaxExpect.label(), alignRight);
        pPeakMatch.add(feOutputMaxExpect.comp, wrap);
      }

      pAdvanced.add(pSpectral, new CC().wrap().growX());
      pAdvanced.add(pPeakMatch, new CC().wrap().growX());
      pAdvanced.add(pOpenSearch, new CC().wrap().growX());

      pContent.add(pAdvanced, new CC().wrap().growX());
    }

    this.add(scroll, BorderLayout.CENTER);
  }

  private void onClickSave(ActionEvent e) {
    cacheSave();

    // now save the actual user's choice
    JFileChooser fc = new JFileChooser();
    fc.setApproveButtonText("Save");
    fc.setApproveButtonToolTipText("Save to a file");
    fc.setDialogTitle("Choose where params file should be saved");
    fc.setMultiSelectionEnabled(false);

    final String propName = ThisAppProps.PROP_FRAGGER_PARAMS_FILE_IN;
    ThisAppProps.load(propName, fc);

    fc.setSelectedFile(new File(MsfraggerParams.CACHE_FILE));
    Component parent = SwingUtils.findParentFrameForDialog(this);
    int saveResult = fc.showSaveDialog(parent);
    if (JFileChooser.APPROVE_OPTION == saveResult) {
      File selectedFile = fc.getSelectedFile();
      Path path = Paths.get(selectedFile.getAbsolutePath());
      ThisAppProps.save(propName, path.toString());

      // if exists, overwrite
      if (Files.exists(path)) {
        int overwrite = JOptionPane.showConfirmDialog(parent, "<html>File exists, overwrtie?<br/><br/>" + path.toString(), "Overwrite", JOptionPane.OK_CANCEL_OPTION);
        if (JOptionPane.OK_OPTION != overwrite) {
          return;
        }
        try {
          Files.delete(path);
        } catch (IOException ex) {
          JOptionPane.showMessageDialog(parent, "Could not overwrite", "Overwrite", JOptionPane.ERROR_MESSAGE);
          return;
        }
      }
      try {
        ThisAppProps.save(PROP_FILECHOOSER_LAST_PATH, path.toAbsolutePath().toString());
        MsfraggerParams params = formCollect();

        params.save(new FileOutputStream(path.toFile()));
        params.save();

      } catch (IOException ex) {
        JOptionPane.showMessageDialog(parent, "<html>Could not save file: <br/>" + path.toString() +
            "<br/>" + ex.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
        return;
      }
    }
  }

  private void cacheSave() {
    // saving form data, except modification tables
    {
      Map<String, String> map = formTo();
      Properties mapAsProps = PropertiesUtils.from(map);
      Path tempFileForm = CacheUtils.getTempFile(CACHE_FORM);
      try {
        mapAsProps.store(Files.newBufferedWriter(tempFileForm), ThisAppProps.cacheComments());
      } catch (IOException e) {
        log.warn("Could not store {} cache as map to: {}", this.getClass().getSimpleName(), tempFileForm.toString());
      }
    }

    // storing form properties that can't be just represented in the map
    {
      MsfraggerParams msfraggerParams = formCollect();
      Path tempFileProps = CacheUtils.getTempFile(CACHE_PROPS);
      try {
        msfraggerParams.save(Files.newOutputStream(tempFileProps));
      } catch (IOException e) {
        log.warn("Could not store {} cache as msfragger props to: {}", this.getClass().getSimpleName(), tempFileProps.toString());
      }
    }
  }

  private void cacheLoad() {
    // load form as map first
    {
      try {
        Path path = CacheUtils.locateTempFile(CACHE_FORM);
        Properties propsFromFile = PropertiesUtils.from(path);
        Map<String, String> map = PropertiesUtils.to(propsFromFile);
        formFrom(map);
      } catch (FileNotFoundException ignored) {
        // no form cache yet
      } catch (IOException e) {
        log.warn("Could not load properties as map from cache file: {}", e.getMessage());
      }
    }

    // then load specific msfragger non-properties-representable params
    {
      try {
        Path path = CacheUtils.locateTempFile(CACHE_PROPS);
        MsfraggerParams params = new MsfraggerParams();
        params.load(Files.newInputStream(path), false);
        formFrom(params);
      } catch (FileNotFoundException ignored) {
        // no form cache yet
      } catch (IOException e) {
        log.warn("Could not load properties as map from cache file: {}", e.getMessage());
      }

    }
  }

  private void setJTableColSize(JTable table, int colIndex, int minW, int maxW, int prefW) {
    table.getColumnModel().getColumn(colIndex).setMinWidth(minW);
    table.getColumnModel().getColumn(colIndex).setMaxWidth(maxW);
    table.getColumnModel().getColumn(colIndex).setPreferredWidth(prefW);
  }

  private synchronized TableModel getDefaultVarModTableModel() {
    if (tableModelVarMods != null) {
      return tableModelVarMods;
    }
    Object[][] data = new Object[MsfraggerParams.VAR_MOD_COUNT_MAX][TABLE_VAR_MODS_COL_NAMES.length];
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
    Object[][] data = new Object[MsfraggerParams.ADDONS_HUMAN_READABLE.length][TABLE_FIX_MODS_COL_NAMES.length];
    for (int i = 0; i < data.length; i++) {
      data[i][0] = false;
      data[i][1] = MsfraggerParams.ADDONS_HUMAN_READABLE[i];
      data[i][2] = 0.0;
    }

    tableModelFixMods = new ModificationsTableModel(
        TABLE_FIX_MODS_COL_NAMES,
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

  private void formFrom(MsfraggerParams params) {
    Map<String, String> map = paramsTo(params);
    formFrom(map);
    formFromMods(tableModelVarMods, TABLE_VAR_MODS_COL_NAMES, params.getVariableMods());
    formFromMods(tableModelFixMods, TABLE_FIX_MODS_COL_NAMES, params.getAdditionalMods());
    updateRowHeights(tableVarMods);
    setJTableColSize(tableVarMods, 0, 20, 150, 50);
    updateRowHeights(tableFixMods);
    setJTableColSize(tableFixMods, 0, 20, 150, 50);
  }

  private MsfraggerParams formCollect() {
    Map<String, String> map = formTo();
    MsfraggerParams params = paramsFrom(map);

    // before collecting mods, make sure that no table cell editor is open
    stopJTableEditing(tableFixMods);
    stopJTableEditing(tableVarMods);

    List<Mod> modsVar = formTo(tableModelVarMods);
    params.setVariableMods(modsVar);
    List<Mod> modsFix = formTo(tableModelFixMods);
    params.setAdditionalMods(modsFix);
    return params;
  }

  private boolean stopJTableEditing(JTable t) {
    TableCellEditor editor = t.getCellEditor();
    if (editor == null) {
      log.debug("cell editor was null");
      return true;
    }
    log.debug("cell editor existed, trying to close");
    return editor.stopCellEditing();
  }

  private void formFromMods(ModificationsTableModel model, Object[] colNames, List<Mod> mods) {
    Object[][] data = modListToTableData(mods);
    model.setDataVector(data, colNames);
  }

  private List<Mod> formTo(ModificationsTableModel model) {
    return model.getModifications();
  }

  private void formFrom(Map<String, String> map) {
    SwingUtilities.invokeLater(() -> valuesFromMap(this, map));
  }

  public void valuesFromMap(Container origin, Map<String, String> map) {
    Map<String, Component> comps = SwingUtils.mapComponentsByName(origin, true);
    for (Entry<String, String> kv : map.entrySet()) {
      final String name = kv.getKey();
      Component component = comps.get(name);
      if (component != null) {
        if (!(component instanceof StringRepresentable)) {
          log.trace(String
              .format("SwingUtils.valuesFromMap() Found component of type [%s] by name [%s] which does not implement [%s]",
                  component.getClass().getSimpleName(), name,
                  StringRepresentable.class.getSimpleName()));
          continue;
        }
        try {
          ((StringRepresentable) component).fromString(kv.getValue());
        } catch (IllegalArgumentException ex) {
          if (component.equals(uiComboMassMode)) {
            log.error("When loading fragger-mass-mode option, the given value ({}) is no longer an option in MSfragger/FragPipe. "
                + "Not changing value, please select manually", kv.getValue());
          } else if (component instanceof JComboBox) {
            log.warn(
                "Tried to load a value in combo-box that is not in combo-box's model. Component name={}, input value={}",
                name, kv.getValue());
          } else {
            log.warn(
                "Illegal input when filling UI form. Name={}, input value={}", name, kv.getValue());
          }
        }

      }
    }
  }

  private Map<String, String> formTo() {
    return SwingUtils.valuesToMap(this);
  }

  public MsfraggerParams getParams() {
    return formCollect();
  }

  /**
   * Converts textual representations of all fields in the form to stadard {@link MsfraggerParams}.
   */
  private MsfraggerParams paramsFrom(Map<String, String> map) {
    MsfraggerParams p = new MsfraggerParams();
    final double[] precursorRemoveRange = new double[2];
    final double[] clearMzRange = new double[2];
    final double[] digestMassRange = new double[2];
    final int[] precursorChargeRange = new int[2];

    for (Entry<String, String> e : map.entrySet()) {
      final String k = e.getKey();
      final String v = e.getValue();
      if (MsfraggerParams.PROP_NAMES_SET.contains(k)) {
        // known property
        Function<String, String> converter = CONVERT_TO_FILE.getOrDefault(k, s -> s);
        String converted = converter.apply(v);

        if (MsfraggerParams.PROP_fragment_ion_series.equals(k) && StringUtils.isNullOrWhitespace(converted)) {
          // don't set ion series to be used in the fragger config file if the string is emtpty
          continue;
        }
        p.getProps().setProp(k, converted);

      } else {
        // unknown prop, it better should be from the "misc" category we added in this panel
        if (PROPS_MISC_NAMES.contains(k) || k.startsWith("misc.")) {
          log.trace("Found misc option: {}={}", k, v);

          switch (k) {
            case PROP_misc_fragger_remove_precursor_range_lo:
              precursorRemoveRange[0] = Double.parseDouble(v);
              break;
            case PROP_misc_fragger_remove_precursor_range_hi:
              precursorRemoveRange[1] = Double.parseDouble(v);
              break;
            case PROP_misc_fragger_clear_mz_lo:
              clearMzRange[0] = Double.parseDouble(v);
              break;
            case PROP_misc_fragger_clear_mz_hi:
              clearMzRange[1] = Double.parseDouble(v);
              break;
            case PROP_misc_fragger_digest_mass_lo:
              digestMassRange[0] = Double.parseDouble(v);
              break;
            case PROP_misc_fragger_digest_mass_hi:
              digestMassRange[1] = Double.parseDouble(v);
              break;
            case PROP_misc_fragger_precursor_charge_lo:
              precursorChargeRange[0] = Integer.parseInt(v);
              break;
            case PROP_misc_fragger_precursor_charge_hi:
              precursorChargeRange[1] = Integer.parseInt(v);
              break;
          }

        } else {
          // we don't know what this option is, someone probably forgot to add it to the list of
          // known ones
          log.debug("Unknown prop name in fragger panel: [{}] with value [{}]", k, v);
        }
      }
    }
    p.setClearMzRange(clearMzRange);
    p.setDigestMassRange(digestMassRange);
    p.setPrecursorCharge(precursorChargeRange);
    p.setRemovePrecursorRange(precursorRemoveRange);

    FraggerOutputType outType = p.getOutputFormat();
    if (outType == null) {
      throw new IllegalStateException("FraggerOutputType was not set by the point where we needed to provide the output extension.");
    }
    p.setOutputFileExtension(outType.getExtension());

    return p;
  }

  private Map<String, String> paramsTo(MsfraggerParams params) {
    HashMap<String, String> map = new HashMap<>();
    for (Entry<String, Prop> e : params.getProps().getMap().entrySet()) {
      if (e.getValue().isEnabled) {
        final Function<String, String> converter = CONVERT_TO_GUI.get(e.getKey());
        final String converted;
        if (converter != null) {
          try {
            converted = converter.apply(e.getValue().value);
            map.put(e.getKey(), converted);
          } catch (Exception ex) {
            log.error("Error converting parameter [{}={}]", e.getKey(), e.getValue().value);
          }
        } else {
          converted = e.getValue().value;
          map.put(e.getKey(), converted);
        }
      }
    }

    // special treatment of some fields
    double[] precursorRemoveRange = params.getRemovePrecursorRange();
    double[] clearMzRange = params.getClearMzRange();
    double[] digestMassRange = params.getDigestMassRange();
    int[] precursorCharge = params.getPrecursorCharge();
    DecimalFormat fmt = new DecimalFormat("0.#");
    DecimalFormat fmt2 = new DecimalFormat("0.##");
    map.put(PROP_misc_fragger_clear_mz_lo, fmt.format(clearMzRange[0]));
    map.put(PROP_misc_fragger_clear_mz_hi, fmt.format(clearMzRange[1]));
    map.put(PROP_misc_fragger_digest_mass_lo, fmt.format(digestMassRange[0]));
    map.put(PROP_misc_fragger_digest_mass_hi, fmt.format(digestMassRange[1]));
    map.put(PROP_misc_fragger_precursor_charge_lo, fmt.format(precursorCharge[0]));
    map.put(PROP_misc_fragger_precursor_charge_hi, fmt.format(precursorCharge[1]));
    map.put(PROP_misc_fragger_remove_precursor_range_lo, fmt2.format(precursorRemoveRange[0]));
    map.put(PROP_misc_fragger_remove_precursor_range_hi, fmt2.format(precursorRemoveRange[1]));

    return map;
  }

  private boolean modListContainsIllegalSites(List<Mod> mods) {
    return mods.stream().anyMatch(m -> m.sites != null && m.sites.contains("[*"));
  }

  private Object[][] modListToTableData(List<Mod> mods) {
    final Object[][] data = new Object[mods.size()][3];
    for (int i = 0; i < mods.size(); i++) {
      Mod m = mods.get(i);
      data[i][0] = m.isEnabled;
      data[i][1] = m.sites;
      data[i][2] = m.massDelta;
    }
    return data;
  }

  private void setTableData(DefaultTableModel model, Object[][] data, String[] colNames,
      int maxRows) {
    model.setRowCount(0);
    model.setRowCount(maxRows);
    model.setDataVector(data, colNames);
  }

  @Subscribe
  public void onValidityFragger(MessageValidityFragger msg) {
    enablementMapping.put(this, msg.isValid);
    updateEnabledStatus(this, msg.isValid);
  }

  @Subscribe
  public void onValidityMsadjuster(MessageValidityMsadjuster msg) {
    log.debug("'Adjust precursor masses' checkbox was removed. Not reacting to MessageValidityMsadjuster event.");
//    enablementMapping.put(uiCheckAdjustPrecursorMass, msg.isValid);
//    updateEnabledStatus(uiCheckAdjustPrecursorMass, msg.isValid);
  }

  @Subscribe
  public void onValidityMassCalibration(MessageValidityMassCalibration msg) {
    log.debug("Got message 'MessageValidityMassCalibration' reading isValid = {} ", msg.isValid);
    enablementMapping.put(uiComboMassCalibrate, msg.isValid);
    updateEnabledStatus(uiComboMassCalibrate, msg.isValid);
  }

  private void updateEnabledStatus(Component top, boolean enabled) {
    if (top == null || top.isEnabled() == enabled)
      return;
    SwingUtilities.invokeLater(() -> {
      ArrayDeque<Component> stack = new ArrayDeque<>();
      stack.push(top);
      while (!stack.isEmpty()) {
        Component c = stack.pop();
        Container parent = c.getParent();
        boolean parentsEnabledStatus = parent != null && parent.isEnabled();
        boolean enabledStatus = enabled && parentsEnabledStatus && enablementMapping.getOrDefault(c, true);

        c.setEnabled(enabledStatus);
        if (c instanceof Container) {
          for (Component child : ((Container) c).getComponents()) {
            stack.push(child);
          }
        }
      }
    });
  }

  @Subscribe
  public void onPrecursorSelectionMode(MessagePrecursorSelectionMode m) {
    log.debug("Received MessagePrecursorSelectionMode [{}]. Doing nothing.", m.mode.name());
  }

  @Subscribe
  public void onMsfraggerParamsUpdated(MessageMsfraggerParamsUpdate m) {
    formFrom(m.params);
    cacheSave();
  }

  @Subscribe
  public void onSearchType(MessageSearchType m) {
    loadDefaults(m.type);
  }

  public int getRamGb() {
    return (Integer) uiSpinnerRam.getValue();
  }

  public int getThreads() {
    return (Integer) uiSpinnerThreads.getValue();
  }

  public boolean isRun() {
    return checkRun.isSelected() && checkRun.isEnabled();
  }

  public boolean isMsadjuster() {
    FraggerPrecursorMassMode mode = FraggerPrecursorMassMode.valueOf((String) uiComboMassMode.getSelectedItem());
    if (FraggerPrecursorMassMode.recalculated.equals(mode)) {
      return true;
    }
    return false;
  }

  public int getNumDbSlices() {
    return uiSpinnerDbslice.getActualValue();
  }

  public String getOutputFileExt() {
    return getOutputType().getExtension();
  }

  public FraggerOutputType getOutputType() {
    String val = uiComboOutputType.getItemAt(uiComboOutputType.getSelectedIndex());
    return FraggerOutputType.valueOf(val);
  }

  private void onClickLoad(ActionEvent e) {
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
          MsfraggerParams p = formCollect();
          p.load(new FileInputStream(selectedFile), true);
          EventBus.getDefault().post(new MessageMsfraggerParamsUpdate(p));
          p.save();

        } catch (Exception ex) {
          JOptionPane
              .showMessageDialog(parent,
                  "<html>Could not load the saved file: <br/>" + ex.getMessage(), "Error",
                  JOptionPane.ERROR_MESSAGE);
        }
      } else {
        JOptionPane.showMessageDialog(parent, "<html>This is strange,<br/> "
                + "but the file you chose to load doesn't exist anymore.", "Strange",
            JOptionPane.ERROR_MESSAGE);
      }
    }
  }

  private void loadDefaults(SearchTypeProp type) {
    MsfraggerParams params = new MsfraggerParams();
    params.loadDefaults(type);
    formFrom(params);

    // reset some fields that are not part of Fragger config file
    uiSpinnerDbslice.setValue(1);
  }

  /**
   * @return False if user's confirmation was required, but they cancelled the operation. True
   *         otherwise.
   */
  private boolean loadDefaults(final SearchTypeProp type, boolean askConfirmation) {
    if (askConfirmation) {
      int confirmation = JOptionPane.showConfirmDialog(SwingUtils.findParentFrameForDialog(this),
          "Load " + type + " search default configuration?");
      if (JOptionPane.YES_OPTION != confirmation) {
        return false;
      }
    }
    loadDefaults(type);
    return true;
  }

  private boolean trySelectEnzymeDropdown(String name) {
    try {
      String saveCuts = uiTextCuts.getNonGhostText();
      String saveNouts = uiTextNocuts.getNonGhostText();
      uiComboEnzymes.setSelectedItem(name);
      if (true || "custom".equals(name)) { // remove 'true' to reset specificities to their definitions in our file
        uiTextCuts.setText(saveCuts);
        uiTextNocuts.setText(saveNouts);
      }
      return true;
    } catch (Exception ignored) {
    }
    return false;
  }

  /**
   * @return False if user's confirmation was required, but they cancelled the operation. True
   *         otherwise.
   */
  private boolean postSearchTypeUpdate(SearchTypeProp type, boolean askConfirmation) {
    if (askConfirmation) {
      int confirmation = JOptionPane.showConfirmDialog(SwingUtils.findParentFrameForDialog(this),
          "<html>Would you like to update options for other tools as well?<br/>"
              + "<b>Highly recommended</b>, unless you're sure what you're doing)");
      if (JOptionPane.OK_OPTION != confirmation) {
        return false;
      }
    }
    EventBus.getDefault().post(new MessageSearchType(type));
    return true;
  }

  @Subscribe
  public void onDbslicingInitDone(DbSlice.MessageInitDone m) {
    enablementMapping.put(uiSpinnerDbslice, m.isSuccess);
    updateEnabledStatus(uiSpinnerDbslice, m.isSuccess);
  }

  @Subscribe
  public void onRun(MessageRun msg) {
    cacheSave();
  }

  @Subscribe
  public void onSaveCache(MessageSaveCache msg) {
    cacheSave();
  }

  public String getEnzymeName() {
    return uiTextEnzymeName.getNonGhostText();
  }
}
