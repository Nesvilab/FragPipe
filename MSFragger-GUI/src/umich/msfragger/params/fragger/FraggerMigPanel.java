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
import java.awt.BorderLayout;
import java.text.DecimalFormat;
import java.util.Arrays;
import javax.swing.DefaultComboBoxModel;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.border.EmptyBorder;
import javax.swing.border.TitledBorder;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.greenrobot.eventbus.EventBus;
import umich.msfragger.gui.api.SearchTypeProp;
import umich.msfragger.messages.MessageSearchType;
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
  private static String[] PROPS_MISC = {PROP_adjust_precurosr_mass, PROP_slice_db, PROP_ram};

  public FraggerMigPanel() {
    initMore();
  }

  private void initMore() {
    icon = new ImageIcon(
        getClass().getResource("/umich/msfragger/gui/icons/bolt-16.png"));

    this.setLayout(new BorderLayout());

//    LC lc = new LC();//.debug();
    LC lc = new LC().debug();

    // Top panel with checkbox, buttons and RAM+Threads spinners
    {
      JPanel pTop = new JPanel(new MigLayout(lc));
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
      JPanel pBasic = new JPanel(new MigLayout(lc));
      pBasic.setBorder(new TitledBorder("Basic Options"));

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
      pBasic.add(fePrecTolUnits.label(), new CC().alignX("right"));
      pBasic.add(fePrecTolUnits.comp, new CC());
      pBasic.add(feSpinnerPrecTolLo.comp, new CC().minWidth("45px"));
      pBasic.add(new JLabel("-"), new CC().span(2));
      pBasic.add(feSpinnerPrecTolHi.comp, new CC().minWidth("45px"));
      pBasic.add(feAdjustPrecMass.comp, new CC().gapLeft("5px").wrap());

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
      pBasic.add(feFragTolUnits.label(), new CC().alignX("right"));
      pBasic.add(feFragTolUnits.comp, new CC());
      pBasic.add(feFragTol.comp, new CC().minWidth("45px").maxWidth("100px").growX().wrap());

      UiText uiTextIsoErr = new UiText();
      uiTextIsoErr.setDocument(DocumentFilters.getFilter("[^\\d/-]+"));
      uiTextIsoErr.setText("-1/0/1/2");
      FormEntry feIsotopeError = new FormEntry(MsfraggerParams.PROP_isotope_error, "Isotope error", uiTextIsoErr,
          "<html>String of the form -1/0/1/2 indicating which isotopic<br/>peak selection errors MSFragger will try to correct.");
      pBasic.add(feIsotopeError.label(), new CC().alignX("right"));
      pBasic.add(feIsotopeError.comp, new CC().minWidth("45px").span(2).growX().wrap());



      pContent.add(pBasic, new CC().wrap().growX());
    }

    // Panel with all the advanced options
    {
      JPanel pAdvanced = new JPanel(new MigLayout(lc));
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
