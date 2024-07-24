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

package com.dmtavt.fragpipe.tools.skyline;

import static com.github.chhh.utils.OsUtils.isWindows;

import com.dmtavt.fragpipe.messages.MessageUiRevalidate;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.swing.FileChooserUtils;
import com.github.chhh.utils.swing.FileChooserUtils.FcMode;
import com.github.chhh.utils.swing.FormEntry;
import com.github.chhh.utils.swing.JPanelBase;
import com.github.chhh.utils.swing.UiCheck;
import com.github.chhh.utils.swing.UiCombo;
import com.github.chhh.utils.swing.UiRadio;
import com.github.chhh.utils.swing.UiText;
import com.github.chhh.utils.swing.UiUtils;
import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.ItemSelectable;
import java.awt.image.BufferedImage;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Objects;
import java.util.stream.Stream;
import javax.imageio.ImageIO;
import javax.swing.ButtonGroup;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.border.TitledBorder;
import javax.swing.filechooser.FileNameExtensionFilter;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.ThreadMode;


public class SkylinePanel extends JPanelBase {

  private static final String PREFIX = "skyline.";

  private JCheckBox checkRun;
  private JPanel pContent;
  private JPanel pTop;
  private JPanel panelBasic;
  private UiRadio uiRadioSkyline;
  private UiRadio uiRadioSkylineDaily;
  private UiRadio uiRadioSkylineCustom;
  private UiText uiTextSkylineCustom;
  private UiCombo uiComboMode;
  private UiCombo uiComboModsMode;

  @Override
  protected void initMore() {
    super.initMore();
    SwingUtils.setEnablementUpdater(this, pContent, checkRun);
  }

  @Override
  protected ItemSelectable getRunCheckbox() {
    return checkRun;
  }

  @Override
  protected Component getEnablementToggleComponent() {
    return pContent;
  }

  @Override
  protected String getComponentNamePrefix() {
    return PREFIX;
  }

  private JPanel createPanelTop() {
    // setting the insets allows the top panel to be shifted left of the options panel
    JPanel p = new JPanel(new MigLayout(new LC().insetsAll("0px")));
    mu.borderEmpty(p);

    checkRun = new UiCheck("Generate Skyline Document", null, false);
    checkRun.setName("run-skyline");

    JLabel imageLabel = new JLabel();
    try {
      BufferedImage image = ImageIO.read(Objects.requireNonNull(getClass().getResource("/com/dmtavt/fragpipe/icons/icon-skyline-48.png")));
      imageLabel = new JLabel(new ImageIcon(image));
    } catch (Exception ex) {
      ex.printStackTrace();
    }

    mu.add(p, checkRun).pushX();
    mu.add(p, imageLabel).gapRight("50").wrap();

    return p;
  }

  private JPanel createPanelContent() {
    JPanel p = new JPanel(new MigLayout(new LC().fillX()));
    mu.borderEmpty(p);

    panelBasic = createPanelBasic();

    mu.add(p, panelBasic).growX().wrap();

    return p;
  }

  private JPanel createPanelBasic() {
    panelBasic = mu.newPanel(mu.lcFillX());
    mu.border(panelBasic, 1);

    ButtonGroup radioGroup = new ButtonGroup();

    uiRadioSkyline = new UiRadio("Skyline", null, true);
    radioGroup.add(uiRadioSkyline);
    FormEntry feRadioSkyline = new FormEntry("skyline", "Not shown", uiRadioSkyline);

    uiRadioSkylineDaily = new UiRadio("Skyline Daily", null, false);
    radioGroup.add(uiRadioSkylineDaily);
    FormEntry feRadioSkylineDaily = new FormEntry("skyline-daily", "Not shown", uiRadioSkylineDaily);

    uiRadioSkylineCustom = new UiRadio("Custom", null, false);
    radioGroup.add(uiRadioSkylineCustom);
    FormEntry feRadioSkylineCustom = new FormEntry("skyline-custom", "Not shown", uiRadioSkylineCustom);

    uiTextSkylineCustom = UiUtils.uiTextBuilder().create();
    FormEntry feSkylineCustom = new FormEntry("skyline-custom-path", "Skyline custom path (optional)", uiTextSkylineCustom, "");
    JButton jButtonSkylineCustom = feSkylineCustom.browseButton("Browse", "Select library file", () -> {
      final FileNameExtensionFilter fileNameExtensionFilter = new FileNameExtensionFilter("Skyline", "exe");
      JFileChooser fc = FileChooserUtils.create("Skyline executable file", "Select", false, FcMode.FILES_ONLY, true, fileNameExtensionFilter);
      fc.setFileFilter(fileNameExtensionFilter);
      FileChooserUtils.setPath(fc, Stream.of(uiTextSkylineCustom.getNonGhostText()));
      return fc;
    }, paths -> {
      Path path = paths.get(0); // we only allowed selection of a single file in the file chooser
      uiTextSkylineCustom.setText(path.toString());
    });

    uiRadioSkylineCustom.addItemListener(e -> {
      updateEnabledStatus(feSkylineCustom.comp, uiRadioSkylineCustom.isSelected());
      updateEnabledStatus(jButtonSkylineCustom, uiRadioSkylineCustom.isSelected());
    });

    uiComboMode = UiUtils.createUiCombo(Arrays.asList("Use speclib as input", "Use pep.xml as input"));
    uiComboMode.setSelectedIndex(0);
    FormEntry feComboMode = new FormEntry("skyline-mode", "Skyline running mode", uiComboMode, "Let Skyline take FragPipe speclib as input or build its own speclib");

    uiComboModsMode = UiUtils.createUiCombo(Arrays.asList("Default", "O-glyco", "N-glyco"));
    uiComboModsMode.setSelectedIndex(0);
    FormEntry feComboModsMode = new FormEntry("skyline-mods-mode", "Special Modifications Mode", uiComboModsMode, "Special modification support. If O-glyco, uses O-Pair glycan database instead of mass offsets list. If N-glyco, uses Glycan Composition Assignment glycan database instead of mass offsets list.");

    mu.add(panelBasic, feRadioSkyline.comp);
    mu.add(panelBasic, feRadioSkylineDaily.comp);
    mu.add(panelBasic, feRadioSkylineCustom.comp).split(3);
    mu.add(panelBasic, feSkylineCustom.comp).growX().pushX();
    mu.add(panelBasic, jButtonSkylineCustom).wrap();

    mu.add(panelBasic, feComboMode.label(), mu.ccL());
    mu.add(panelBasic, feComboMode.comp).wrap();

    mu.add(panelBasic, feComboModsMode.label(), mu.ccL());
    mu.add(panelBasic, feComboModsMode.comp).wrap();

    updateEnabledStatus(feSkylineCustom.comp, uiRadioSkylineCustom.isSelected());
    updateEnabledStatus(jButtonSkylineCustom, uiRadioSkylineCustom.isSelected());
    updateEnabledStatus(panelBasic, true);

    adjustRadios();

    return panelBasic;
  }

  @Override
  protected void init() {
    this.setLayout(new BorderLayout());
    this.setBorder(new TitledBorder("Skyline"));

    pTop = createPanelTop();
    pContent = createPanelContent();

    this.add(pTop, BorderLayout.NORTH);
    this.add(pContent, BorderLayout.CENTER);
  }

  @Override
  public boolean isRun() {
    return SwingUtils.isEnabledAndChecked(checkRun);
  }

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void on(MessageUiRevalidate m) {
    adjustRadios();
  }

  private void adjustRadios() {
    DefaultArtifactVersion skylineVersion = Skyline.getSkylineVersion();
    DefaultArtifactVersion skylineDailyVersion = Skyline.getSkylineDailyVersion();

    if (skylineVersion != null && skylineDailyVersion != null) {
      uiRadioSkyline.setSelected(true);
      updateEnabledStatus(uiRadioSkyline, true);
      uiRadioSkylineDaily.setSelected(false);
      updateEnabledStatus(uiRadioSkylineDaily, true);
    } else if (skylineVersion != null) {
      uiRadioSkyline.setSelected(true);
      updateEnabledStatus(uiRadioSkyline, true);
      uiRadioSkylineDaily.setSelected(false);
      updateEnabledStatus(uiRadioSkylineDaily, false);
    } else if (skylineDailyVersion != null) {
      uiRadioSkyline.setSelected(false);
      updateEnabledStatus(uiRadioSkyline, false);
      uiRadioSkylineDaily.setSelected(true);
      updateEnabledStatus(uiRadioSkylineDaily, true);
    } else {
      uiRadioSkyline.setSelected(false);
      updateEnabledStatus(uiRadioSkyline, false);
      uiRadioSkylineDaily.setSelected(false);
      updateEnabledStatus(uiRadioSkylineDaily, false);
    }
  }

  public String getSkylinePath() {
    if (!isWindows()) {
      return null;
    } else if (uiRadioSkyline.isEnabled() && uiRadioSkyline.isSelected()) {
      return Skyline.getSkylineRunnerPath();
    } else if (uiRadioSkylineDaily.isEnabled() && uiRadioSkylineDaily.isSelected()) {
      return Skyline.getSkylineDailyRunnerPath();
    } else if (uiRadioSkylineCustom.isSelected()) {
      return uiTextSkylineCustom.getNonGhostText();
    } else {
      return null;
    }
  }

  public String getSkylineVersion() {
    if (uiRadioSkyline.isEnabled() && uiRadioSkyline.isSelected()) {
      return Skyline.getSkylineVersion().toString();
    } else if (uiRadioSkylineDaily.isEnabled() && uiRadioSkylineDaily.isSelected()) {
      return Skyline.getSkylineDailyVersion().toString();
    } else if (uiRadioSkylineCustom.isSelected()) {
      try {
        return Skyline.sub2(uiTextSkylineCustom.getNonGhostText());
      } catch (Exception e) {
        throw new RuntimeException(e);
      }
    } else {
      return null;
    }
  }

  public int getMode() {
    return uiComboMode.getSelectedIndex();
  }

  public int getModsMode() {
    return uiComboModsMode.getSelectedIndex();
  }
}