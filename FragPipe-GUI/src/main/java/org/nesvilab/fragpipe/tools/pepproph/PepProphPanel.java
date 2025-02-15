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

package org.nesvilab.fragpipe.tools.pepproph;

import org.nesvilab.fragpipe.Fragpipe;
import org.nesvilab.fragpipe.api.SearchTypeProp;
import org.nesvilab.fragpipe.messages.MessageSearchType;
import org.nesvilab.fragpipe.messages.NoteConfigPeptideProphet;
import org.nesvilab.fragpipe.tools.PSMValidation;
import org.nesvilab.fragpipe.tools.msbooster.MSBoosterPanel;
import org.nesvilab.fragpipe.tools.percolator.PercolatorPanel;
import org.nesvilab.utils.SwingUtils;
import org.nesvilab.utils.swing.FormEntry;
import org.nesvilab.utils.swing.JPanelBase;
import org.nesvilab.utils.swing.MigUtils;
import org.nesvilab.utils.swing.UiCheck;
import org.nesvilab.utils.swing.UiCombo;
import org.nesvilab.utils.swing.UiRadio;
import org.nesvilab.utils.swing.UiText;
import org.nesvilab.utils.swing.UiUtils;
import java.awt.Component;
import java.awt.ItemSelectable;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import javax.swing.ButtonGroup;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.ThreadMode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class PepProphPanel extends JPanelBase {
  private static final Logger log = LoggerFactory.getLogger(PepProphPanel.class);
  private static MigUtils mu = MigUtils.get();
  private UiRadio checkRun;
  private UiText uiTextCmdOpts;
  private UiCheck uiCheckCombinePepxml;
  private JPanel pTop;
  private JPanel pContent;
  public static final String PREFIX = "peptide-prophet.";

  public PepProphPanel(ButtonGroup radioGroup) {
    radioGroup.add(checkRun);
  }

  public boolean isRun() {
    PSMValidation psmValidation = Fragpipe.getStickyStrict(PSMValidation.class);
    return psmValidation.isRun() && SwingUtils.isEnabledAndChecked(checkRun);
  }

  public boolean isSelected() {
    return checkRun.isSelected();
  }

  public String getCmdOpts() {
    return uiTextCmdOpts.getNonGhostText().trim();
  }

  public boolean isCombinePepxml() {
    return uiCheckCombinePepxml.isSelected();
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

  @Override
  protected void initMore() {
    updateEnabledStatus(this, true);
    super.initMore();
  }

  @Subscribe(sticky = true, threadMode = ThreadMode.MAIN_ORDERED)
  public void on(NoteConfigPeptideProphet m) {
    updateEnabledStatus(this, m.isValid());
    if (!m.isValid()) {
      PercolatorPanel percolatorPanel = Fragpipe.getStickyStrict(PercolatorPanel.class);
      percolatorPanel.checkRun.setSelected(true);
    }
  }

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void on(MessageSearchType m) {
    log.debug("Got MessageSearchType of type [{}], loading defaults for it", m.type.toString());
    switch (m.type) {
      case open:
      case offset:
      case glyco:
        checkRun.setSelected(true);
        break;
      case closed:
      case nonspecific:
        checkRun.setSelected(false);
        break;
      default:
        throw new IllegalStateException("Not covered enum option: " + m.type.name());
    }
    loadDefaults(m.type);
  }

  @Override
  protected void init() {
    checkRun = new UiRadio("Run PeptideProphet", null, true);
    checkRun.setName("run-peptide-prophet");
    JLabel labelDefaults = new JLabel("Defaults for:");
    final LinkedHashMap<String, SearchTypeProp> defaults = new LinkedHashMap<>();
    defaults.put("Closed Search", SearchTypeProp.closed);
    defaults.put("Open Search", SearchTypeProp.open);
    defaults.put("Non-Specific Search", SearchTypeProp.nonspecific);
    defaults.put("Offset Search", SearchTypeProp.offset);
    final UiCombo uiComboDefaults = UiUtils.createUiCombo(new ArrayList<>(defaults.keySet()));
    uiComboDefaults.addItemListener(e -> {
      SearchTypeProp type = defaults.get((String) uiComboDefaults.getSelectedItem());
      loadDefaults(type);
    });

    uiTextCmdOpts = UiUtils.uiTextBuilder().cols(20).text(defaultCmdOpts()).create();
    FormEntry feCmdOpts = fe(uiTextCmdOpts, "cmd-opts")
        .label("Cmd line opts:")
        .tooltip("These options will be passed on to PeptideProphet.\n"
            + "This set will be merged with some additional options\n"
            + "added as a requirement by other stages in the pipeline.\n"
            + "See output log (e.g. dry-run results) for the complete command.").create();
    uiCheckCombinePepxml = UiUtils
        .createUiCheck("<html>Single <b>combined</b> pepxml file per experiment / group", false);
    uiCheckCombinePepxml.setName("combine-pepxml");

    checkRun.addItemListener(e -> {
      if (checkRun.isSelected()) {
        MSBoosterPanel msBoosterPanel = Fragpipe.getStickyStrict(MSBoosterPanel.class);
        msBoosterPanel.setRunStatus(false);
      }
    });


    mu.layout(this).fillX();
    mu.borderEmpty(this);

    pTop = mu.newPanel(null, mu.lcFillXNoInsetsTopBottom());
    mu.add(pTop, checkRun).split();
    mu.add(pTop, labelDefaults);
    mu.add(pTop, uiComboDefaults);
    mu.add(pTop, uiCheckCombinePepxml).gapLeft("80px").wrap();

    pContent = mu.newPanel(null, mu.lcFillXNoInsetsTopBottom());
    mu.add(pContent, feCmdOpts.label()).alignX("right");
    mu.add(pContent, feCmdOpts.comp).growX().pushX().wrap();

    mu.add(this, pTop).growX().wrap();
    mu.add(this, pContent).growX().wrap();
  }

  private void loadDefaults(SearchTypeProp type) {
    Fragpipe.getPropsFixAndSetVal("peptideprophet.cmd.line.opts." + type.name(), uiTextCmdOpts);
    Fragpipe.getPropsFixAndSetVal("peptideprophet.combine.pepxml." + type.name(), uiCheckCombinePepxml);
  }

  private String defaultCmdOpts() {
    String v = Fragpipe.getPropFix("peptideprophet.cmd.line.opts", "closed");
    log.debug("PeptideProphet default value for Cmd Opts in ui fetched from properties: peptideprophet.cmd.line.opts.closed={}", v);
    return v;
  }

  private FormEntry.Builder fe(JComponent comp, String name) {
    return Fragpipe.fe(comp, name, PREFIX);
  }


}
