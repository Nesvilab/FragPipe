package com.dmtavt.fragpipe.tools.pepproph;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.api.Bus;
import com.dmtavt.fragpipe.messages.NoteConfigPhilosopher;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.UsageTrigger;
import com.github.chhh.utils.swing.FormEntry;
import com.github.chhh.utils.swing.JPanelWithEnablement;
import com.github.chhh.utils.swing.MigUtils;
import com.github.chhh.utils.swing.UiCheck;
import com.github.chhh.utils.swing.UiCombo;
import com.github.chhh.utils.swing.UiText;
import com.github.chhh.utils.swing.UiUtils;
import java.awt.Component;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.ThreadMode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import umich.msfragger.gui.api.SearchTypeProp;

public class PepProphPanel extends JPanelWithEnablement {
  private static final Logger log = LoggerFactory.getLogger(PepProphPanel.class);
  private static MigUtils mu = MigUtils.get();
  private UiCheck checkRunPeptideProphet;
  private UiText uiTextCmdOpts;
  private UiCheck uiCheckCombinePepxml;
  private JPanel pTop;
  private JPanel pContent;
  public static final String PREFIX = "peptide-prophet.";

  public PepProphPanel() {
    init();
    initMore();
  }

  private void initMore() {
    SwingUtils.renameDeep(this, false, PREFIX, null);
    updateEnabledStatus(this, false);
    Bus.registerQuietly(this);
  }

  @Subscribe(sticky = true, threadMode = ThreadMode.MAIN_ORDERED)
  public void on(NoteConfigPhilosopher m) {
    updateEnabledStatus(this, m.isValid());
  }

  private void init() {
    checkRunPeptideProphet = UiUtils.createUiCheck("Run PeptideProphet", true);
    JLabel labelDefaults = new JLabel("Defaults for:");
    final LinkedHashMap<String, SearchTypeProp> defaults = new LinkedHashMap<>();
    defaults.put("Closed Search", SearchTypeProp.closed);
    defaults.put("Open Search", SearchTypeProp.open);
    defaults.put("Non-Specific Search", SearchTypeProp.nonspecific);
    defaults.put("Offset Search", SearchTypeProp.offset);
    final UiCombo uiComboDefaults = UiUtils.createUiCombo(new ArrayList<>(defaults.keySet()));
    uiTextCmdOpts = UiUtils.uiTextBuilder().cols(20).create();
    FormEntry feCmdOpts = fe(uiTextCmdOpts, "cmd-opts")
        .label("Cmd line opts:")
        .tooltip("These options will be passed on to Peptide Prophet.\n"
            + "This set will be merged with some additional options\n"
            + "added as a requirement by other stages in the pipeline.\n"
            + "See output log (e.g. dry-run results) for the complete command.").create();
    uiCheckCombinePepxml = UiUtils
        .createUiCheck("<html>Single <b>combined</b> pepxml file per experiment / group", false);
    uiCheckCombinePepxml.setName("combine-pepxml");
    JButton btnLoadDefaults = UiUtils
        .createButton("Load", "Load peptide prophet settings for given search type", e -> {
          SearchTypeProp st = defaults.get((String) uiComboDefaults.getSelectedItem());
          Fragpipe.getPropAndSetVal("peptideprophet.cmd.line.opts." + st.name(), uiTextCmdOpts);
          Fragpipe.getPropAndSetVal("peptideprophet.combine.pepxml." + st.name(), uiCheckCombinePepxml);
        });

    mu.layout(this).fillX();
    mu.border(this, "PeptideProphet");

    pTop = mu.newPanel(null, mu.lcFillXNoInsetsTopBottom());
    mu.add(pTop, checkRunPeptideProphet).split();
    mu.add(pTop, labelDefaults);
    mu.add(pTop, uiComboDefaults);
    mu.add(pTop, btnLoadDefaults).wrap();

    pContent = mu.newPanel(null, mu.lcFillXNoInsetsTopBottom());
    mu.add(pContent, feCmdOpts.label()).alignX("right");
    mu.add(pContent, feCmdOpts.comp).growX().pushX().wrap();
    mu.add(pContent, uiCheckCombinePepxml).skip(1).wrap();

    mu.add(this, pTop).growX().wrap();
    mu.add(this, pContent).growX().wrap();
  }

  private FormEntry.Builder fe(JComponent comp, String name) {
    return Fragpipe.fe(comp, name, PREFIX);
  }


}
