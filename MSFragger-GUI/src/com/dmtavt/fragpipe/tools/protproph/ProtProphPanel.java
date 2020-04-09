package com.dmtavt.fragpipe.tools.protproph;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.api.Bus;
import com.dmtavt.fragpipe.messages.NoteConfigPhilosopher;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.swing.FormEntry;
import com.github.chhh.utils.swing.JPanelBase;
import com.github.chhh.utils.swing.UiCheck;
import com.github.chhh.utils.swing.UiText;
import com.github.chhh.utils.swing.UiUtils;
import javax.swing.JButton;
import javax.swing.JPanel;
import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.ThreadMode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import umich.msfragger.params.ThisAppProps;

public class ProtProphPanel extends JPanelBase {
  private static final Logger log = LoggerFactory.getLogger(ProtProphPanel.class);
  public static final String PREFIX = "protein-prophet.";
  private UiCheck checkRun;
  private JButton btnAllowMassShifted;
  private JButton btnDisallowMassShifted;
  private UiText uiTextCmdOpts;
  private UiCheck uiCheckSeparateProtxml;
  private JPanel pTop;
  private JPanel pContent;

  @Override
  public void init() {
    checkRun = UiUtils.createUiCheck("Run Protein Prophet", true);
    checkRun.setName("run-protein-prophet");
    btnAllowMassShifted = UiUtils.createButton("Allow mass shifted peptides", e -> {
      log.debug("Clicked button " + btnAllowMassShifted.getText());
      String v = Fragpipe.getProp(ThisAppProps.PROP_TEXT_CMD_PROTEIN_PROPHET, "open");
      uiTextCmdOpts.setText(v);
    });
    btnDisallowMassShifted = UiUtils.createButton("Do NOT allow mass shifted peptides", e -> {
      log.debug("Clicked button " + btnDisallowMassShifted.getText());
      String v = Fragpipe.getProp(ThisAppProps.PROP_TEXT_CMD_PROTEIN_PROPHET, "tight");
      uiTextCmdOpts.setText(v);
    });
    uiTextCmdOpts = UiUtils.uiTextBuilder().cols(20).text(defaultCmdOpt()).create();
    FormEntry feCmdOpts = mu.feb("cmd-opts", uiTextCmdOpts).label("Cmd line opts:").create();
    uiCheckSeparateProtxml = UiUtils
        .createUiCheck("Separate Protein Prophet prot.xml file per experiment / group", false);

    mu.layout(this, mu.lcFillXNoInsetsTopBottom());
    mu.border(this, "ProteinProphet");

    pTop = mu.newPanel(null, mu.lcFillXNoInsetsTopBottom());
    mu.add(pTop, checkRun).split();
    mu.add(pTop, btnAllowMassShifted);
    mu.add(pTop, btnDisallowMassShifted).wrap();

    pContent = mu.newPanel(null, mu.lcFillXNoInsetsTopBottom());
    mu.add(pContent, feCmdOpts.label()).alignX("right");
    mu.add(pContent, feCmdOpts.comp).growX().pushX().wrap();
    mu.add(pContent, uiCheckSeparateProtxml).skip(1).wrap();

    mu.add(this, pTop).growX().wrap();
    mu.add(this, pContent).growX().wrap();
  }

  private String defaultCmdOpt() {
    return Fragpipe.getProp(ThisAppProps.PROP_TEXT_CMD_PROTEIN_PROPHET, "open");
  }

  @Override
  public void initMore() {
    SwingUtils.renameDeep(this, false, PREFIX, null);
    SwingUtils.setEnablementUpdater(this, pContent, checkRun);
    updateEnabledStatus(this, false); // will get enabled when Philosopher is selected
    Bus.registerQuietly(this);
  }

  @Subscribe(sticky = true, threadMode = ThreadMode.MAIN_ORDERED)
  public void on(NoteConfigPhilosopher m) {
    updateEnabledStatus(this, m.isValid());
  }
}
