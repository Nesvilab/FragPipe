package com.dmtavt.fragpipe.tools.protproph;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.messages.MessageSearchType;
import com.dmtavt.fragpipe.messages.NoteConfigPhilosopher;
import com.dmtavt.fragpipe.params.ThisAppProps;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.swing.FormEntry;
import com.github.chhh.utils.swing.JPanelBase;
import com.github.chhh.utils.swing.UiCheck;
import com.github.chhh.utils.swing.UiText;
import com.github.chhh.utils.swing.UiUtils;
import java.awt.Component;
import java.awt.ItemSelectable;
import javax.swing.JButton;
import javax.swing.JPanel;
import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.ThreadMode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ProtProphPanel extends JPanelBase {
  private static final Logger log = LoggerFactory.getLogger(ProtProphPanel.class);
  public static final String PREFIX = "protein-prophet.";
  private UiCheck checkRun;
  private JButton btnAllowMassShifted;
  private JButton btnDisallowMassShifted;
  private UiText uiTextCmdOpts;
  private JPanel pTop;
  private JPanel pContent;

  public boolean isRun() {
    return SwingUtils.isEnabledAndChecked(checkRun);
  }

  public String getCmdOpts() {
    return uiTextCmdOpts.getNonGhostText().trim();
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

  private void loadDefaults(String type) {
    String v = Fragpipe.getPropFix(ThisAppProps.PROP_TEXT_CMD_PROTEIN_PROPHET, type);
    if (v == null) {
      throw new IllegalStateException("No property found for key: " + ThisAppProps.PROP_TEXT_CMD_PROTEIN_PROPHET + "." + type);
    }
    uiTextCmdOpts.setText(v);
  }

  @Override
  public void init() {
    checkRun = UiUtils.createUiCheck("Run ProteinProphet", true);
    checkRun.setName("run-protein-prophet");
    // btnAllowMassShifted = UiUtils.createButton("Allow mass shifted peptides", e -> {
    //   log.debug("Clicked button " + btnAllowMassShifted.getText());
    //   loadDefaults("open");
    // });
    // btnDisallowMassShifted = UiUtils.createButton("Do NOT allow mass shifted peptides", e -> {
    //   log.debug("Clicked button " + btnDisallowMassShifted.getText());
    //   loadDefaults("tight");
    // });
    uiTextCmdOpts = UiUtils.uiTextBuilder().cols(20).text(defaultCmdOpt()).create();
    FormEntry feCmdOpts = mu.feb("cmd-opts", uiTextCmdOpts).label("Cmd line opts:").create();

    mu.layout(this, mu.lcFillXNoInsetsTopBottom());
    mu.border(this, "Protein Inference");

    pTop = mu.newPanel(null, mu.lcFillXNoInsetsTopBottom());
    mu.add(pTop, checkRun).wrap();
    // mu.add(pTop, btnAllowMassShifted);
    // mu.add(pTop, btnDisallowMassShifted).wrap();

    pContent = mu.newPanel(null, mu.lcFillXNoInsetsTopBottom());
    mu.add(pContent, feCmdOpts.label()).alignX("right");
    mu.add(pContent, feCmdOpts.comp).growX().pushX().wrap();

    mu.add(this, pTop).growX().wrap();
    mu.add(this, pContent).growX().wrap();
  }

  private String defaultCmdOpt() {
    return Fragpipe.getPropFix(ThisAppProps.PROP_TEXT_CMD_PROTEIN_PROPHET, "open");
  }

  @Override
  public void initMore() {
    updateEnabledStatus(this, false); // will get enabled when Philosopher is selected
    super.initMore();
  }

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void on(MessageSearchType m) {
    loadDefaults(m.type.name());
  }

  @Subscribe(sticky = true, threadMode = ThreadMode.MAIN_ORDERED)
  public void on(NoteConfigPhilosopher m) {
    updateEnabledStatus(this, m.isValid());
  }
}
