package com.dmtavt.fragpipe;

import com.dmtavt.fragpipe.messages.MessageClearCache;
import com.dmtavt.fragpipe.messages.MessageFindTools;
import com.dmtavt.fragpipe.messages.MessageShowAboutDialog;
import com.dmtavt.fragpipe.messages.MessageUmpireEnabled;
import com.github.chhh.utils.swing.UiCheck;
import com.github.chhh.utils.swing.UiUtils;
import java.awt.Component;
import java.util.Properties;
import java.util.function.Supplier;
import javax.swing.JCheckBox;
import javax.swing.JEditorPane;
import javax.swing.JLabel;
import javax.swing.JPanel;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.greenrobot.eventbus.EventBus;
import umich.msfragger.params.ThisAppProps;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.swing.JPanelWithEnablement;

public class TabConfig extends JPanelWithEnablement {

  public TabConfig() {
    init();
    initMore();
  }

  private void initMore() {
    //EventBus.getDefault().register(this);
  }

  private void init() {
    this.setLayout(new MigLayout(new LC().fillX()));
    add(createPanelTopButtons(), new CC().growX().wrap());

    {
      JLabel c = new JLabel();
      c.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
      c.setAlignmentX(Component.CENTER_ALIGNMENT);
      c.setText(
          "<html>Tabs on top represent processing steps and will be performed sequentially.<br/>\n"
              + "Tabs will become enabled once the tools on this panel are configured.");
      add(c, new CC().growX().wrap());
    }

    {
      Properties props = ThisAppProps.getRemotePropertiesWithLocalDefaults();
//      Properties props = ThisAppProps.getLocalProperties(); // for testing
      String linkUrl = props.getProperty(ThisAppProps.PROP_SETUP_TUTORIAL_URL,
          "https://msfragger.nesvilab.org/tutorial_setup_fragpipe.html");

      JEditorPane c = SwingUtils.createClickableHtml(
          "<a href='" + linkUrl + "'>Configuration Help</a>");
      c.setAlignmentX(Component.CENTER_ALIGNMENT);
      JPanel p = new JPanel();
      p.setAlignmentX(Component.CENTER_ALIGNMENT);
      p.add(c);
      add(p, new CC().growX().wrap());
    }
  }

  private JPanel createPanelTopButtons() {
    JPanel p = new JPanel();
    p.setLayout(new MigLayout(new LC().fillX().debug()));
    Supplier<CC> ccL = () -> new CC().alignX("left");
    Supplier<CC> ccR = () -> new CC().alignX("right");
    p.add(UiUtils.createButton("About", e -> post(new MessageShowAboutDialog())), ccL.get().split().spanX());
    p.add(UiUtils.createButton("Clear Cache", e -> post(new MessageClearCache())), ccL.get());
    UiCheck uiCheckUmpire = UiUtils.createUiCheck("Enable DIA-Umpire", false,
        e -> post(new MessageUmpireEnabled(((JCheckBox) e.getSource()).isSelected())));
    p.add(uiCheckUmpire, ccL.get().wrap());
    p.add(UiUtils.createButton("Find tools", e -> post(new MessageFindTools())), ccL.get().split().spanX());
    p.add(new JLabel("Search for tools recursively in a direcotry"), ccL.get().wrap());
    return p;
  }

  private void post(Object message) {
    EventBus.getDefault().post(message);
  }
}
