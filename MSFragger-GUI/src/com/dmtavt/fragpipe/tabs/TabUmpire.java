package com.dmtavt.fragpipe.tabs;

import com.dmtavt.fragpipe.messages.MessageUmpireEnabled;
import com.github.chhh.utils.swing.JPanelWithEnablement;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.greenrobot.eventbus.EventBus;
import org.greenrobot.eventbus.Subscribe;
import umich.msfragger.params.umpire.UmpirePanel;

public class TabUmpire extends JPanelWithEnablement {

  private UmpirePanel umpirePanel;

  public TabUmpire() {
    init();
    initMore();
  }

  private void initMore() {
    EventBus.getDefault().register(this);
  }

  private void init() {
    this.setLayout(new MigLayout(new LC().fillX()));
    umpirePanel = new UmpirePanel();
    add(umpirePanel, new CC().growX().wrap());
  }

  @Subscribe
  public void onUmpireEnabled(MessageUmpireEnabled m) {
    umpirePanel.checkRunUmpireSe.setSelected(m.enabled);
  }
}
