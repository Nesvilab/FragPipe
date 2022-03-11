package com.dmtavt.fragpipe.tabs;

import com.dmtavt.fragpipe.tools.comet.CometPanel;
import com.github.chhh.utils.swing.JPanelWithEnablement;
import com.github.chhh.utils.swing.MigUtils;

public class TabComet extends JPanelWithEnablement {
    private static MigUtils mu = MigUtils.get();
    public static final String TAB_PREFIX = "comet.";
    private CometPanel panelComet;

    public TabComet() {
        init();
        initMore();
    }

    private void initMore() {
        //Bus.registerQuietly(this);
    }

    private void init() {
        mu.layout(this).fillX();

        panelComet = new CometPanel();

        mu.add(this, panelComet).growX().wrap();
    }
}
