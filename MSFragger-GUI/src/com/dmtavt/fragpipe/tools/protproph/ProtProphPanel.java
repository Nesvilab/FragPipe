package com.dmtavt.fragpipe.tools.protproph;

import com.dmtavt.fragpipe.api.Bus;
import com.github.chhh.utils.swing.JPanelBase;
import com.github.chhh.utils.swing.UiCheck;
import com.github.chhh.utils.swing.UiUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ProtProphPanel extends JPanelBase {
  private static final Logger log = LoggerFactory.getLogger(ProtProphPanel.class);
  public static final String PREFIX = "protein-prophet.";

  @Override
  public void init() {
    mu.layout(this).fillX();
    mu.border(this, "ProteinProphet");

    UiUtils.createUiCheck("Run Protein Prophet", true);
  }

  @Override
  public void initMore() {
    Bus.registerQuietly(this);
  }
}
