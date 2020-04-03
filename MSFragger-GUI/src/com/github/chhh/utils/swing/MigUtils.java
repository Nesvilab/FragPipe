package com.github.chhh.utils.swing;

import javax.swing.JPanel;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;

public class MigUtils {
  private static final MigUtils INSTANCE = new MigUtils();

  private MigUtils() {
  }

  public static MigUtils get() {
    return MigUtils.INSTANCE;
  }

  public CC ccGx() {
    return new CC().growX();
  }

  public CC ccL() {
    return new CC().alignX("left");
  }

  public CC ccR() {
    return new CC().alignX("right");
  }

  public JPanel panel() {
    return panel(false);
  }

  public JPanel panel(boolean zeroInsets) {
    LC lc = new LC().fillX();
    if (zeroInsets)
      lc = lc.insetsAll("0px");
    return new JPanel(new MigLayout(lc));
  }
}
