package com.dmtavt.fragpipe.api;

import java.awt.Component;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class UiTab {
  private static final Logger log = LoggerFactory.getLogger(UiTab.class);

  private Icon icon;
  private String iconResourcePath;
  private final String title;
  private final Component comp;
  private final String tip;

  public UiTab(String title, Component comp, String iconResourcePath, String tip) {
    this.iconResourcePath = iconResourcePath;
    this.title = title;
    this.comp = comp;
    this.tip = tip;

    if (iconResourcePath != null) {
      try {
        icon = new ImageIcon(getClass().getResource(iconResourcePath));
      } catch (Exception e) {
        log.warn("Error loading icons", e);
      }
    }
  }

  public String getTitle() {
    return title;
  }

  public Icon getIcon() {
    return icon;
  }

  public Component getComponent() {
    return comp;
  }

  public String getTooltip() {
    return tip;
  }
}
