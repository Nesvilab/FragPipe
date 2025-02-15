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

package org.nesvilab.fragpipe.api;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JComponent;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class UiTab {
  private static final Logger log = LoggerFactory.getLogger(UiTab.class);

  private Icon icon;
  private String iconResourcePath;
  private final String title;
  private final JComponent comp;
  private final String tip;
  private final boolean wrapTabInScroll;

  public UiTab(String title, JComponent comp, String iconResourcePath, String tip, boolean wrapTabInScroll) {
    this.iconResourcePath = iconResourcePath;
    this.title = title;
    this.comp = comp;
    this.tip = tip;
    this.wrapTabInScroll = wrapTabInScroll;

    if (iconResourcePath != null) {
      try {
        icon = new ImageIcon(getClass().getResource(iconResourcePath));
      } catch (Exception e) {
        log.warn("Error loading icons", e);
      }
    }
  }

  public UiTab(String title, JComponent comp, String iconResourcePath, String tip) {
    this(title, comp, iconResourcePath, tip, true);
  }

  public String getTitle() {
    return title;
  }

  public Icon getIcon() {
    return icon;
  }

  public JComponent getComponent() {
    return comp;
  }

  public String getTooltip() {
    return tip;
  }

  public boolean isWrapTabInScroll() {
    return wrapTabInScroll;
  }
}
