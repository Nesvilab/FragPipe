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

package org.nesvilab.utils;

import java.awt.Container;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GraphicsConfiguration;
import java.awt.GraphicsEnvironment;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.Window;

public final class ScreenUtils {
  // Suppress default constructor for noninstantiability
  private ScreenUtils() {
    throw new AssertionError();
  }

  /**
   * Calculates the available on-screen space inside a window/panel/component.
   *
   * @param container The window (JFrame)/JPanel/JComponent for which the available real estate should be calculated.
   *            If null is provided, calcs screen size minus the insets (like windows toolbar)
   * @return rectangle with origin (x,y) and window (width, height)
   */
  static public Rectangle getBounds(Container container) {
    Rectangle sb;
    Dimension sd;
    Insets si = getInsets(container);

    if (container == null) {
      sd = Toolkit.getDefaultToolkit().getScreenSize();
      sb = new Rectangle(new Point(0,0), sd);
    } else {
      sb = container.getBounds();
    }

    sb.x += si.left;
    sb.width -= (si.left + si.right);
    sb.y += si.top;
    sb.height -= (si.top + si.bottom);
    return sb;
  }

  /**
   * Gets the insets (like toolbars, borders, etc) for the current window.
   * You probably will never need this one.
   *
   * @param container The window (JFrame)/JPanel/JComponent for which to get the insets. If null, your desktop is assumed.
   * @return ScreenInsets (top, bottom, left, right..)
   */
  static public Insets getInsets(Container container) {
    Insets si;

    if (container == null) {
      si = Toolkit.getDefaultToolkit().getScreenInsets(new Frame().getGraphicsConfiguration());
    } else {
      si = container.getInsets();
    }

    return si;
  }


  /**
   * The insets of the screen, which are defined by any task bars
   * that have been set up by the user. This function accounts for multi-monitor setups. If a
   * window is supplied, then the the monitor that contains the window will be used. If a window
   * is not supplied, then the primary monitor will be used.
   */
  static public Insets getScreenInsets(Window windowOrNull) {
    Insets insets;
    if (windowOrNull == null) {
      insets = Toolkit.getDefaultToolkit().getScreenInsets(GraphicsEnvironment
          .getLocalGraphicsEnvironment().getDefaultScreenDevice()
          .getDefaultConfiguration());
    } else {
      insets = windowOrNull.getToolkit().getScreenInsets(
          windowOrNull.getGraphicsConfiguration());
    }
    return insets;
  }

  /**
   * getScreenWorkingArea, This returns the working area of the screen. (The working area excludes
   * any task bars.) This function accounts for multi-monitor setups. If a window is supplied,
   * then the the monitor that contains the window will be used. If a window is not supplied, then
   * the primary monitor will be used.
   */
  static public Rectangle getScreenWorkingArea(Window windowOrNull) {
    Insets insets;
    Rectangle bounds;
    if (windowOrNull == null) {
      GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
      insets = Toolkit.getDefaultToolkit().getScreenInsets(ge.getDefaultScreenDevice()
          .getDefaultConfiguration());
      bounds = ge.getDefaultScreenDevice().getDefaultConfiguration().getBounds();
    } else {
      GraphicsConfiguration gc = windowOrNull.getGraphicsConfiguration();
      insets = windowOrNull.getToolkit().getScreenInsets(gc);
      bounds = gc.getBounds();
    }
    bounds.x += insets.left;
    bounds.y += insets.top;
    bounds.width -= (insets.left + insets.right);
    bounds.height -= (insets.top + insets.bottom);
    return bounds;
  }

  /**
   * getScreenTotalArea, This returns the total area of the screen. (The total area includes any
   * task bars.) This function accounts for multi-monitor setups. If a window is supplied, then
   * the the monitor that contains the window will be used. If a window is not supplied, then the
   * primary monitor will be used.
   */
  static public Rectangle getScreenTotalArea(Window windowOrNull) {
    Rectangle bounds;
    if (windowOrNull == null) {
      GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
      bounds = ge.getDefaultScreenDevice().getDefaultConfiguration().getBounds();
    } else {
      GraphicsConfiguration gc = windowOrNull.getGraphicsConfiguration();
      bounds = gc.getBounds();
    }
    return bounds;
  }
}
