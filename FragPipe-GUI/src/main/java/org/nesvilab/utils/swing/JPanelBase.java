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

package org.nesvilab.utils.swing;

import org.nesvilab.fragpipe.api.Bus;
import org.nesvilab.utils.StringUtils;
import org.nesvilab.utils.SwingUtils;
import java.awt.Component;
import java.awt.ItemSelectable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class JPanelBase extends JPanelWithEnablement {

  private static final Logger log = LoggerFactory.getLogger(JPanelBase.class);
  protected final MigUtils mu = MigUtils.get();

  public JPanelBase() {
    init();
    initMore();
  }

  protected abstract ItemSelectable getRunCheckbox();
  protected abstract Component getEnablementToggleComponent();
  protected abstract String getComponentNamePrefix();
  protected abstract boolean isRun();

  /** Override if the component itself doesn't need to be
   * published to the bus by initMore() method. */
  protected boolean doPostSelfAsSticky() {
    return true;
  }

  protected abstract void init();
  protected void initMore() {
    if (StringUtils.isNotBlank(getComponentNamePrefix())) {
      SwingUtils.renameDeep(this, false, getComponentNamePrefix(), null);
    }
    if (getRunCheckbox() != null && getEnablementToggleComponent() != null) {
      SwingUtils.setEnablementUpdater(this, getEnablementToggleComponent(), getRunCheckbox());
    }

    log.debug("Trying to register quietly on the bus: {}. (Caller: {})", this.getClass().getCanonicalName(), tryGetCallerForLog());
    Bus.registerQuietly(this);
    if (doPostSelfAsSticky()) {
      Bus.postSticky(this);
    }
  }

  public static String tryGetCallerForLog() {
    StackTraceElement[] trace = Thread.currentThread().getStackTrace();
    if (trace != null && trace.length > 2) {
      StackTraceElement t = trace[2];
      return String
          .join(", ", "Class: " + t.getClassName(), "Method: " + t.getMethodName(),
              "Line: " + t.getLineNumber());
    }
    return "N/A";
  }
}
