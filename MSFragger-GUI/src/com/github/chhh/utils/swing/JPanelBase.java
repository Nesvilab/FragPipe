package com.github.chhh.utils.swing;

import com.dmtavt.fragpipe.api.Bus;
import com.github.chhh.utils.StringUtils;
import com.github.chhh.utils.SwingUtils;
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
