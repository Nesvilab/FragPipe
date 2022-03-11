package com.dmtavt.fragpipe.tabs;

import javax.swing.*;
import java.util.List;

/**
 * Use for cases when a UI component is created, but not shown on screen.
 * E.g. it's stored in a collection somewhere, but not currently attached
 * as a child to any UI container.
 * Originally used for saving/restoring values of UI tabs/panels that are
 * shown/hidden based on some radio button, dropdown or checkbox.
 */
@FunctionalInterface
public interface IExtraChildren {
    List<JComponent> getExtraChildComponents();
}
