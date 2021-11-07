package com.dmtavt.fragpipe;

import javax.swing.JFrame;
import java.awt.event.WindowListener;

public class JFrameHeadless {
    private JFrame j;

    public JFrameHeadless(final boolean headless) {
        if (!headless)
            j = new JFrame();
    }

    public void addWindowListener(WindowListener l) {
        if (j != null)
            j.addWindowListener(l);
    }

    public JFrame toJFrame() {
        return j;
    }

}
