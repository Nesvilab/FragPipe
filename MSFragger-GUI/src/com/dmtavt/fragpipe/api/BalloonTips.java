package com.dmtavt.fragpipe.api;

import com.dmtavt.fragpipe.messages.MessageBalloon;
import com.github.chhh.utils.StringUtils;
import com.github.chhh.utils.SwingUtils;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.FlowLayout;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.HashMap;
import java.util.Map;
import javax.swing.JButton;
import javax.swing.JEditorPane;
import javax.swing.JPanel;
import net.java.balloontip.BalloonTip;
import net.java.balloontip.styles.BalloonTipStyle;
import net.java.balloontip.styles.RoundedBalloonStyle;
import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.ThreadMode;

public class BalloonTips {
  public static final Color BG_COLOR = Color.WHITE;
  public static final BalloonTipStyle STYLE = new RoundedBalloonStyle(5, 5, BG_COLOR, Color.BLACK);
  /** From component id to tip */
  Map<String, BalloonTip> tips = new HashMap<>();

  public BalloonTips() {
    Bus.register(this);
  }

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void onMessageBalloon(MessageBalloon m) {
    synchronized (tips) {
      remove(m); // always remove old balloon

      // create a new tip in one of three ways
      BalloonTip tip = null;
      if (m.tip != null) {
        tip = m.tip;

      } else if (m.parent != null && m.body != null) {
        tip = new BalloonTip(m.parent, m.body,
            new RoundedBalloonStyle(5, 5, BG_COLOR, Color.BLACK), true);

      } else if (m.parent != null && m.html != null) {
        JEditorPane ep = SwingUtils.createClickableHtml(m.html, BG_COLOR);
        JPanel p = new JPanel(new BorderLayout());
        p.setBackground(ep.getBackground());
        p.add(ep, BorderLayout.CENTER);

//        JPanel pBtns = new JPanel();
//        pBtns.setBackground(ep.getBackground());
//        pBtns.setLayout(new FlowLayout(FlowLayout.LEFT, 5, 5));
//        pBtns.add(btnClose);
//        p.add(pBtns, BorderLayout.SOUTH);

        tip = new BalloonTip(m.parent, p, STYLE, true);

      } else {
        throw new IllegalStateException("Must either provide a Tip, a JComponent for tip, or HTML body");
      }

      if (tip != null) {
        tips.put(m.topic, tip);
        tip.setVisible(true);
      }
    }
  }

  private void remove(MessageBalloon m) {
    BalloonTip tip = tips.remove(m.topic);
    if (tip == null)
      return;
    tip.closeBalloon();
    tip.setVisible(false);
  }
}
