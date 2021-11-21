package com.dmtavt.fragpipe.api;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.exceptions.ValidationException;
import com.dmtavt.fragpipe.messages.MessageBalloon;
import com.dmtavt.fragpipe.messages.MessageShowException;
import com.dmtavt.fragpipe.messages.NoteConfigTips;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.swing.HtmlStyledJEditorPane;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.util.HashMap;
import java.util.Map;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import net.java.balloontip.BalloonTip;
import net.java.balloontip.styles.BalloonTipStyle;
import net.java.balloontip.styles.RoundedBalloonStyle;
import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.ThreadMode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Notifications {

  private static final Logger log = LoggerFactory.getLogger(Notifications.class);
  //  private static final BalloonTips INSTANCE;
  public static final Color BG_COLOR = Color.WHITE;
  public static final BalloonTipStyle STYLE = new RoundedBalloonStyle(5, 5, BG_COLOR, Color.BLACK);
  /** From component id to tip */
  final Map<String, BalloonTip> tips = new HashMap<>();

//  static {
//    log.debug("Setting singleton instance of BalloonTips");
//    INSTANCE = new BalloonTips();
//    Bus.register(INSTANCE);
//  }

  public Notifications() {
  }

  public static Notifications get() {
    NoteConfigTips m = Bus.getStickyEvent(NoteConfigTips.class);
    if (m.tips == null) {
      log.error("There was no NoteConfigTips among sticky events");
    }
    return m.tips;
  }

  public static void tryClose(String topic) {
    Bus.post(new MessageBalloon(topic));
  }

  public static void tryOpen(MessageBalloon m) {
    Bus.post(m);
  }

  @Subscribe(sticky = true, threadMode = ThreadMode.MAIN_ORDERED)
  public void on(MessageBalloon m) {
    log.debug("Got {}", m);
    synchronized (tips) {
      remove(m.topic); // always remove old balloon

      // create a new tip in one of three ways
      BalloonTip tip = null;
      if (m.tip != null) {
        tip = m.tip;

      } else if (m.parent != null && m.body != null) {
        if (Fragpipe.headless) {
          System.err.println(m.body);
        }
        tip = new BalloonTip(m.parent, m.body,
            new RoundedBalloonStyle(5, 5, BG_COLOR, Color.BLACK), true);

      } else if (m.parent != null && m.html != null) {
        if (Fragpipe.headless) {
          System.err.println(m.html);
        }
        HtmlStyledJEditorPane ep = SwingUtils.createClickableHtml(m.html, BG_COLOR);
//        JPanel p = new JPanel(new BorderLayout());
//        p.setBackground(ep.getBackground());
//        p.add(ep, BorderLayout.CENTER);

//        JPanel pBtns = new JPanel();
//        pBtns.setBackground(ep.getBackground());
//        pBtns.setLayout(new FlowLayout(FlowLayout.LEFT, 5, 5));
//        pBtns.add(btnClose);
//        p.add(pBtns, BorderLayout.SOUTH);

        tip = new BalloonTip(m.parent, ep, STYLE, true);

      } else {
        remove(m.topic);
      }

      if (tip != null) {
        tips.put(m.topic, tip);
        tip.setVisible(true);
      }
    }
  }

  private void remove(String topic) {
    BalloonTip tip = tips.remove(topic);
    if (tip == null) {
      return;
    }
    tip.closeBalloon();
    tip.setVisible(false);
  }

  /**
   * Shows {@link com.dmtavt.fragpipe.exceptions.ValidationException}s as balloon tips. All other
   * exception classes will be shown as error dialog.
   */
  public static void showException(String topic, JComponent comp, Throwable e,
      boolean showStacktrace) {
    if (e instanceof ValidationException) {
      log.debug("Got ValidationException, showing as balloon for topic {}", topic);
      Bus.post(new MessageBalloon(topic, comp, SwingUtils.makeHtml(e.getMessage())));
    } else {
      JScrollPane content = SwingUtils
          .createClickableHtmlInScroll(true, e.getMessage(), new Dimension(400, 50));
      SwingUtils.showErrorDialogWithStacktrace(e, comp, content, showStacktrace);
    }
  }

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void on(MessageShowException m) {
    showException(m.topic, m.comp, m.ex, m.showStacktrace);
  }
}
