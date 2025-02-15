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

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.util.HashMap;
import java.util.Map;
import javax.swing.JComponent;
import javax.swing.JScrollPane;
import net.java.balloontip.BalloonTip;
import net.java.balloontip.styles.BalloonTipStyle;
import net.java.balloontip.styles.RoundedBalloonStyle;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.ThreadMode;
import org.nesvilab.fragpipe.Fragpipe;
import org.nesvilab.fragpipe.exceptions.ValidationException;
import org.nesvilab.fragpipe.messages.MessageBalloon;
import org.nesvilab.fragpipe.messages.MessageShowException;
import org.nesvilab.fragpipe.messages.NoteConfigTips;
import org.nesvilab.utils.SwingUtils;
import org.nesvilab.utils.swing.HtmlStyledJEditorPane;
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
          String s = null;
          try {
            if (m.body instanceof HtmlStyledJEditorPane) {
              s = ((HtmlStyledJEditorPane) m.body).getTextLessHtml();
            } else {
              for (Component tt : m.body.getComponents()) {
                if (tt instanceof HtmlStyledJEditorPane) {
                  s = ((HtmlStyledJEditorPane) tt).getTextLessHtml();
                  break;
                }
              }
            }
          } catch (Exception ignored) {
            log.error("Cannot get error message");
            log.error(ExceptionUtils.getStackTrace(ignored));
          }
          if (m.exitHeadless) {
            log.error(s);
            System.exit(1);
          } else {
            log.warn(s);
          }
        } else {
          tip = new BalloonTip(m.parent, m.body, new RoundedBalloonStyle(5, 5, BG_COLOR, Color.BLACK), true);
        }
      } else if (m.parent != null && m.html != null) {
        HtmlStyledJEditorPane ep = SwingUtils.createClickableHtml(m.html, BG_COLOR);
        if (Fragpipe.headless) {
          if (m.exitHeadless) {
            log.error(ep.getTextLessHtml());
            System.exit(1);
          } else {
            log.warn(ep.getTextLessHtml());
          }
        } else {
          tip = new BalloonTip(m.parent, ep, STYLE, true);
        }
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
   * Shows {@link ValidationException}s as balloon tips. All other
   * exception classes will be shown as error dialog.
   */
  public static void showException(String topic, JComponent comp, Throwable e,
      boolean showStacktrace) {
    if (e instanceof ValidationException) {
      log.debug("Got ValidationException, showing as balloon for topic {}", topic);
      Bus.post(new MessageBalloon(topic, comp, SwingUtils.makeHtml(e.getMessage()), true));
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
