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

package com.dmtavt.fragpipe.tools;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.messages.MessageDownloadProgress;
import com.github.chhh.utils.FileUtils;
import com.github.chhh.utils.SwingUtils;
import java.awt.Dimension;
import java.util.concurrent.TimeUnit;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.SwingUtilities;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.ThreadMode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import rx.Emitter.BackpressureMode;
import rx.Observable;
import rx.Subscription;

public class DownloadProgress {
  private static final Logger log = LoggerFactory.getLogger(DownloadProgress.class);
  private final JProgressBar progress;
  private final JFrame frame;
  public boolean isCancel = false;
  private On<MessageDownloadProgress> onEvent;
  private Observable<MessageDownloadProgress> obs;
  private Subscription sub;

  public DownloadProgress() {
    init();
    frame = new JFrame();
    Fragpipe.decorateFrame(frame);
    frame.setTitle("Downloading");
    frame.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
    frame.addWindowListener(new java.awt.event.WindowAdapter() {
      @Override
      public void windowClosing(java.awt.event.WindowEvent windowEvent) {
        setCancel(true);
      }
    });
    frame.setLayout(new MigLayout(new LC().fill()));

    JPanel content = new JPanel(new MigLayout(new LC().fillX()));

    JLabel label = new JLabel("Download progress");
    progress = new JProgressBar();
    progress.setIndeterminate(true);
    progress.setStringPainted(true);
    progress.setString("?");

    content.add(label, new CC().alignX("center").spanX().growX().wrap());
    content.add(progress, new CC().alignX("center").spanX().growX().wrap());

    frame.add(content, new CC().growX());
    frame.setMinimumSize(new Dimension(400, 50));
    frame.pack();
    SwingUtils.centerFrame(frame);
    frame.setVisible(true);
  }

  public void close() {
    if (frame != null) {
      frame.setVisible(false);
      frame.dispose();
    }
  }

  public synchronized boolean isCancel() {
    return isCancel;
  }

  public synchronized void setCancel(boolean cancel) {
    isCancel = cancel;
  }


  interface On<T> {
    void on(T m);
  }

  private void init() {
    obs = Observable.create(emitter -> {
          onEvent = emitter::onNext;
          emitter.setCancellation(() -> onEvent = null);
        }, BackpressureMode.LATEST);

    //sub = obs.subscribe(m -> {
    sub = obs.sample(500, TimeUnit.MILLISECONDS).subscribe(m -> {
      log.debug("Finally got message: {}", m);
      SwingUtilities.invokeLater(() -> {
        if (m.totalSize > 0) {
          String progeressText = String
              .format("%s / %s", FileUtils.fileSize(m.downloaded), FileUtils.fileSize(m.totalSize));
          progress.setString(progeressText);
          if (progress.isIndeterminate()) {
            progress.setIndeterminate(false);
          }
          progress.setMinimum(0);
          progress.setMaximum((int) m.totalSize);
          progress.setValue((int) m.downloaded);
        } else {
          progress.setString(String.format("%s / ?", FileUtils.fileSize(m.downloaded)));
          if (!progress.isIndeterminate()) {
            progress.setIndeterminate(true);
          }
        }
      });
    });
  }

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void on(MessageDownloadProgress m) {
    if (onEvent != null) {
      onEvent.on(m);
    }
  }
}
