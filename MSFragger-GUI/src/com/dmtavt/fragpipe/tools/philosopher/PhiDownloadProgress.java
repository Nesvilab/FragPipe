package com.dmtavt.fragpipe.tools.philosopher;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.messages.MessagePhiDlProgress;
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
import rx.Emitter;
import rx.Emitter.BackpressureMode;
import rx.Observable;
import rx.Observable.OnSubscribe;
import rx.Scheduler;
import rx.Subscriber;
import rx.Subscription;
import rx.functions.Cancellable;
import rx.observables.SyncOnSubscribe;
import rx.schedulers.Schedulers;

public class PhiDownloadProgress {
  private static final Logger log = LoggerFactory.getLogger(PhiDownloadProgress.class);
  private final JProgressBar progress;
  private final JFrame frame;
  public boolean isCancel = false;
  private On<MessagePhiDlProgress> onEvent;
  private Observable<MessagePhiDlProgress> obs;
  private Subscription sub;

  public PhiDownloadProgress() {
    init();
    frame = new JFrame();
    Fragpipe.decorateFrame(frame);
    frame.setTitle("Downloading Philosopher");
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
      log.warn("Finally got message: {}", m);
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
  public void on(MessagePhiDlProgress m) {
    if (onEvent != null) {
      onEvent.on(m);
    }
  }
}
