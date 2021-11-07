package com.dmtavt.fragpipe;

import com.dmtavt.fragpipe.api.Bus;
import com.dmtavt.fragpipe.api.Notifications;
import com.dmtavt.fragpipe.messages.MessageLoaderUpdate;
import com.dmtavt.fragpipe.messages.NoteFragpipeCache;
import com.dmtavt.fragpipe.messages.NoteFragpipeProperties;
import com.dmtavt.fragpipe.messages.NoteStartupComplete;
import com.dmtavt.fragpipe.params.ThisAppProps;
import com.github.chhh.utils.SwingUtils;
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.time.Duration;
import java.util.Locale;
import java.util.Properties;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.ToolTipManager;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.ThreadMode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import rx.Observable;
import rx.Subscription;
import rx.schedulers.Schedulers;

public class FragpipeLoader {

  private static final Logger log = LoggerFactory.getLogger(FragpipeLoader.class);

  private final JProgressBar progress;
  private final JFrame frameLoading;

  public FragpipeLoader() {
    if (!Fragpipe.headless) {
      frameLoading = new JFrame();
      Fragpipe.decorateFrame(frameLoading);
      frameLoading.setTitle("Starting FragPipe");
      frameLoading.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
      frameLoading.setLayout(new BorderLayout());
    } else
      frameLoading = null;
    JPanel content = new JPanel(new MigLayout(new LC().fillX()));

    JLabel label = new JLabel("Initializing FragPipe");
    content.add(label, new CC().alignX("center").spanX().wrap());

    progress = new JProgressBar();
    progress.setIndeterminate(true);
    progress.setStringPainted(true);
    progress.setString("Initialization");
    content.add(progress, new CC().spanX().growX().wrap());
    if(!Fragpipe.headless) {
      frameLoading.add(content, BorderLayout.CENTER);
      frameLoading.setMinimumSize(new Dimension(400, 50));
      frameLoading.pack();
      SwingUtils.centerFrame(frameLoading);
      frameLoading.setVisible(true);
    }
    initApplication();
  }

  private void initApplication() {
    Bus.post(new MessageLoaderUpdate("Loading classes"));
    log.debug("Loading BalloonTips class: {}", Notifications.class.getCanonicalName()); // do not remove, triggers static init
    ToolTipManager.sharedInstance().setDismissDelay(Integer.MAX_VALUE);
    Locale.setDefault(Locale.ROOT);

    final ExecutorService exec = Executors.newWorkStealingPool();
    final Duration timeoutMax = Duration.ofSeconds(5);

    exec.submit(loadCache());
    exec.submit(loadRemoteProps(timeoutMax.getSeconds()));

    try {
      exec.shutdown();
      exec.awaitTermination(timeoutMax.getSeconds(), TimeUnit.SECONDS);
      NoteFragpipeProperties props = Bus.getStickyEvent(NoteFragpipeProperties.class);
      NoteFragpipeCache cache = Bus.getStickyEvent(NoteFragpipeCache.class);
      //Stream.of(props, cache).filter(Objects::isNull).forEach(o -> log.warn("All startup objects must be not null"));
      Bus.postSticky(new NoteStartupComplete(props, cache));
    } catch (InterruptedException e) {
      log.error("Error waiting for executor service shutdown in FragpipeLoader.initApplication()");
      SwingUtils.showErrorDialogWithStacktrace(e, frameLoading);
    }
  }

  @Subscribe(sticky = true, threadMode = ThreadMode.MAIN_ORDERED)
  public void on(NoteStartupComplete m) {
    log.debug(
        "Got NoteStartupComplete, which triggers main app start, loader window will close now");
    Bus.post(new MessageLoaderUpdate("Starting FragPipe"));

    log.debug("Closing loader frame");
    Bus.unregister(this);
    if (!Fragpipe.headless) {
      frameLoading.setVisible(false);
      frameLoading.dispose();
    }
    Fragpipe.displayMainWindow();
  }

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void on(MessageLoaderUpdate m) {
    log.debug("Updating loader progress: {}", m.text);
    progress.setString(m.text);
  }

  private static Runnable loadCache() {
    return () -> {
      Bus.post(new MessageLoaderUpdate("Checking cache"));
      try {
        log.debug("Loading cache");
        NoteFragpipeCache m = FragpipeLocations.get().loadCache();
        log.debug("Posting cache note");
        Bus.postSticky(m);
      } catch (Exception e) {
        log.error("Loading cache should not result in exceptions", e);
        throw new IllegalStateException("Loading cache should not result in exceptions", e);
      }
    };
  }

  private static Runnable loadRemoteProps(final long timeoutSeconds) {
    return () -> {
      log.info("Trying to fetch remote properties, {} seconds timeout", timeoutSeconds);
      Bus.post(new MessageLoaderUpdate("Trying to load remote configuration"));

      Observable<Properties> obs = Observable
          .fromCallable(ThisAppProps::getRemotePropertiesWithLocalDefaults)
          .timeout(timeoutSeconds, TimeUnit.SECONDS)
          .subscribeOn(Schedulers.io());

      Subscription sub = obs.subscribe(
          props -> {
            try {
              log.debug("Got remote properties successfully");
            } finally {
              Bus.postSticky(new NoteFragpipeProperties(props, true));
              Bus.post(new MessageLoaderUpdate("Loaded remote configuration"));
            }
          },
          ex -> {
            Properties local = null;
            Bus.post(new MessageLoaderUpdate("Failed to load remote configuration"));
            try {
              log.error("Something happened while trying to get application properties at startup", ex);
              log.debug("Falling back to using local properties file");
              local = ThisAppProps.getLocalProperties();
              if (local == null) {
                throw new IllegalStateException("Loading local properties file failed. Please report to developers.");
              }
            } finally {
              Bus.postSticky(new NoteFragpipeProperties(local, false));
            }
          });
    };

  }
}
