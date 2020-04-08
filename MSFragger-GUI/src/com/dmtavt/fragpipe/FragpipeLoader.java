package com.dmtavt.fragpipe;

import com.dmtavt.fragpipe.api.Notifications;
import com.dmtavt.fragpipe.api.Bus;
import com.dmtavt.fragpipe.api.FragpipeCacheUtils;
import com.dmtavt.fragpipe.messages.MessageLoadPreviousUiState;
import com.dmtavt.fragpipe.messages.MessageLoaderUpdate;
import com.dmtavt.fragpipe.messages.NoteFragpipeProperties;
import com.dmtavt.fragpipe.messages.NotePreviousUiState;
import com.dmtavt.fragpipe.messages.NoteStartupComplete;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.Tuple2;
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.time.Duration;
import java.time.Period;
import java.time.temporal.TemporalUnit;
import java.util.Locale;
import java.util.Objects;
import java.util.Properties;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.stream.Stream;
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
import umich.msfragger.params.ThisAppProps;

public class FragpipeLoader {

  private static final Logger log = LoggerFactory.getLogger(FragpipeLoader.class);

  private final JProgressBar progress;
  private final JFrame frameLoading;

  public FragpipeLoader() {
    frameLoading = new JFrame();
    Fragpipe.decorateFrame(frameLoading);
    frameLoading.setTitle("Starting FragPipe");
    frameLoading.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
    frameLoading.setLayout(new BorderLayout());

    JPanel content = new JPanel(new MigLayout(new LC().fillX()));

    JLabel label = new JLabel("Initializing FragPipe");
    content.add(label, new CC().alignX("center").spanX().wrap());

    progress = new JProgressBar();
    progress.setIndeterminate(true);
    progress.setStringPainted(true);
    progress.setString("Initialization");
    content.add(progress, new CC().spanX().growX().wrap());

    frameLoading.add(content, BorderLayout.CENTER);
    frameLoading.setMinimumSize(new Dimension(400, 50));
    frameLoading.pack();
    SwingUtils.centerFrame(frameLoading);
    frameLoading.setVisible(true);

    initApplication();
  }

  private void initApplication() {
    Bus.post(new MessageLoaderUpdate("Loading classes"));
    log.debug("Loading BalloonTips class: {}", Notifications.class.getCanonicalName()); // do not remove, triggers static init
    ToolTipManager.sharedInstance().setDismissDelay(Integer.MAX_VALUE);
    Locale.setDefault(Locale.ROOT);

    final ExecutorService exec = Executors.newWorkStealingPool();
    final Duration timeoutMax = Duration.ofSeconds(5);

    exec.submit(loadPreviousUiState());
    exec.submit(loadRemoteProps(Math.min(timeoutMax.getSeconds(), 3)));

    try {
      exec.shutdown();
      exec.awaitTermination(timeoutMax.getSeconds(), TimeUnit.SECONDS);
      NoteFragpipeProperties props = Bus.getStickyEvent(NoteFragpipeProperties.class);
      NotePreviousUiState uiState = Bus.getStickyEvent(NotePreviousUiState.class);
      Stream.of(props, uiState).filter(Objects::isNull).forEach(o -> log.error("All startup objects must be not null"));
      Bus.postSticky(new NoteStartupComplete(props, uiState));
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
    frameLoading.setVisible(false);
    frameLoading.dispose();
    Fragpipe.displayMainWindow();
  }

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void on(MessageLoaderUpdate m) {
    log.debug("Updating loader progress: {}", m.text);
    progress.setString(m.text);
  }

  private static Runnable loadPreviousUiState() {
    return () -> {
      Properties props = null;
      try {
        log.debug("Trying to check cache");
        Bus.post(new MessageLoaderUpdate("Checking cache"));
        MessageLoadPreviousUiState m = MessageLoadPreviousUiState.newForCache();
        log.debug("Fragpipe.Loader Loading ui state cache from: {}", m.path);
        try (InputStream is = Files.newInputStream(m.path)) {
          props = FragpipeCacheUtils.loadAsProperties(is);
          Bus.post(new MessageLoaderUpdate("Done checking cache"));
        } catch (IOException e) {
          log.error("Fragpipe.Loader Could not read fragpipe cache from: {}", m.path.toString());
          Bus.post(new MessageLoaderUpdate("Error while checking cache"));
        }
      } finally {
        Bus.postSticky(new NotePreviousUiState(props));
      }
    };
  }

  private static Runnable loadRemoteProps(final long timeoutSeconds) {
    return () -> {
      log.debug("Trying to fetch remote properties, {} seconds timeout", timeoutSeconds);
      Bus.post(new MessageLoaderUpdate("Trying to load remote configuration"));

      Observable<Properties> obs = Observable
          .fromCallable(ThisAppProps::getRemotePropertiesWithLocalDefaults)
          .timeout(timeoutSeconds, TimeUnit.SECONDS)
          .subscribeOn(Schedulers.immediate());

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
            try {
              log.error("Something happened while trying to get application properties at startup", ex);
              log.debug("Falling back to using local properties file");
              local = ThisAppProps.getLocalProperties();
              if (local == null) {
                throw new IllegalStateException(
                    "Loading local properties file failed. Please report to developers.");
              }
            } finally {
              Bus.postSticky(new NoteFragpipeProperties(local, false));
              Bus.post(new MessageLoaderUpdate("Failed to load remote configuration"));
            }
          });
    };

  }
}
