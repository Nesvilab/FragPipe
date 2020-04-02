package com.dmtavt.fragpipe;

import com.dmtavt.fragpipe.api.BalloonTips;
import com.dmtavt.fragpipe.api.Bus;
import com.dmtavt.fragpipe.api.FragpipeCacheUtils;
import com.dmtavt.fragpipe.messages.MessageLoadPreviousUiState;
import com.dmtavt.fragpipe.messages.MessageLoaderUpdate;
import com.dmtavt.fragpipe.messages.NoteFragpipeProperties;
import com.dmtavt.fragpipe.messages.NotePreviousUiState;
import com.dmtavt.fragpipe.messages.NoteStartupComplete;
import com.github.chhh.utils.SwingUtils;
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.util.Locale;
import java.util.Properties;
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
    log.debug("Loading BalloonTips class: {}", BalloonTips.class.getCanonicalName()); // do not remove, triggers static init
    ToolTipManager.sharedInstance().setDismissDelay(Integer.MAX_VALUE);
    Locale.setDefault(Locale.ROOT);

    loadPreviousUiState();
    loadRemoteProps(3);
  }

  @Subscribe(sticky = true, threadMode = ThreadMode.MAIN_ORDERED)
  public void onNoteStartupComplete(NoteStartupComplete m) {
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
  public void onLoaderUpdate(MessageLoaderUpdate m) {
    log.debug("Updating loader progress: {}", m.text);
    progress.setString(m.text);
  }

  @Subscribe(sticky = true, threadMode = ThreadMode.ASYNC)
  public void onNoteFragpipeProperties(NoteFragpipeProperties m) {
    log.debug("Fragpipe Properties file loaded");
    NotePreviousUiState lastUiState = Bus.getStickyEvent(NotePreviousUiState.class);
    // trigger main window start
    Bus.postSticky(new NoteStartupComplete(lastUiState == null ? new Properties() : lastUiState.props, m.props));
  }

  private void loadPreviousUiState() {
    log.debug("Trying to check cache");
    Bus.post(new MessageLoaderUpdate("Checking cache"));
    MessageLoadPreviousUiState m = MessageLoadPreviousUiState.newForCache();
    log.debug("Fragpipe.Loader Loading ui state cache from: {}", m.path);
    try (InputStream is = Files.newInputStream(m.path)) {
      Properties props = FragpipeCacheUtils.loadAsProperties(is);
      Bus.postSticky(new NotePreviousUiState(props));
    } catch (IOException e) {
      log.error("Fragpipe.Loader Could not read fragpipe cache from: {}", m.path.toString());
    }
  }

  private static void loadRemoteProps(long timeoutSeconds) {
    log.debug("Trying to fetch remote properties, {} seconds timeout", timeoutSeconds);
    Bus.post(new MessageLoaderUpdate("Trying to load remote configuration"));

    Observable<Properties> obs = Observable
        .fromCallable(ThisAppProps::getRemotePropertiesWithLocalDefaults)
        .timeout(timeoutSeconds, TimeUnit.SECONDS)
        .subscribeOn(Schedulers.immediate());

    obs.subscribe(
        props -> {
          log.debug("Got remote properties successfully");
          Bus.postSticky(new NoteFragpipeProperties(props, true));
        },
        ex -> {
          log.error("Something happened while trying to get application properties at startup", ex);
          Bus.post(new MessageLoaderUpdate("Failed to load remote configuration"));
          log.debug("Falling back to using local properties file");
          Properties local = ThisAppProps.getLocalProperties();
          if (local == null) {
            throw new IllegalStateException(
                "Loading local properties file failed. Please report to developers.");
          }
          Bus.postSticky(new NoteFragpipeProperties(local, false));
        });
  }
}
