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

package org.nesvilab.fragpipe;

import static org.nesvilab.fragpipe.FragpipeLocations.Holder.locations;
import static org.nesvilab.fragpipe.Version.PROGRAM_TITLE;

import org.nesvilab.fragpipe.api.Bus;
import org.nesvilab.fragpipe.api.Notifications;
import org.nesvilab.fragpipe.messages.MessageLoaderUpdate;
import org.nesvilab.fragpipe.messages.NoteFragpipeCache;
import org.nesvilab.fragpipe.messages.NoteFragpipeProperties;
import org.nesvilab.fragpipe.messages.NoteStartupComplete;
import org.nesvilab.fragpipe.params.ThisAppProps;
import org.nesvilab.utils.SwingUtils;
import io.reactivex.rxjava3.core.Observable;
import io.reactivex.rxjava3.disposables.Disposable;
import io.reactivex.rxjava3.schedulers.Schedulers;
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

public class FragpipeLoader {

  private static final Logger log = LoggerFactory.getLogger(FragpipeLoader.class);

  private final JProgressBar progress;
  private final JFrame frameLoading;

  public FragpipeLoader() {
    if (!Fragpipe.headless) {
      frameLoading = new JFrame();
      Fragpipe.decorateFrame(frameLoading);
      frameLoading.setTitle("Starting " + PROGRAM_TITLE);
      frameLoading.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
      frameLoading.setLayout(new BorderLayout());
      JPanel content = new JPanel(new MigLayout(new LC().fillX()));

      JLabel label = new JLabel("Initializing " + PROGRAM_TITLE);
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
    } else {
      frameLoading = null;
      progress = null;
    }
    initApplication();
  }

  private void initApplication() {
    Bus.post(new MessageLoaderUpdate("Loading classes"));
    log.debug("Loading BalloonTips class: {}", Notifications.class.getCanonicalName()); // do not remove, triggers static init
    ToolTipManager.sharedInstance().setDismissDelay(Integer.MAX_VALUE);
    Locale.setDefault(Locale.ROOT);

    final ExecutorService exec = Executors.newWorkStealingPool();
    final Duration timeoutMax = Duration.ofSeconds(1);

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
    Bus.post(new MessageLoaderUpdate("Starting " + PROGRAM_TITLE));

    log.debug("Closing loader frame");
    Bus.unregister(this);
    if (!Fragpipe.headless) {
      frameLoading.setVisible(false);
      frameLoading.dispose();
    }

    if (locations.getJarPath().toAbsolutePath().normalize().toString().contains(" ") || locations.getJarPath().toAbsolutePath().normalize().toString().contains("\t")) {
      SwingUtils.showErrorDialog(null, PROGRAM_TITLE + " cannot be run from a path that contains spaces or tabs. Please move the " + PROGRAM_TITLE + " folder to a path without spaces or tabs.", "Spaces in path");
      System.exit(1);
    }

    Fragpipe.displayMainWindow();
  }

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void on(MessageLoaderUpdate m) {
    if (!Fragpipe.headless) {
      log.debug("Updating loader progress: {}", m.text);
      progress.setString(m.text);
    }
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

      Disposable sub = obs.subscribe(
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
              log.debug("Falling back to using local properties file");
              local = ThisAppProps.getLocalProperties();
              if (local == null) {
                Bus.post(new MessageLoaderUpdate("Failed to load remote configuration"));
                throw new IllegalStateException("Loading local properties file failed. Please report to developers.");
              }
            } finally {
              Bus.postSticky(new NoteFragpipeProperties(local, false));
            }
          });
    };

  }
}
