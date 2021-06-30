package com.dmtavt.fragpipe.tabs;

import static com.dmtavt.fragpipe.Fragpipe.fe;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.FragpipeLocations;
import com.dmtavt.fragpipe.api.Bus;
import com.dmtavt.fragpipe.api.Notifications;
import com.dmtavt.fragpipe.api.PyInfo;
import com.dmtavt.fragpipe.dialogs.DownloadMSFraggerPanel;
import com.dmtavt.fragpipe.exceptions.UnexpectedException;
import com.dmtavt.fragpipe.exceptions.ValidationException;
import com.dmtavt.fragpipe.messages.MessageBalloon;
import com.dmtavt.fragpipe.messages.MessageClearCache;
import com.dmtavt.fragpipe.messages.MessageDbNewPath;
import com.dmtavt.fragpipe.messages.MessageFindSystemPython;
import com.dmtavt.fragpipe.messages.MessageInstallEasyPQP;
import com.dmtavt.fragpipe.messages.MessageLcmsAddFolder;
import com.dmtavt.fragpipe.messages.MessageMsfraggerNewBin;
import com.dmtavt.fragpipe.messages.MessageMsfraggerUpdateAvailable;
import com.dmtavt.fragpipe.messages.MessagePhilosopherNewBin;
import com.dmtavt.fragpipe.messages.MessagePythonNewBin;
import com.dmtavt.fragpipe.messages.MessageShowAboutDialog;
import com.dmtavt.fragpipe.messages.MessageUiRevalidate;
import com.dmtavt.fragpipe.messages.NoteConfigDbsplit;
import com.dmtavt.fragpipe.messages.NoteConfigMsfragger;
import com.dmtavt.fragpipe.messages.NoteConfigPhilosopher;
import com.dmtavt.fragpipe.messages.NoteConfigPython;
import com.dmtavt.fragpipe.messages.NoteConfigSpeclibgen;
import com.dmtavt.fragpipe.messages.NoteFragpipeUpdate;
import com.dmtavt.fragpipe.params.ThisAppProps;
import com.dmtavt.fragpipe.tools.fragger.Msfragger;
import com.dmtavt.fragpipe.tools.fragger.Msfragger.Version;
import com.dmtavt.fragpipe.tools.fragger.MsfraggerVersionFetcherServer;
import com.dmtavt.fragpipe.tools.philosopher.Philosopher;
import com.dmtavt.fragpipe.tools.philosopher.Philosopher.UpdateInfo;
import com.dmtavt.fragpipe.util.GitHubJson;
import com.github.chhh.utils.JarUtils;
import com.github.chhh.utils.OsUtils;
import com.github.chhh.utils.PathUtils;
import com.github.chhh.utils.ProcessUtils;
import com.github.chhh.utils.StringUtils;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.VersionComparator;
import com.github.chhh.utils.swing.ContentChangedFocusAdapter;
import com.github.chhh.utils.swing.FileChooserUtils;
import com.github.chhh.utils.swing.FileChooserUtils.FcMode;
import com.github.chhh.utils.swing.FormEntry;
import com.github.chhh.utils.swing.HtmlStyledJEditorPane;
import com.github.chhh.utils.swing.JPanelWithEnablement;
import com.github.chhh.utils.swing.MigUtils;
import com.github.chhh.utils.swing.UiText;
import com.github.chhh.utils.swing.UiUtils;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.reflect.TypeToken;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.regex.Pattern;
import java.util.stream.Stream;
import javax.imageio.ImageIO;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JEditorPane;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.border.TitledBorder;
import javax.swing.filechooser.FileNameExtensionFilter;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.ThreadMode;
import org.jooq.lambda.Seq;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import rx.Observable;
import rx.schedulers.Schedulers;

public class TabConfig extends JPanelWithEnablement {

  private static final Logger log = LoggerFactory.getLogger(TabConfig.class);
  private static final String msfraggerMinVersion = "3.2";
  private static final MigUtils mu = MigUtils.get();
  private static final Pattern emailPattern = Pattern.compile("(?:[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*|\"(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21\\x23-\\x5b\\x5d-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])*\")@(?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\\[(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?|[a-z0-9-]*[a-z0-9]:(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21-\\x5a\\x53-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])+)\\])");

  private UiText uiTextBinFragger;
  private HtmlStyledJEditorPane epFraggerVer;
  private UiText uiTextBinPhi;
  private HtmlStyledJEditorPane epPhiVer;
  private UiText uiTextBinPython;
  private HtmlStyledJEditorPane epPythonVer;
  private HtmlStyledJEditorPane epDbsplitText;
  private HtmlStyledJEditorPane epDbsplitErr;
  private Container epDbsplitErrParent;
  private HtmlStyledJEditorPane epEasyPQPText;
  private HtmlStyledJEditorPane epSpectraSTText;
  private HtmlStyledJEditorPane epSpeclibgenErr;
  private Container epSpeclibgenErrParent;
  private JButton btnAbout;
  public static final String TIP_MSFRAGGER_BIN = "tip.msfragger.bin";
  public static final String TIP_PHILOSOPHER_BIN = "tip.pholosopher.bin";
  public static final String TIP_PYTHON_BIN = "tip.python.bin";
  private static final String TIP_DBSPLIT = "tip.dbsplit";
  private static final String TIP_SPECLIBGEN = "tip.speclibgen";
  private static final String TIP_FRAGPIPE_UPDATE = "tip.fragpipe.update";
  public static final String TAB_PREFIX = "fragpipe-config.";


  public TabConfig() {
    init();
    initMore();
  }

  private void initMore() {
    Bus.register(this);
  }

  private void init() {
    this.setLayout(new MigLayout(new LC().fillX()));
    add(createPanelTopButtons(), new CC().growX().wrap());
    add(createPanelFragger(), new CC().growX().wrap());
    add(createPanelPhilosopher(), new CC().growX().wrap());
    add(createPanelPython(), new CC().growX().wrap());
    add(createPanelDbsplit(), new CC().growX().wrap());
    add(createPanelSpeclibgen(), new CC().growX().wrap());
    add(createPanelBottomInfo(), new CC().growX().wrap());
    add(createPanelBottomLink(), new CC().growX().wrap());
  }

  private JLabel createPanelBottomInfo() {
    JLabel c = new JLabel();
    c.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
    c.setAlignmentX(Component.CENTER_ALIGNMENT);
    c.setText(SwingUtils.makeHtml(
        "Tabs at the top represent processing steps that will be performed sequentially if enabled.\n"
            + "Load a workflow from the dropdown menu on the 'Workflow' tab to get started."));
    return c;
  }

  private JPanel createPanelBottomLink() {
    Properties props = ThisAppProps.getRemotePropertiesWithLocalDefaults();
//      Properties props = ThisAppProps.getLocalProperties(); // for testing
    String linkUrl = props.getProperty(ThisAppProps.PROP_SETUP_TUTORIAL_URL,
        "https://msfragger.nesvilab.org/tutorial_setup_fragpipe.html");

    JEditorPane c = SwingUtils.createClickableHtml(
        "<a href='" + linkUrl + "'>Configuration Help</a>");
    c.setAlignmentX(Component.CENTER_ALIGNMENT);
    JPanel p = new JPanel();
    p.setAlignmentX(Component.CENTER_ALIGNMENT);
    p.add(c);

    return p;
  }

  private JPanel createPanelTopButtons() {
    JPanel p = newMigPanel();
    btnAbout = UiUtils.createButton("About", e -> Bus.post(new MessageShowAboutDialog()));
    JButton btnOpenCacheInExplorer = SwingUtils
        .createButtonOpenInFileManager(this, "Open cache location",
            () -> FragpipeLocations.get().getPathLongTermStorage());
    mu.add(p, btnAbout).split().spanX();
    mu.add(p, btnOpenCacheInExplorer);
    mu.add(p, UiUtils.createButton("Clear cache and close", e -> Bus.post(new MessageClearCache(true))));

    if (com.dmtavt.fragpipe.Version.isDevBuild() || log.isDebugEnabled()) {
      mu.add(p, UiUtils.createButton("Debug button", e -> {
        Bus.post(new MessageMsfraggerNewBin("C:\\Users\\chhh\\lib\\msfragger\\MSFragger-2.4\\MSFragger-2.4.jar"));
        Bus.post(new MessagePhilosopherNewBin("C:\\Users\\chhh\\lib\\philosopher\\philosopher_v3.2.3_windows_amd64\\philosopher.exe"));
        Bus.post(new MessageDbNewPath("D:\\ms-data\\fasta\\2019-09-26-td-RefSeq.20180629_Human_ucsc_hg38_cpdbnr_mito_264contams.fasta"));
        Bus.post(new MessageLcmsAddFolder(Seq.of("D:\\ms-data\\TMTIntegrator_v1.1.4\\TMT-I-Test\\tmti-test-data_5-min-cuts").map(Paths::get).toList()));
//        log.debug("Debugging python environment vars");
//        NoteConfigPython configPython = Fragpipe.getStickyStrict(NoteConfigPython.class);
//        PyInfo.modifyEnvironmentVariablesForPythonSubprocesses(configPython.pi.getCommand(), new HashMap<>());



      }));
    }
    
    JLabel sysInfo = new JLabel(SwingUtils.makeHtml(
        OsUtils.OsInfo() + "\n"
            + OsUtils.JavaInfo()
            + "\nFragPipe: v" + com.dmtavt.fragpipe.Version.version(false)));
            //+ FragpipeLocations.get().getJarPath().toString()));
    sysInfo.setVerticalAlignment(JLabel.TOP);
    //p.add(UiUtils.createButton("Find tools", e -> post(new MessageFindTools())), ccL.get().split().spanX());

    try {
      BufferedImage image = ImageIO.read(getClass().getResource("/com/dmtavt/fragpipe/icons/fragpipe-128.png"));
      JLabel imageLabel = new JLabel(new ImageIcon(image));
      p.add(sysInfo, ccR().growX());
      p.add(imageLabel, ccL().wrap());
    } catch (Exception ex) {
      ex.printStackTrace();
      p.add(sysInfo, ccR().wrap());
    }

    JLabel label = new JLabel("Main tools configuration");
    label.setFont(new Font(label.getFont().getName(), Font.BOLD, label.getFont().getSize() + 3));
    p.add(label, ccL().wrap());

    JEditorPane ep = SwingUtils.createClickableHtml(createOthersCitationBody());
    p.add(ep, ccL().spanX().growX().wrap());
    return p;
  }

  private JPanel createPanelFragger() {
    JPanel p = newMigPanel();
    p.setBorder(new TitledBorder("MSFragger"));

    final String binMsfraggerTip = "Select path to MSFragger.jar";
    uiTextBinFragger = UiUtils.uiTextBuilder().create();
    uiTextBinFragger.addFocusListener(new ContentChangedFocusAdapter(uiTextBinFragger, (s, s2) -> {
      Bus.post(new MessageMsfraggerNewBin(s2));
    }));
    FormEntry feBinMsfragger = fe(uiTextBinFragger, "bin-msfragger", TAB_PREFIX)
        .tooltip(binMsfraggerTip).create();
    p.add(feBinMsfragger.comp, ccL().split().growX());

    JButton btnBrowse = feBinMsfragger
        .browseButton("Browse", binMsfraggerTip, this::createFraggerFilechooser, paths -> {
          paths.stream().findFirst()
              .ifPresent(jar -> Bus.post(new MessageMsfraggerNewBin(jar.toString())));
        });
    p.add(btnBrowse, ccL());
    JButton btnUpdate = UiUtils.createButton("Download / Update", this::actionMsfraggerUpdate);

    btnUpdate.setToolTipText(SwingUtils.makeHtml("Open MSFragger upgrader tool in browser.\n" +
        "In order to update you <b>must</b> download an\n" +
        "original copy from the <b>download</b> website once."));
    epFraggerVer = new HtmlStyledJEditorPane("MSFragger version: N/A");
    p.add(btnUpdate, ccL().wrap());
    p.add(Fragpipe.renameNoCache(epFraggerVer, "msfragger.version-info", TAB_PREFIX), ccL().split());
    
    JEditorPane ep = SwingUtils.createClickableHtml(createFraggerCitationBody());
    p.add(ep, ccL().spanX().growX().wrap());
    return p;
  }

  @Subscribe(sticky = true, threadMode = ThreadMode.MAIN_ORDERED)
  public void on(NoteFragpipeUpdate m) {
    log.debug("Got NoteFragpipeUpdate: {}", m.toString());
    StringBuilder sb = new StringBuilder();
    if (StringUtils.isNotBlank(m.releaseVer)) {
      sb.append(String.format("FragPipe update available, new version %s\n", m.releaseVer));
    }
    if (StringUtils.isNotBlank(m.downloadUrl)) {
      sb.append(String.format("<a href=\"%s\">Click here to download</a>\n", m.downloadUrl));
    }
    if (StringUtils.isNotBlank(m.announcement)) {
      if (sb.length() != 0)
        sb.append("\n");
      sb.append(m.announcement).append("\n");
    }
    if (sb.length() == 0) {
      log.warn("Received NoteFragpipeUpdate message, but did not compose any user notification out of it.");
    } else {
      HtmlStyledJEditorPane ep = SwingUtils.createClickableHtml(true, sb.toString());
      ep.setBackground(Color.white);
      Bus.postSticky(new MessageBalloon(TIP_FRAGPIPE_UPDATE, btnAbout, ep));
    }
  }

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void on(MessageMsfraggerUpdateAvailable m) {
    StringBuilder sb = new StringBuilder();
    sb.append(
        String.format("There is a newer version of MSFragger available [%s].<br>\n", m.newVersion));

    JEditorPane ep = SwingUtils.createClickableHtml(sb.toString(), Notifications.BG_COLOR);
    JPanel content = new JPanel(new BorderLayout());
    content.setBackground(ep.getBackground());

    JPanel pBtns = new JPanel();
    pBtns.setBackground(ep.getBackground());
    pBtns.setLayout(new FlowLayout(FlowLayout.LEFT, 5, 5));

    if (!StringUtils.isNullOrWhitespace(m.manualDownloadUrl)) {
      JButton btnManualUpdate = new JButton("Download update");
      btnManualUpdate.addActionListener(
        this::actionMsfraggerUpdate
      );
      pBtns.add(btnManualUpdate);
    }

    JButton btnClose = new JButton("Close");
    btnClose.addActionListener(e -> {
      Bus.post(new MessageBalloon(TIP_MSFRAGGER_BIN));
    });

    content.add(ep, BorderLayout.CENTER);
    pBtns.add(btnClose);
    content.add(pBtns, BorderLayout.SOUTH);

    Bus.post(new MessageBalloon(TIP_MSFRAGGER_BIN, uiTextBinFragger, content));
  }

  private CC ccL() {
    return new CC().alignX("left");
  }

  private CC ccR() {
    return new CC().alignX("right");
  }

  private JPanel newMigPanel() {
    return new JPanel(new MigLayout(new LC().fillX()));
  }

  private void actionMsfraggerUpdate(ActionEvent evt) {
    try {
      DownloadMSFraggerPanel p = new DownloadMSFraggerPanel();
      int confirmation = JOptionPane.showConfirmDialog(this, p, "Please agree to the terms of the licenses.", JOptionPane.YES_NO_CANCEL_OPTION);
      if (JOptionPane.OK_OPTION == confirmation) {
        if (p.getName() == null || p.getName().isEmpty()) {
          JOptionPane.showMessageDialog(this, "Please fill in your name.", "Error", JOptionPane.ERROR_MESSAGE);
          return;
        }
        if (p.getEmail() == null || p.getEmail().isEmpty() || !emailPattern.matcher(p.getEmail()).matches()) {
          JOptionPane.showMessageDialog(this, "Please fill in an valid email.", "Error", JOptionPane.ERROR_MESSAGE);
          return;
        }
        if (p.getInstitution() == null || p.getInstitution().isEmpty()) {
          JOptionPane.showMessageDialog(this, "Please fill in your institution.", "Error", JOptionPane.ERROR_MESSAGE);
          return;
        }
        if (!p.licensesChecked()) {
          JOptionPane.showMessageDialog(this, "Please read and check all of the licenses.", "Error", JOptionPane.ERROR_MESSAGE);
          return;
        }

        Path toolsPath = PathUtils.createDirs(FragpipeLocations.get().getDirTools());
        MsfraggerVersionFetcherServer msfraggerVersionFetcherServer = new MsfraggerVersionFetcherServer(p.getName(), p.getEmail(), p.getInstitution());
        new Thread(() -> {
          try {
            Path msfraggerPath = msfraggerVersionFetcherServer.autoUpdate(toolsPath);
            if (msfraggerPath != null) {
              Bus.post(new MessageMsfraggerNewBin(msfraggerPath.toAbsolutePath().toString()));
            } else {
              Bus.postSticky(new NoteConfigMsfragger("N/A", "N/A", null));
            }
          } catch (Exception ex) {
            Bus.postSticky(new NoteConfigMsfragger("N/A", "N/A", ex));
          }
        }).start();
      }
    } catch (Exception ex) {
      Bus.postSticky(new NoteConfigMsfragger("N/A", "N/A", ex));
    }
  }

  private JFileChooser createFraggerFilechooser() {
    final FileNameExtensionFilter fileNameExtensionFilter = new FileNameExtensionFilter("JAR files", "jar");
    JFileChooser fc = FileChooserUtils.create("Select MSFragger jar", "Select",
        false, FcMode.FILES_ONLY, true,
        fileNameExtensionFilter);
    fc.setFileFilter(fileNameExtensionFilter);
    FileChooserUtils.setPath(fc, Stream.of(
        uiTextBinFragger.getNonGhostText(),
        Fragpipe.propsVarGet(ThisAppProps.PROP_BINARIES_IN),
        FragpipeLocations.get().getDirFragpipeRoot().toString()));
    return fc;
  }

  @Subscribe(threadMode = ThreadMode.ASYNC)
  public void on(MessagePhilosopherNewBin m) {
    if (StringUtils.isBlank(m.path)) {
      Bus.postSticky(new NoteConfigPhilosopher(null, "N/A"));
      return;
    }
    try {
      Philosopher.Version v = Philosopher.validate(m.path);
      String version = v.version + (StringUtils.isBlank(v.build) ? "" : " (build " + v.build + ")");
      Bus.postSticky(new NoteConfigPhilosopher(m.path, version));
    } catch (ValidationException | UnexpectedException e) {
      Bus.postSticky(new NoteConfigPhilosopher(m.path, "N/A", e));
    }
  }

  @Subscribe(sticky = true, threadMode = ThreadMode.MAIN_ORDERED)
  public void on(NoteConfigPhilosopher m) {
    log.debug("Got {}", m);
    uiTextBinPhi.setText(m.path);

    Path existing = PathUtils.existing(m.path);
    if (existing != null) {
      Fragpipe.propsVarSet(ThisAppProps.PROP_BINARIES_IN, existing.toString());
      if (existing.toString().contains(" ")) {
        SwingUtils.showWarningDialog(this, "There are spaces in Philosopher path.\n"
            + "Depending on your OS/Java/Input files some tools that FragPipe\n"
            + "runs might crash.", "Spaces in path");
      }
    }

    if (m.ex != null) {
      epPhiVer.setText("Philosopher version: N/A");
      showConfigError(m.ex, TIP_PHILOSOPHER_BIN, uiTextBinPhi);
    } else {
      epPhiVer.setText("Philosopher version: " + m.version);
      Notifications.tryClose(TIP_PHILOSOPHER_BIN);
      checkPhilosopherUpdateAsync(m.path);
    }
  }

  private void checkPhilosopherUpdateAsync(String path) {
    Observable<UpdateInfo> obs = Observable
        .fromCallable(() -> Philosopher.checkUpdates(path))
        .subscribeOn(Schedulers.io());
    obs.subscribe(info -> {
      log.debug("Got philosopher update info with updateAvailable={}", info.isUpdateAvailable);
      if (info.isUpdateAvailable) {
        MessageBalloon tip = new MessageBalloon(TIP_PHILOSOPHER_BIN, uiTextBinPhi,
            SwingUtils.makeHtml("Philosopher update available."));
        Notifications.tryOpen(tip);
      }
    }, throwable -> {
      log.warn("Something happened when checking for philosopher updates", throwable);
    });
  }

  @Subscribe(threadMode = ThreadMode.ASYNC)
  public void on(MessageMsfraggerNewBin m) {
    if (StringUtils.isBlank(m.binPath)) {
      return;
    }
    Version v;
    try {
      Msfragger.validateJar(m.binPath);
      v = Msfragger.version(Paths.get(m.binPath));
      if (v.isVersionParsed) {
        if (v.version.compareTo(msfraggerMinVersion) >= 0) {
          Bus.postSticky(new NoteConfigMsfragger(m.binPath, v.version));
        } else {
          Bus.postSticky(new NoteConfigMsfragger(m.binPath, v.version, true, null));
        }
      } else {
        Bus.postSticky(new NoteConfigMsfragger(m.binPath, "N/A", null));
      }
    } catch (ValidationException | UnexpectedException e) {
      Bus.postSticky(new NoteConfigMsfragger(m.binPath, "N/A", e));
    }
  }

  @Subscribe(sticky = true, threadMode = ThreadMode.MAIN_ORDERED)
  public void on(NoteConfigMsfragger m) {
    log.debug("Got {}", m);
    uiTextBinFragger.setText(m.path);

    Path existing = PathUtils.existing(m.path);
    if (existing != null) {
      Fragpipe.propsVarSet(ThisAppProps.PROP_BINARIES_IN, existing.toString());
      if (existing.toString().contains(" ")) {
        SwingUtils.showWarningDialog(this, "There are spaces in MSFragger path.\n"
            + "Depending on your OS/Java/Input files some tools that FragPipe\n"
            + "runs might crash.", "Spaces in path");
      }
    }

    if (m.ex != null) {
      epFraggerVer.setText("MSFragger version: N/A");
      showConfigError(m.ex, TIP_MSFRAGGER_BIN, uiTextBinFragger);
    } else if (m.isTooOld) {
      epFraggerVer.setText("MSFragger version: too old, not supported anymore");
      Bus.post(new MessageBalloon(TIP_MSFRAGGER_BIN, uiTextBinFragger,
          "MSFragger " + msfraggerMinVersion + " is required."));
    } else {
      epFraggerVer.setText("MSFragger version: " + m.version);
      Notifications.tryClose(TIP_MSFRAGGER_BIN);
    }
    if (m.isValid() && !m.isTooOld) {
      Msfragger.checkUpdates(m);
    }
  }

  @Subscribe
  public void on(MessageUiRevalidate m) {
    log.debug("Got MessageUiRevalidate");
    String binFragger = uiTextBinFragger.getNonGhostText();
    if (StringUtils.isNotBlank(binFragger)) {
      Bus.post(new MessageMsfraggerNewBin(binFragger));
    }
    String binPhi = uiTextBinPhi.getNonGhostText();
    if (StringUtils.isNotBlank(binPhi)) {
      Bus.post(new MessagePhilosopherNewBin(binPhi));
    }
    String binPython = uiTextBinPython.getNonGhostText();
    if (StringUtils.isNotBlank(binPython)) {
      Bus.post(new MessagePythonNewBin(binPython));
    } else {
      // python was not loaded, try finding system python
      Bus.post(new MessageFindSystemPython());
    }
  }

  @Subscribe(threadMode = ThreadMode.ASYNC)
  public void on(MessageFindSystemPython m) {
    try {
      PyInfo pi = PyInfo.findSystemPython(3);
      log.debug("Found system wide python install: {}", pi);
      if (pi != null) {
        Bus.post(new MessagePythonNewBin(pi.getCommand()));
      } else {
        log.debug("Did not find system python");
      }
    } catch (UnexpectedException e) {
      log.warn("Problems while searching for system wide python installation", e);
    }
  }

  @Subscribe(threadMode = ThreadMode.ASYNC)
  public void on(MessagePythonNewBin m) {
    PyInfo pi;
    try {
      // first check if the path is absolute, then it must exist
      Path path = Paths.get(m.command);
      final boolean fileExists = Files.exists(path) || (OsUtils.isWindows() && Files.exists(Paths.get(path.toString() + ".exe")));
      if ((path.isAbsolute() && !fileExists) || StringUtils.isBlank(path.toString())) {
        throw new ValidationException("File does not exist");
      }

      // if paths.get didn't throw, we can try the binary, it might be on PATH
      pi = PyInfo.fromCommand(m.command);
      if (pi.getMajorVersion() < 3) {
        Bus.postSticky(
            new NoteConfigPython(pi, new ValidationException("Python version 3+ required"),
                pi.getCommand(), pi.getVersion()));
      } else {
        Bus.postSticky(new NoteConfigPython(pi));
      }
    } catch (ValidationException | UnexpectedException e) {
      Bus.postSticky(new NoteConfigPython(null, e, m.command, "N/A"));
    }
  }

  @Subscribe(sticky = true, threadMode = ThreadMode.MAIN_ORDERED)
  public void on(NoteConfigPython m) {
    uiTextBinPython.setText(m.command);
    epPythonVer.setText(StringUtils.isBlank(m.version) ? "Python version: N/A" : "Python version: " + m.version);
    if (m.ex != null) {
      showConfigError(m.ex, TIP_PYTHON_BIN, uiTextBinPython);
    }
  }

  private String textDbsplitEnabled(boolean isEnabled) {
    return "Database Splitting: <b>" + (isEnabled ? "Available" : "Not available") + "</b>.&emsp;"
        + "Used for searching very large databases by splitting into smaller chunks.";
  }

  @Subscribe(sticky = true, threadMode = ThreadMode.MAIN_ORDERED)
  public void on(NoteConfigDbsplit m) {
    if (m.ex != null) {
      log.debug("Got NoteDbsplitConfig with exception set");
      if (epDbsplitErrParent != null && !epDbsplitErrParent.isAncestorOf(epDbsplitErr)) {
        epDbsplitErrParent.add(epDbsplitErr, new CC().wrap());
        epDbsplitErr.setVisible(true);
      }
      epDbsplitText.setText(textDbsplitEnabled(false));
      if (m.ex instanceof ValidationException) {
        epDbsplitErr.setText(m.ex.getMessage());
      } else {
        showConfigError(m.ex, TIP_DBSPLIT, epDbsplitText);
      }
      this.revalidate();
      return;
    }
    if (m.instance == null) {
      throw new IllegalStateException("If no exception is reported from DBSplit init, instance should not be null");
    }
    log.debug("Got NoteDbsplitConfig without exceptions");

    epDbsplitErrParent = epDbsplitErr.getParent();
    if (epDbsplitErrParent != null) {
      epDbsplitErrParent.remove(epDbsplitErr);
    }
    epDbsplitText.setText(textDbsplitEnabled(true));
    this.revalidate();
  }

  private String textEasyPQPEnabled(String easypqpLocalVersion, String easypqpLatestVersion, boolean enableEasypqp) {
    StringBuilder sb = new StringBuilder();
    if (enableEasypqp && !easypqpLocalVersion.contentEquals("N/A")) {
      if (!easypqpLatestVersion.contentEquals("N/A") && VersionComparator.cmp(easypqpLocalVersion, easypqpLatestVersion) < 0) {
        sb.append("EasyPQP: <b>Available</b>. Version: " + easypqpLocalVersion + "<br>"
            + "<p style=\"color:red\">There is a new version (" + easypqpLatestVersion + "). Please upgrade it using the button below.<br>");
      } else {
        sb.append("EasyPQP: <b>Available</b>. Version: " + easypqpLocalVersion + "<br>");
      }
    } else {
      sb.append("EasyPQP: <b>Not available</b><br>"
          + "Please make sure that Python is installed, and then click the button below.<br>");
    }
    return sb.toString();
  }

  private String textSpectraSTEnabled(boolean enableSpectrast) {
    StringBuilder sb = new StringBuilder();
    if (enableSpectrast) {
      sb.append("SpectraST: <b>Available</b>");
    } else {
      sb.append("SpectraST: <b>Not available</b>");
    }
    return sb.toString();
  }

  @Subscribe(sticky = true, threadMode = ThreadMode.MAIN_ORDERED)
  public void on(NoteConfigSpeclibgen m) {
    if (m.ex != null) {
      log.debug("Got NoteConfigSpeclibgen with exception set");
      if (epSpeclibgenErrParent != null && !epSpeclibgenErrParent.isAncestorOf(epSpeclibgenErr)) {
        epSpeclibgenErrParent.add(epSpeclibgenErr, new CC().wrap());
        epSpeclibgenErr.setVisible(true);
      }
      epEasyPQPText.setText(textEasyPQPEnabled("N/A", "N/A", false));
      epSpectraSTText.setText(textSpectraSTEnabled(false));
      if (m.ex instanceof ValidationException) {
        epSpeclibgenErr.setText(m.ex.getMessage());
      } else {
        showConfigError(m.ex, TIP_SPECLIBGEN, epSpeclibgenErr);
      }
      this.revalidate();
      return;
    }

    if (m.instance == null) {
      throw new IllegalStateException("If no exception is reported from Speclibgen init, instance should not be null");
    }

    log.debug("Got NoteConfigSpeclibgen without exceptions");
    boolean enableSpectrast = true;
    boolean enableEasypqp = true;
    List<String> errMsgLines = new ArrayList<>();
    if (!m.instance.missingModulesSpeclibgen.isEmpty()) {
      errMsgLines.add("Missing python modules: " + Seq.seq(m.instance.missingModulesSpeclibgen).map(pm -> pm.installName).toString(", "));
      enableSpectrast = false;
      enableEasypqp = false;
    }
    if (!m.instance.missingModulesSpectrast.isEmpty()) {
      errMsgLines.add("Missing python modules for SpectraST: " + Seq.seq(m.instance.missingModulesSpectrast).map(pm -> pm.installName).toString(", "));
      enableSpectrast = false;
    }
    if (!m.instance.missingModulesEasyPqp.isEmpty()) {
      errMsgLines.add("Missing python modules for EasyPQP: " + Seq.seq(m.instance.missingModulesEasyPqp).map(pm -> pm.installName).toString(", "));
      enableEasypqp = false;
    }

    if (errMsgLines.isEmpty()) {
      epSpeclibgenErrParent = epSpeclibgenErr.getParent();
      if (epSpeclibgenErrParent != null) {
        epSpeclibgenErrParent.remove(epSpeclibgenErr);
      }
    } else {
      epSpeclibgenErr.setText(String.join("\n", errMsgLines));
    }

    // get EasyPQP local version
    String easypqpLocalVersion = "N/A";
    try {
      if (m.instance.isEasypqpOk()) {
        final ProcessBuilder pb = new ProcessBuilder(m.instance.getPython().getCommand(), "-c",
                "import pkg_resources\n" +
                        "try:\n" +
                        "    print(pkg_resources.get_distribution('easypqp').version)\n" +
                        "except pkg_resources.DistributionNotFound:\n" +
                        "    print('N/A')"
        );
        easypqpLocalVersion = ProcessUtils.captureOutput(pb);
      }
    } catch (Exception ex) {
      easypqpLocalVersion = "N/A";
    }

    // get EasyPQP latest version
    String easypqpLatestVersion;
    try {
      String response = org.apache.commons.io.IOUtils.toString(new URL("https://api.github.com/repos/grosenberger/easypqp/tags"), StandardCharsets.UTF_8);
      Gson gson = new GsonBuilder().create();
      List<GitHubJson> gitHubJsons = gson.fromJson(response, new TypeToken<List<GitHubJson>>() {}.getType());
      gitHubJsons.sort((e1, e2) -> VersionComparator.cmp(e2.getName(), e1.getName()));
      easypqpLatestVersion = gitHubJsons.get(0).getName();
    } catch (Exception ex) {
      easypqpLatestVersion = "N/A";
    }

    epEasyPQPText.setText(textEasyPQPEnabled(easypqpLocalVersion, easypqpLatestVersion, enableEasypqp));
    epSpectraSTText.setText(textSpectraSTEnabled(enableSpectrast));

    this.revalidate();
  }

  private void showConfigError(Throwable e, String balloonTopic, JComponent balloonParent) {
    if (e instanceof ValidationException) {
      Bus.post(new MessageBalloon(balloonTopic, balloonParent, e.getMessage()));
    } else {
      SwingUtils.showErrorDialogWithStacktrace(e, this);
    }
  }

  private static String createOthersCitationBody() {
    final StringBuilder sb = new StringBuilder();

    sb.append("<p style=\"margin-top: 0\">")
        .append("More info and docs: <a href=\"").append("https://www.nesvilab.org/Crystal-C/").append("\">Crystal-C</a>")
        .append(", <a href=\"").append("https://ionquant.nesvilab.org/").append("\">IonQuant</a>")
        .append(", <a href=\"").append("https://tmt-integrator.nesvilab.org/").append("\">TMT-Integrator</a>")
        .append(", <a href=\"").append("https://ptmshepherd.nesvilab.org/").append("\">PTM-Shepherd</a>")
        .append(", <a href=\"").append("https://github.com/grosenberger/easypqp").append("\">EasyPQP</a>")
        .append(", <a href=\"").append("http://percolator.ms/").append("\">Percolator</a>");
    return sb.toString();
  }

  private static String createFraggerCitationBody() {
    final StringBuilder sb = new StringBuilder();
    sb.append("&emsp;More info and docs: <a href=\"").append("https://msfragger.nesvilab.org/").append("\">MSFragger</a>");
    return sb.toString();
  }

  private JPanel createPanelPhilosopher() {
    JPanel p = newMigPanel();
    p.setBorder(new TitledBorder("Philosopher"));

    final String tip = "Select path to Philosopher binary";
    uiTextBinPhi = UiUtils.uiTextBuilder().create();
    uiTextBinPhi.addFocusListener(new ContentChangedFocusAdapter(uiTextBinPhi, (s, s2) -> {
      Bus.post(new MessagePhilosopherNewBin(s2));
    }));
    FormEntry feBin = fe(uiTextBinPhi, "bin-philosopher", TAB_PREFIX)
        .tooltip(tip).create();
    p.add(feBin.comp, ccL().split().growX());

    JButton btnBrowse = feBin
        .browseButton("Browse", tip, this::createPhilosopherFilechooser,
            paths -> paths.stream().findFirst()
                .ifPresent(bin -> Bus.post(new MessagePhilosopherNewBin(bin.toString()))));
    p.add(btnBrowse, ccL());
    JButton btnDownload = UiUtils.createButton("Download / Update", this::actionPhilosopherDownload);
    p.add(btnDownload, ccL().wrap());

    epPhiVer = new HtmlStyledJEditorPane("Philosopher version: N/A");
    p.add(Fragpipe.rename(epPhiVer, "philosopher.version-info", TAB_PREFIX, true), ccL().split());
    p.add(SwingUtils.createClickableHtml(createPhilosopherCitationBody()), ccL().spanX().growX().wrap());
    return p;
  }

  private JPanel createPanelPython() {
    JPanel p = newMigPanel();
    p.setBorder(new TitledBorder("Python"));
    final String tip = "Python 3 is required for Spectral Library generation and Database Splitting";
    final String ghost = "Select Python 3 binary (Anaconda Python recommended)";
    uiTextBinPython = UiUtils.uiTextBuilder().ghost(ghost).create();
    SwingUtils.addOnFocusLostAndContentChanged(uiTextBinPython, (s, s2) -> {
      Bus.post(new MessagePythonNewBin(s2));
    });
    FormEntry fe = fe(uiTextBinPython, "bin-python", TAB_PREFIX).tooltip(tip).create();

    p.add(fe.comp, ccL().split().growX());
    JButton btnBrowse = fe.browseButton("Browse", ghost, this::createPythonFilechooser, paths ->
        paths.stream().findFirst()
            .ifPresent(bin -> Bus.post(new MessagePythonNewBin(bin.toString()))));
    p.add(btnBrowse, ccL());
    final String url = ThisAppProps.def().getProperty(ThisAppProps.PROP_PYTHON_DOWNLOAD_URL);
    if (StringUtils.isNotBlank(url)) {
      JButton btnDonwload = UiUtils
          .createButton("Download", e -> SwingUtils.openBrowserOrThrow(url));
      p.add(btnDonwload, ccL().wrap());
    }
    epPythonVer = new HtmlStyledJEditorPane("Python version: N/A");
    p.add(epPythonVer, ccL().wrap());

//    final Runnable pipList = () -> {
//      final ProcessBuilder pb = new ProcessBuilder(uiTextBinPython.getNonGhostText(), "-m", "pip", "freeze");
//      String pythonPipOutputNew;
//      try {
//        pythonPipOutputNew = ProcessUtils.captureOutput(pb);
//      } catch (UnexpectedException ex) {
//        pythonPipOutputNew = null;
//      }
//      if (pythonPipOutputNew != null && !pythonPipOutput.equals(pythonPipOutputNew)) {
//        pythonPipOutput = pythonPipOutputNew;
//        Bus.post(new MessageUiRevalidate());
//      }
//    };
//    final javax.swing.Timer timer = new javax.swing.Timer(5000, e -> {
//      final String binPython = uiTextBinPython.getNonGhostText();
//      if (StringUtils.isNotBlank(binPython))
//        javax.swing.SwingUtilities.invokeLater(pipList);
//    });
//    timer.setInitialDelay(0);
//    timer.start();

    return p;
  }

  @Subscribe
  public void on(MessageInstallEasyPQP m) {
    final String binPython = uiTextBinPython.getNonGhostText();
    if (StringUtils.isNotBlank(binPython)) {
      final ProcessBuilder pb = new ProcessBuilder(binPython, "-m",
              "pip", "uninstall", "--yes", "easypqp");
      String pythonPipOutputNew;
      try {
        pythonPipOutputNew = ProcessUtils.captureOutput(pb);
      } catch (UnexpectedException ex) {
        pythonPipOutputNew = ex.toString();
      }
      final ProcessBuilder pb2 = new ProcessBuilder(binPython, "-m",
              "pip", "install", "git+https://github.com/grosenberger/easypqp.git@master");
      try {
        pythonPipOutputNew += ProcessUtils.captureOutput(pb2);
      } catch (UnexpectedException ex) {
        pythonPipOutputNew += ex.toString();
      }
      System.out.println("pythonPipOutputNew = " + pythonPipOutputNew);
      Bus.post(new MessageUiRevalidate());
    }
  }

  private JPanel createPanelDbsplit() {
    JPanel p = mu.newPanel("Database Splitting", true);

    StringBuilder tip = new StringBuilder()
        .append("Used for searching very large databases by splitting into smaller chunks.<br/>")
        .append("Requires <b>Python 3</b> with packages <b>Numpy, Pandas</b>.\n")
        .append("Ways to get everything set up:").append("<ul>")
        .append("<li>Install Python 3 if you don't yet have it.</li>")
        .append(
            "<li>Install required python modules using <i>pip</i>, the python package manager, with command:</li>")
        .append("<ul>").append("<li>pip install numpy pandas</li>").append("</ul>")
        .append("</ul>");
    String tipHtml = SwingUtils.makeHtml(tip.toString());
    p.setToolTipText(tipHtml);

    Dimension dim = new Dimension(400, 25);
    epDbsplitText = new HtmlStyledJEditorPane(textDbsplitEnabled(false));
    epDbsplitText.setToolTipText(tipHtml);
    epDbsplitText.setPreferredSize(dim);
    epDbsplitErr = new HtmlStyledJEditorPane("Requires Python 3 with modules Numpy and Pandas.");
    epDbsplitErr.setPreferredSize(dim);

    mu.add(p, epDbsplitText).growX().pushX().wrap();
    mu.add(p, epDbsplitErr).growX().pushX().wrap();

    return p;
  }

  private JPanel createPanelSpeclibgen() {
    JPanel p = mu.newPanel("Spectral Library Generation", true);

    p.setToolTipText(SwingUtils.makeHtml("SpectraST: Requires <b>Python 3</b> with packages <b>Cython, Matplotlib, msproteomicstools</b>\nEasyPQP: Requires <b>Python 3</b> with package <b>EasyPQP</b>"));
    Dimension dim = new Dimension(200, 25);

    epEasyPQPText = new HtmlStyledJEditorPane("Configuring EasyPQP.");
    epEasyPQPText.setToolTipText(SwingUtils.makeHtml("EasyPQP: Requires <b>Python 3</b> with package <b>EasyPQP</b>"));
    epEasyPQPText.setPreferredSize(dim);

    epSpectraSTText = new HtmlStyledJEditorPane("Configuring SpectraST.");
    epSpectraSTText.setToolTipText(SwingUtils.makeHtml("SpectraST: Requires <b>Python 3</b> with packages <b>Cython, Matplotlib, msproteomicstools</b>"));
    epSpectraSTText.setPreferredSize(dim);

    epSpeclibgenErr = new HtmlStyledJEditorPane("Requires Python 3 with modules Cython, Matplotlib, msproteomicstools.");
    epSpeclibgenErr.setPreferredSize(dim);

    final JButton btnInstallEasyPQP = UiUtils.createButton("Install/Upgrade EasyPQP", e -> Bus.post(new MessageInstallEasyPQP()));

    mu.add(p, epEasyPQPText).growX().wrap();
    mu.add(p, btnInstallEasyPQP).split().wrap();
    mu.add(p, new HtmlStyledJEditorPane("")).wrap();
    mu.add(p, epSpectraSTText).split();
    mu.add(p, epSpeclibgenErr).growX().wrap();

    return p;
  }

  private JFileChooser createPhilosopherFilechooser() {
    JFileChooser fc = FileChooserUtils.create("Select Philosopher binary", "Select",
        false, FcMode.FILES_ONLY, true);
    if (OsUtils.isWindows()) {
      fc.addChoosableFileFilter(new FileNameExtensionFilter("Executables", "exe"));
    }
    FileChooserUtils.setPath(fc, Stream.of(
        uiTextBinPhi.getNonGhostText(),
        Fragpipe.propsVarGet(ThisAppProps.PROP_BINARIES_IN),
        JarUtils.getCurrentJarPath()));
    return fc;
  }

  private JFileChooser createPythonFilechooser() {
    JFileChooser fc = FileChooserUtils.create("Select Python 3 binary", "Select",
        false, FcMode.FILES_ONLY, true);
    if (OsUtils.isWindows()) {
      fc.addChoosableFileFilter(new FileNameExtensionFilter("Executables", "exe"));
    }

    PyInfo sysPy = null;
    try {
      sysPy = PyInfo.findSystemPython(3);
    } catch (UnexpectedException e) {
      log.debug("Something happened while checking system python for Python bin file chooser", e);
    }

    Stream<String> pathsToCheck = Stream
        .of(uiTextBinPython.getNonGhostText(), sysPy == null ? "" : sysPy.getCommand())
        .filter(StringUtils::isNotBlank)
        .flatMap(text -> {
          try {
            Path p = Paths.get(text);
            return p.isAbsolute() ? Stream.of(p, p.getParent()) : Stream.of(p);
          } catch (Exception e) {
            return Stream.empty();
          }
        }).distinct().map(Path::toString);
    FileChooserUtils.setPath(fc, pathsToCheck);
    return fc;
  }

  private String createPhilosopherCitationBody() {
    StringBuilder sb = new StringBuilder();
    sb.append("&emsp;More info and docs: <a href=\"https://philosopher.nesvilab.org/\">Philosopher</a>");
    return sb.toString();
  }

  private void actionPhilosopherDownload(ActionEvent e) {
    int choice = SwingUtils.showChoiceDialog(TabConfig.this, "Choose download type",
        "Do you want to attemp automatic download?\n"
            + "If you choose \"Manually\", you will be redirected to the download website.",
        new String[]{"Automatically", "Manually", "Cancel"}, 0);
    switch (choice) {
      case 0:
        new Thread(() -> {
          try {
            SwingUtils.setUncaughtExceptionHandlerMessageDialog(TabConfig.this);
            Philosopher.downloadPhilosopherAutomatically();
          } catch (IOException ex) {
            throw new IllegalStateException("Error downloading Philosopher automatically", ex);
          }
        }).start();
        break;
      case 1:
        Philosopher.downloadPhilosopherManually();
        break;
    }
  }
}
