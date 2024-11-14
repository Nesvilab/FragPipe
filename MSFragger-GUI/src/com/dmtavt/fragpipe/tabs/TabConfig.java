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

package com.dmtavt.fragpipe.tabs;

import static com.dmtavt.fragpipe.Fragpipe.fe;
import static com.dmtavt.fragpipe.Fragpipe.headless;
import static com.dmtavt.fragpipe.messages.MessagePrintToConsole.toConsole;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.FragpipeLocations;
import com.dmtavt.fragpipe.api.Bus;
import com.dmtavt.fragpipe.api.Notifications;
import com.dmtavt.fragpipe.api.PyInfo;
import com.dmtavt.fragpipe.dialogs.DownloadToolsPanel;
import com.dmtavt.fragpipe.exceptions.NoStickyException;
import com.dmtavt.fragpipe.exceptions.UnexpectedException;
import com.dmtavt.fragpipe.exceptions.ValidationException;
import com.dmtavt.fragpipe.messages.MessageBalloon;
import com.dmtavt.fragpipe.messages.MessageClearCache;
import com.dmtavt.fragpipe.messages.MessageDiaTracerNewBin;
import com.dmtavt.fragpipe.messages.MessageDiaTracerUpdateAvailable;
import com.dmtavt.fragpipe.messages.MessageDiannNewBin;
import com.dmtavt.fragpipe.messages.MessageFindSystemPython;
import com.dmtavt.fragpipe.messages.MessageInstallEasyPQP;
import com.dmtavt.fragpipe.messages.MessageIonQuantNewBin;
import com.dmtavt.fragpipe.messages.MessageIonQuantUpdateAvailable;
import com.dmtavt.fragpipe.messages.MessageMsfraggerNewBin;
import com.dmtavt.fragpipe.messages.MessageMsfraggerUpdateAvailable;
import com.dmtavt.fragpipe.messages.MessagePythonNewBin;
import com.dmtavt.fragpipe.messages.MessageShowAboutDialog;
import com.dmtavt.fragpipe.messages.MessageUiRevalidate;
import com.dmtavt.fragpipe.messages.NoteConfigDbsplit;
import com.dmtavt.fragpipe.messages.NoteConfigDiaTracer;
import com.dmtavt.fragpipe.messages.NoteConfigDiann;
import com.dmtavt.fragpipe.messages.NoteConfigIonQuant;
import com.dmtavt.fragpipe.messages.NoteConfigMsfragger;
import com.dmtavt.fragpipe.messages.NoteConfigPython;
import com.dmtavt.fragpipe.messages.NoteConfigSpeclibgen;
import com.dmtavt.fragpipe.messages.NoteFragpipeUpdate;
import com.dmtavt.fragpipe.params.ThisAppProps;
import com.dmtavt.fragpipe.tools.diann.Diann;
import com.dmtavt.fragpipe.tools.diatracer.DiaTracer;
import com.dmtavt.fragpipe.tools.diatracer.DiaTracerVersionFetcherServer;
import com.dmtavt.fragpipe.tools.fragger.Msfragger;
import com.dmtavt.fragpipe.tools.fragger.Msfragger.Version;
import com.dmtavt.fragpipe.tools.fragger.MsfraggerVersionFetcherServer;
import com.dmtavt.fragpipe.tools.ionquant.IonQuant;
import com.dmtavt.fragpipe.tools.ionquant.IonQuantVersionFetcherServer;
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
import com.github.chhh.utils.swing.TextConsole;
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
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.List;
import java.util.Objects;
import java.util.Properties;
import java.util.TreeMap;
import java.util.regex.Matcher;
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
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.ThreadMode;
import org.jooq.lambda.Seq;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class TabConfig extends JPanelWithEnablement {

  private static final Logger log = LoggerFactory.getLogger(TabConfig.class);
  private static final DefaultArtifactVersion msfraggerMinVersion = new DefaultArtifactVersion("4.1");
  private static final DefaultArtifactVersion ionquantMinVersion = new DefaultArtifactVersion("1.10.27");
  private static final DefaultArtifactVersion diatracerMinVersion = new DefaultArtifactVersion("1.1.3");
  public static final DefaultArtifactVersion pythonMinVersion = new DefaultArtifactVersion("3.9");

  private static final MigUtils mu = MigUtils.get();
  private static final Pattern emailPattern = Pattern.compile("(?:[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*|\"(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21\\x23-\\x5b\\x5d-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])*\")@(?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\\[(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?|[a-z0-9-]*[a-z0-9]:(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21-\\x5a\\x53-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])+)\\])");

  public static final String TAB_NAME = "Config";

  private UiText uiTextToolsFolder;
  private HtmlStyledJEditorPane epFraggerVer;
  private HtmlStyledJEditorPane epIonQuantVer;
  private HtmlStyledJEditorPane epDiaTracerVer;
  private UiText uiTextBinDiann;
  private HtmlStyledJEditorPane epDiannVer;
  private UiText uiTextBinPython;
  private HtmlStyledJEditorPane epPythonVer;
  private HtmlStyledJEditorPane epDbsplitText;
  private HtmlStyledJEditorPane epDbsplitErr;
  private Container epDbsplitErrParent;
  private HtmlStyledJEditorPane epEasyPQPText;
  private Container epSpeclibgenParent;
  private JButton btnAbout;

  public static final String TIP_MSFRAGGER_BIN = "tip.msfragger.bin";
  public static final String TIP_IONQUANT_BIN = "tip.ionquant.bin";
  public static final String TIP_DIATRACER_BIN = "tip.diatracer.bin";
  public static final String TIP_DIANN_BIN = "tip.diann.bin";
  public static final String TIP_PYTHON_BIN = "tip.python.bin";
  private static final String TIP_DBSPLIT = "tip.dbsplit";
  private static final String TIP_SPECLIBGEN = "tip.speclibgen";
  private static final String TIP_FRAGPIPE_UPDATE = "tip.fragpipe.update";
  public static final String TAB_PREFIX = "fragpipe-config.";

  // cache user info to save some typing for the users
  public static String userName = null;
  public static String userEmail = null;
  public static String userInstitution = null;

  private static final Pattern msfraggerRegex = Pattern.compile("msfragger-(.*)\\.jar", Pattern.CASE_INSENSITIVE);
  private static final Pattern ionquantRegex = Pattern.compile("ionquant-(.*)\\.jar", Pattern.CASE_INSENSITIVE);
  private static final Pattern diatracerRegex = Pattern.compile("diatracer-(.*)\\.jar", Pattern.CASE_INSENSITIVE);

  private final TextConsole console;

  public TabConfig(TextConsole console) {
    this.console = console;
    init();
    initMore();
  }

  public String getJarPath(String s, Pattern pattern) {
    if (!s.isEmpty()) {
      Path p = Paths.get(s);
      if (Files.exists(p) && Files.isDirectory(p)) {
        try {
          TreeMap<DefaultArtifactVersion, Path> map = new TreeMap<>();
          Files.walk(p).forEach(pp -> {
            Matcher m = pattern.matcher(pp.getFileName().toString());
            if (m.matches()) {
              map.put(new DefaultArtifactVersion(m.group(1)), pp);
            }
          });
          if (!map.isEmpty()) {
            uiTextToolsFolder.setText(s);
            return map.lastEntry().getValue().toAbsolutePath().toString();
          }
        } catch (Exception ex) {
          SwingUtils.showErrorDialogWithStacktrace(ex, null);
        }
      }
    }
    return "";
  }

  private void initMore() {
    Bus.register(this);
  }

  private void init() {
    this.setLayout(new MigLayout(new LC().fillX()));
    add(createPanelTopButtons(), new CC().growX().wrap());
    add(createPanelTools(), new CC().growX().wrap());
    add(createPanelDiann(), new CC().growX().wrap());
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
        "https://fragpipe.nesvilab.org/docs/tutorial_setup_fragpipe.html");

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
    
    JLabel sysInfo = new JLabel(SwingUtils.makeHtml(
        OsUtils.OsInfo() + "\n"
            + OsUtils.JavaInfo() + "\n"
            + OsUtils.NetCoreInfo() + "\n"
            + "FragPipe: v" + com.dmtavt.fragpipe.Version.version(false)));
            //+ FragpipeLocations.get().getJarPath().toString()));

    sysInfo.setVerticalAlignment(JLabel.TOP);
    //p.add(UiUtils.createButton("Find tools", e -> post(new MessageFindTools())), ccL.get().split().spanX());

    JLabel imageLabel = new JLabel();
    try {
      BufferedImage image = ImageIO.read(Objects.requireNonNull(getClass().getResource("/com/dmtavt/fragpipe/icons/fragpipe-128.png")));
      imageLabel = new JLabel(new ImageIcon(image));
    } catch (Exception ex) {
      ex.printStackTrace();
    }

    p.add(sysInfo, ccR().growX());
    p.add(imageLabel, ccL().wrap());

    JLabel label = new JLabel("Main tools configuration");
    label.setFont(new Font(label.getFont().getName(), Font.BOLD, label.getFont().getSize() + 3));
    p.add(label, ccL().wrap());

    JEditorPane ep = SwingUtils.createClickableHtml(createOthersCitationBody());
    p.add(ep, ccL().spanX().growX().wrap());
    return p;
  }

  private JPanel createPanelTools() {
    JPanel p = newMigPanel();
    p.setBorder(new TitledBorder("MSFragger, IonQuant, diaTracer"));

    final String toolsFolderTip = "Select path to the folder containing the tools";
    uiTextToolsFolder = UiUtils.uiTextBuilder().create();
    uiTextToolsFolder.addActionListener(e -> {
      Bus.post(new MessageMsfraggerNewBin(getJarPath(uiTextToolsFolder.getNonGhostText(), msfraggerRegex)));
      Bus.post(new MessageIonQuantNewBin(getJarPath(uiTextToolsFolder.getNonGhostText(), ionquantRegex)));
      Bus.post(new MessageDiaTracerNewBin(getJarPath(uiTextToolsFolder.getNonGhostText(), diatracerRegex)));
    });
    FormEntry feBinMsfragger = fe(uiTextToolsFolder, "tools-folder", TAB_PREFIX).tooltip(toolsFolderTip).create();
    p.add(feBinMsfragger.comp, ccL().split().growX());

    JButton btnBrowse = feBinMsfragger.browseButton("Browse", toolsFolderTip, this::createToolsFolderChooser, paths -> {
      paths.stream().findFirst().ifPresent(pp -> {
        Bus.post(new MessageMsfraggerNewBin(getJarPath(pp.toString(), msfraggerRegex)));
        Bus.post(new MessageIonQuantNewBin(getJarPath(pp.toString(), ionquantRegex)));
        Bus.post(new MessageDiaTracerNewBin(getJarPath(pp.toString(), diatracerRegex)));
      });
    });
    p.add(btnBrowse, ccL());
    JButton btnUpdate = UiUtils.createButton("Download / Update", this::actionToolsDownload);

    epFraggerVer = new HtmlStyledJEditorPane("MSFragger version: N/A");
    epIonQuantVer = new HtmlStyledJEditorPane("IonQuant version: N/A");
    epDiaTracerVer = new HtmlStyledJEditorPane("diaTracer version: N/A");

    JEditorPane msfraggerCitation = SwingUtils.createClickableHtml(createFraggerCitationBody());
    JEditorPane ionQuantCitation = SwingUtils.createClickableHtml(createIonQuantCitationBody());
    JEditorPane diaTracerCitation = SwingUtils.createClickableHtml(createDiaTracerCitationBody());

    p.add(btnUpdate, ccL().wrap());

    p.add(Fragpipe.renameNoCache(epFraggerVer, "msfragger.version-info", TAB_PREFIX), ccL().split());
    p.add(msfraggerCitation, ccL().spanX().growX().wrap());

    p.add(Fragpipe.renameNoCache(epIonQuantVer, "ionquant.version-info", TAB_PREFIX), ccL().split());
    p.add(ionQuantCitation, ccL().spanX().growX().wrap());

    p.add(Fragpipe.renameNoCache(epDiaTracerVer, "diatracer.version-info", TAB_PREFIX), ccL().split());
    p.add(diaTracerCitation, ccL().spanX().growX().wrap());

    return p;
  }

  @Subscribe(sticky = true, threadMode = ThreadMode.MAIN_ORDERED)
  public void on(NoteFragpipeUpdate m) {
    log.debug("Got NoteFragpipeUpdate: {}", m.toString());
    StringBuilder sb = new StringBuilder();
    if (StringUtils.isNotBlank(m.releaseVer)) {
      sb.append(String.format("FragPipe update available, new version %s\n", m.releaseVer));
    }
    if (!headless && StringUtils.isNotBlank(m.downloadUrl)) {
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
    JEditorPane ep = SwingUtils.createClickableHtml(String.format("There is a newer version of MSFragger available [%s].<br>\n", m.newVersion), Notifications.BG_COLOR);
    JPanel content = new JPanel(new BorderLayout());
    content.setBackground(ep.getBackground());

    JPanel pBtns = new JPanel();
    pBtns.setBackground(ep.getBackground());
    pBtns.setLayout(new FlowLayout(FlowLayout.LEFT, 5, 5));

    if (!StringUtils.isNullOrWhitespace(m.manualDownloadUrl)) {
      JButton btnManualUpdate = new JButton("Download update");
      btnManualUpdate.addActionListener(
        this::actionToolsDownload
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

    Bus.post(new MessageBalloon(TIP_MSFRAGGER_BIN, uiTextToolsFolder, content));
  }

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void on(MessageIonQuantUpdateAvailable m) {
    JEditorPane ep = SwingUtils.createClickableHtml(String.format("There is a newer version of IonQuant available [%s].<br>\n", m.newVersion), Notifications.BG_COLOR);
    JPanel content = new JPanel(new BorderLayout());
    content.setBackground(ep.getBackground());

    JPanel pBtns = new JPanel();
    pBtns.setBackground(ep.getBackground());
    pBtns.setLayout(new FlowLayout(FlowLayout.LEFT, 5, 5));

    if (!StringUtils.isNullOrWhitespace(m.manualDownloadUrl)) {
      JButton btnManualUpdate = new JButton("Download update");
      btnManualUpdate.addActionListener(
          this::actionToolsDownload
      );
      pBtns.add(btnManualUpdate);
    }

    JButton btnClose = new JButton("Close");
    btnClose.addActionListener(e -> {
      Bus.post(new MessageBalloon(TIP_IONQUANT_BIN));
    });

    content.add(ep, BorderLayout.CENTER);
    pBtns.add(btnClose);
    content.add(pBtns, BorderLayout.SOUTH);

    Bus.post(new MessageBalloon(TIP_IONQUANT_BIN, uiTextToolsFolder, content));
  }

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void on(MessageDiaTracerUpdateAvailable m) {
    JEditorPane ep = SwingUtils.createClickableHtml(String.format("There is a newer version of diaTracer available [%s].<br>\n", m.newVersion), Notifications.BG_COLOR);
    JPanel content = new JPanel(new BorderLayout());
    content.setBackground(ep.getBackground());

    JPanel pBtns = new JPanel();
    pBtns.setBackground(ep.getBackground());
    pBtns.setLayout(new FlowLayout(FlowLayout.LEFT, 5, 5));

    if (!StringUtils.isNullOrWhitespace(m.manualDownloadUrl)) {
      JButton btnManualUpdate = new JButton("Download update");
      btnManualUpdate.addActionListener(
          this::actionToolsDownload
      );
      pBtns.add(btnManualUpdate);
    }

    JButton btnClose = new JButton("Close");
    btnClose.addActionListener(e -> {
      Bus.post(new MessageBalloon(TIP_DIATRACER_BIN));
    });

    content.add(ep, BorderLayout.CENTER);
    pBtns.add(btnClose);
    content.add(pBtns, BorderLayout.SOUTH);

    Bus.post(new MessageBalloon(TIP_DIATRACER_BIN, uiTextToolsFolder, content));
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

  private void actionToolsDownload(ActionEvent evt) {
    try {
      DownloadToolsPanel p = new DownloadToolsPanel();
      int confirmation = SwingUtils.showConfirmDialog2(this, p, "Please agree to the terms of the licenses.", JOptionPane.YES_NO_CANCEL_OPTION);
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

        userName = p.getName();
        userEmail = p.getEmail();
        userInstitution = p.getInstitution();

        if (!p.licensesChecked()) {
          JOptionPane.showMessageDialog(this, "Please read and check all of the licenses.", "Error", JOptionPane.ERROR_MESSAGE);
          return;
        }

        Path toolsPath = PathUtils.createDirs(FragpipeLocations.get().getDirTools()).normalize();

        if (p.downloadMSFragger()) {
          MsfraggerVersionFetcherServer msfraggerVersionFetcherServer = new MsfraggerVersionFetcherServer(p.getName(), p.getEmail(), p.getInstitution(), p.wantReceiveEmail());
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
        } else {
          Bus.post(new MessageMsfraggerNewBin(getJarPath(toolsPath.toAbsolutePath().toString(), msfraggerRegex)));
        }

        if (p.downloadIonQuant()) {
          IonQuantVersionFetcherServer ionQuantVersionFetcherServer = new IonQuantVersionFetcherServer(p.getName(), p.getEmail(), p.getInstitution(), p.wantReceiveEmail());
          new Thread(() -> {
            try {
              Path ionquantPath = ionQuantVersionFetcherServer.autoUpdate(toolsPath);
              if (ionquantPath != null) {
                Bus.post(new MessageIonQuantNewBin(ionquantPath.toAbsolutePath().toString()));
              } else {
                Bus.postSticky(new NoteConfigIonQuant("N/A", "N/A", false, false, new ValidationException("IonQuant path is null.")));
              }
            } catch (Exception ex) {
              Bus.postSticky(new NoteConfigIonQuant("N/A", "N/A", false, false, ex));
            }
          }).start();
        } else {
          Bus.post(new MessageIonQuantNewBin(getJarPath(toolsPath.toAbsolutePath().toString(), ionquantRegex)));
        }

        if (p.downloadDiaTracer()) {
          DiaTracerVersionFetcherServer diaTracerVersionFetcherServer = new DiaTracerVersionFetcherServer(p.getName(), p.getEmail(), p.getInstitution(), p.wantReceiveEmail());
          new Thread(() -> {
            try {
              Path diatracerPath = diaTracerVersionFetcherServer.autoUpdate(toolsPath);
              if (diatracerPath != null) {
                Bus.post(new MessageDiaTracerNewBin(diatracerPath.toAbsolutePath().toString()));
              } else {
                Bus.postSticky(new NoteConfigDiaTracer("N/A", "N/A", false, false, new ValidationException("diaTracer path is null.")));
              }
            } catch (Exception ex) {
              Bus.postSticky(new NoteConfigDiaTracer("N/A", "N/A", false, false, ex));
            }
          }).start();
        } else {
          Bus.post(new MessageDiaTracerNewBin(getJarPath(toolsPath.toAbsolutePath().toString(), diatracerRegex)));
        }

        uiTextToolsFolder.setText(toolsPath.toAbsolutePath().toString());
      }
    } catch (Exception ex) {
      Bus.postSticky(new NoteConfigMsfragger("N/A", "N/A", ex));
    }
  }

  private JFileChooser createToolsFolderChooser() {
    JFileChooser fc = FileChooserUtils.create("Select tools folder", "Select", false, FcMode.DIRS_ONLY, true);
    FileChooserUtils.setPath(fc, Stream.of(uiTextToolsFolder.getNonGhostText(), Fragpipe.propsVarGet(ThisAppProps.PROP_BINARIES_IN), FragpipeLocations.get().getDirFragpipeRoot().toString()));
    return fc;
  }

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void on(MessageDiannNewBin m) {
    if (StringUtils.isBlank(m.path) || !Files.exists(Paths.get(m.path.replaceAll("\"", "")))) {
      Bus.postSticky(new NoteConfigDiann());
      return;
    }

    if (m.path.contains(" ") && !m.path.startsWith("\"") && !m.path.endsWith("\"")) {
      m.path = "\"" + m.path + "\"";
    }

    try {
      Diann.Version v = Diann.validate(m.path);
      Bus.postSticky(new NoteConfigDiann(m.path, v.version, null, true));
    } catch (Exception e) {
      e.printStackTrace();
      Bus.postSticky(new NoteConfigDiann());
    }
  }

  @Subscribe(sticky = true, threadMode = ThreadMode.MAIN_ORDERED)
  public void on(NoteConfigDiann m) {
    log.debug("Got {}", m);
    uiTextBinDiann.setText(m.path);

    Path existing = PathUtils.existing(m.path);
    if (existing != null) {
      Fragpipe.propsVarSet(ThisAppProps.PROP_BINARIES_IN, existing.toString());
    }

    if (m.ex != null) {
      epDiannVer.setText("DIA-NN version: N/A");
      showConfigError(m.ex, TIP_DIANN_BIN, uiTextBinDiann, true);
    } else {
      epDiannVer.setText("DIA-NN version: " + m.version);
      Notifications.tryClose(TIP_DIANN_BIN);
    }
  }

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void on(MessageMsfraggerNewBin m) {
    if (StringUtils.isBlank(m.binPath) || !Files.exists(Paths.get(m.binPath))) {
      Bus.postSticky(new NoteConfigMsfragger(m.binPath, "N/A", false, new ValidationException("MSFragger path " + m.binPath + " does not exist.")));
      return;
    }

    if (!validateJarContents(Paths.get(m.binPath), "MSFragger.class", msfraggerRegex)) {
      Bus.postSticky(new NoteConfigMsfragger(m.binPath, "N/A", false, new ValidationException("Not a MSFragger jar.")));
      return;
    }

    if (m.binPath.contains(" ")) {
      Bus.postSticky(new NoteConfigMsfragger(m.binPath, "N/A", false, new ValidationException("There are spaces in the path: \"" + m.binPath + "\"")));
      return;
    }

    Version v;
    try {
      v = Msfragger.getVersion(Paths.get(m.binPath));
      if (v.isVersionParsed) {
        if (v.version.compareTo(msfraggerMinVersion) >= 0) {
          Bus.postSticky(new NoteConfigMsfragger(m.binPath, v.version.toString()));
        } else {
          Bus.postSticky(new NoteConfigMsfragger(m.binPath, v.version.toString(), true, null));
        }
      } else {
        Bus.postSticky(new NoteConfigMsfragger(m.binPath, "N/A", null));
      }
    } catch (Exception e) {
      Bus.postSticky(new NoteConfigMsfragger(m.binPath, "N/A", e));
    }
  }

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void on(MessageIonQuantNewBin m) {
    if (StringUtils.isBlank(m.binPath) || !Files.exists(Paths.get(m.binPath))) {
      Bus.postSticky(new NoteConfigIonQuant(m.binPath, "N/A", false, false, new ValidationException("IonQuant path " + m.binPath + " does not exist.")));
      return;
    }

    if (!validateJarContents(Paths.get(m.binPath), "IonQuant.class", ionquantRegex)) {
      Bus.postSticky(new NoteConfigIonQuant(m.binPath, "N/A", false, false, new ValidationException("Not an IonQuant jar.")));
      return;
    }

    if (m.binPath.contains(" ")) {
      Bus.postSticky(new NoteConfigIonQuant(m.binPath, "N/A", false, false, new ValidationException("There are spaces in the path: \"" + m.binPath + "\"")));
      return;
    }

    Version v;
    try {
      v = IonQuant.getVersion(Paths.get(m.binPath));
      if (v.isVersionParsed) {
        if (v.version.compareTo(ionquantMinVersion) >= 0) {
          Bus.postSticky(new NoteConfigIonQuant(m.binPath, v.version.toString(), false, true, null));
        } else {
          Bus.postSticky(new NoteConfigIonQuant(m.binPath, v.version.toString(), true, false, null));
        }
      } else {
        Bus.postSticky(new NoteConfigIonQuant(m.binPath, "N/A", false, false, new ValidationException("Could not parse the version.")));
      }
    } catch (Exception e) {
      Bus.postSticky(new NoteConfigIonQuant(m.binPath, "N/A", false, false, e));
    }
  }

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void on(MessageDiaTracerNewBin m) {
    if (StringUtils.isBlank(m.binPath) || !Files.exists(Paths.get(m.binPath))) {
      Bus.postSticky(new NoteConfigDiaTracer(m.binPath, "N/A", false, false, new ValidationException("diaTracer path " + m.binPath + " does not exist.")));
      return;
    }

    if (!validateJarContents(Paths.get(m.binPath), "diaTracerMainClass.class", diatracerRegex)) {
      Bus.postSticky(new NoteConfigDiaTracer(m.binPath, "N/A", false, false, new ValidationException("Not an diaTracer jar.")));
      return;
    }

    if (m.binPath.contains(" ")) {
      Bus.postSticky(new NoteConfigDiaTracer(m.binPath, "N/A", false, false, new ValidationException("There are spaces in the path: \"" + m.binPath + "\"")));
      return;
    }

    Version v;
    try {
      v = DiaTracer.getVersion(Paths.get(m.binPath));
      if (v.isVersionParsed) {
        if (v.version.compareTo(diatracerMinVersion) >= 0) {
          Bus.postSticky(new NoteConfigDiaTracer(m.binPath, v.version.toString(), false, true, null));
        } else {
          Bus.postSticky(new NoteConfigDiaTracer(m.binPath, v.version.toString(), true, false, null));
        }
      } else {
        Bus.postSticky(new NoteConfigDiaTracer(m.binPath, "N/A", false, false, new ValidationException("Could not parse the version.")));
      }
    } catch (Exception e) {
      Bus.postSticky(new NoteConfigDiaTracer(m.binPath, "N/A", false, false, e));
    }
  }

  private static boolean validateJarContents(Path p, String mainClass, Pattern pattern) {
    final boolean[] found = {false};
    try (FileSystem fs = FileSystems.newFileSystem(p, ClassLoader.getSystemClassLoader())) {
      for (Path root : fs.getRootDirectories()) {
        Files.walkFileTree(root, new SimpleFileVisitor<>() {
          @Override
          public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) {
            String fileName = file.getFileName().toString();
            if (mainClass.equalsIgnoreCase(fileName)) {
              found[0] = true;
              return FileVisitResult.TERMINATE;
            } else if (pattern.matcher(fileName).find()) {
              found[0] = true;
              return FileVisitResult.TERMINATE;
            }
            return FileVisitResult.CONTINUE;
          }
        });
      }
    } catch (IOException ex) {
      log.warn("Error while checking " + p.toAbsolutePath() + " contents", ex);
    }
    return found[0];
  }

  @Subscribe(sticky = true, threadMode = ThreadMode.MAIN_ORDERED)
  public void on(NoteConfigMsfragger m) {
    log.debug("Got {}", m);

    Path existing = PathUtils.existing(m.path);
    if (existing != null) {
      Fragpipe.propsVarSet(ThisAppProps.PROP_BINARIES_IN, existing.toString());
      if (existing.toString().contains(" ")) {
        SwingUtils.showErrorDialog(this, "Space is not allowed in MSFragger path.", "Spaces in path");
      }
    }

    if (m.ex != null) {
      epFraggerVer.setText("MSFragger version: N/A");
      showConfigError(m.ex, TIP_MSFRAGGER_BIN, uiTextToolsFolder, true);
    } else if (m.isTooOld) {
      epFraggerVer.setText("MSFragger version: too old, not supported anymore");
      Bus.post(new MessageBalloon(TIP_MSFRAGGER_BIN, uiTextToolsFolder, "MSFragger " + msfraggerMinVersion + " is required.", true));
    } else {
      epFraggerVer.setText("MSFragger version: " + m.version);
      Notifications.tryClose(TIP_MSFRAGGER_BIN);
    }
    if (m.isValid() && !m.isTooOld) {
      Msfragger.checkUpdates(m);
    }
  }

  @Subscribe(sticky = true, threadMode = ThreadMode.MAIN_ORDERED)
  public void on(NoteConfigIonQuant m) {
    Path existing = PathUtils.existing(m.path);
    if (existing != null) {
      Fragpipe.propsVarSet(ThisAppProps.PROP_BINARIES_IN, existing.toString());
      if (existing.toString().contains(" ")) {
        SwingUtils.showErrorDialog(this, "Space is not allowed in IonQuant path.", "Spaces in path");
      }
    }

    if (m.ex != null) {
      epIonQuantVer.setText("IonQuant version: N/A");
      showConfigError(m.ex, TIP_IONQUANT_BIN, uiTextToolsFolder, true);
    } else if (m.isTooOld) {
      epIonQuantVer.setText("IonQuant version: too old, not supported anymore");
      Bus.post(new MessageBalloon(TIP_IONQUANT_BIN, uiTextToolsFolder, "IonQuant " + ionquantMinVersion + " is required.", true));
    } else {
      epIonQuantVer.setText("IonQuant version: " + m.version);
      Notifications.tryClose(TIP_IONQUANT_BIN);
    }
    if (m.isValid() && !m.isTooOld) {
      IonQuant.checkUpdates(m);
    }
  }

  @Subscribe(sticky = true, threadMode = ThreadMode.MAIN_ORDERED)
  public void on(NoteConfigDiaTracer m) {
    Path existing = PathUtils.existing(m.path);
    if (existing != null) {
      Fragpipe.propsVarSet(ThisAppProps.PROP_BINARIES_IN, existing.toString());
      if (existing.toString().contains(" ")) {
        SwingUtils.showErrorDialog(this, "Space is not allowed in diaTracer path.", "Spaces in path");
      }
    }

    if (m.ex != null) {
      epDiaTracerVer.setText("diaTracer version: N/A");
      showConfigError(m.ex, TIP_DIATRACER_BIN, uiTextToolsFolder, true);
    } else if (m.isTooOld) {
      epDiaTracerVer.setText("diaTracer version: too old, not supported anymore");
      Bus.post(new MessageBalloon(TIP_DIATRACER_BIN, uiTextToolsFolder, "diaTracer " + diatracerMinVersion + " is required.", true));
    } else {
      epDiaTracerVer.setText("diaTracer version: " + m.version);
      Notifications.tryClose(TIP_DIATRACER_BIN);
    }
    if (m.isValid() && !m.isTooOld) {
      DiaTracer.checkUpdates(m);
    }
  }

  @Subscribe
  public void on(MessageUiRevalidate m) {
    log.debug("Got MessageUiRevalidate");
    if (m.updateBins) {
      Bus.post(new MessageMsfraggerNewBin(getJarPath(uiTextToolsFolder.getNonGhostText().trim(), msfraggerRegex)));
      Bus.post(new MessageIonQuantNewBin(getJarPath(uiTextToolsFolder.getNonGhostText().trim(), ionquantRegex)));
      Bus.post(new MessageDiaTracerNewBin(getJarPath(uiTextToolsFolder.getNonGhostText().trim(), diatracerRegex)));

      String binDiann = uiTextBinDiann.getNonGhostText();
      if (StringUtils.isNotBlank(binDiann)) {
        Bus.post(new MessageDiannNewBin(binDiann));
      } else {
        Bus.post(new MessageDiannNewBin());
      }
      String binPython = uiTextBinPython.getNonGhostText();
      if (StringUtils.isNotBlank(binPython)) {
        Bus.post(new MessagePythonNewBin(binPython));
      } else {
        // python was not loaded, try finding system python
        Bus.post(new MessageFindSystemPython());
      }
    }
    Fragpipe.loadWorkflowDone.countDown();
  }

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void on(MessageFindSystemPython m) {
    try {
      PyInfo pi = PyInfo.findSystemPython(pythonMinVersion);
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

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void on(MessagePythonNewBin m) {
    PyInfo pi;
    try {
      List<Path> pyPaths = List.of(Path.of("invalid"));
      if (OsUtils.isWindows()) {
        pyPaths = FragpipeLocations.checkToolsMissing(Seq.of(PyInfo.pythonWinPath));
      } else if (OsUtils.isUnix()) {
        pyPaths = FragpipeLocations.checkToolsMissing(Seq.of(PyInfo.pythonLinuxPath));
      } else {
        throw new RuntimeException("FragPipe only works in Windows and Linux. FragPipe not supported in this OS");
      }
      final var command = pyPaths.get(0).toString();
      m = new MessagePythonNewBin(command);
      // first check if the path is absolute, then it must exist
      Path path = Paths.get(m.command);
      final boolean fileExists = Files.exists(path) || (OsUtils.isWindows() && Files.exists(Paths.get(path + ".exe")));
      if ((path.isAbsolute() && !fileExists) || StringUtils.isBlank(path.toString())) {
        throw new ValidationException("File does not exist: \"" + path + "\"");
      }

      // if paths.get didn't throw, we can try the binary, it might be on PATH
      pi = PyInfo.fromCommand(m.command);
      if (pi.getFullVersion().compareTo(pythonMinVersion) < 0) {
        if (Fragpipe.headless) {
          log.debug("headless mode: not checking python version on call from TabConfig");
        } else {
          Bus.postSticky(new NoteConfigPython(pi, new ValidationException("Python version " + pythonMinVersion + "+ required"), pi.getCommand(), pi.getVersion()));
        }
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
      showConfigError(m.ex, TIP_PYTHON_BIN, uiTextBinPython, false);
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
        showConfigError(m.ex, TIP_DBSPLIT, epDbsplitText, false);
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

  private String textEasyPQP(String easypqpLocalVersion, String easypqpLatestVersion, String pandasLocalVersion, String numpyLocalVersion, boolean enableEasypqp, String errMsg) {
    StringBuilder sb = new StringBuilder();
    if (enableEasypqp && !easypqpLocalVersion.contentEquals("N/A")) {
      if (!easypqpLatestVersion.contentEquals("N/A") && VersionComparator.cmp(easypqpLocalVersion, easypqpLatestVersion) < 0) {
        sb.append("EasyPQP: <b>Available</b>. Version: " + easypqpLocalVersion + "<br>"
            + "<p style=\"color:red\">There is a new version (" + easypqpLatestVersion + "). Please upgrade it by clicking the button below and waiting.<br>");
        sb.append("Pandas: <b>Available</b>. Version: ").append(pandasLocalVersion).append("<br>");
        sb.append("Numpy: <b>Available</b>. Version: ").append(numpyLocalVersion).append("<br>");
      } else {
        sb.append("EasyPQP: <b>Available</b>. Version: " + easypqpLocalVersion + "<br>");
        sb.append("Pandas: <b>Available</b>. Version: ").append(pandasLocalVersion).append("<br>");
        sb.append("Numpy: <b>Available</b>. Version: ").append(numpyLocalVersion).append("<br>");
      }
    } else {
      if (errMsg.isEmpty()) {
        sb.append("EasyPQP: <b>Not available</b><br>"
            + "Please make sure that Python is installed, and then click the button below and wait.<br>");
      } else {
        sb.append("EasyPQP: <b>Not available</b><br>"
            + "Please make sure that Python is installed, and then click the button below and wait.<br>").append(errMsg);
      }
    }
    return sb.toString();
  }

  @Subscribe(sticky = true, threadMode = ThreadMode.MAIN_ORDERED)
  public void on(NoteConfigSpeclibgen m) {
    epSpeclibgenParent = epEasyPQPText.getParent();
    epEasyPQPText.setVisible(true);
    if (m.ex != null) {
      log.debug("Got NoteConfigSpeclibgen with exception set");
      epEasyPQPText.setText(textEasyPQP("N/A", "N/A", "N/A", "N/A", false, m.ex.getMessage()));
      showConfigError(m.ex, TIP_SPECLIBGEN, epEasyPQPText, false);
      this.revalidate();
      return;
    }

    if (m.instance == null) {
      throw new IllegalStateException("If no exception is reported from Speclibgen init, instance should not be null");
    }

    try {
      if (m.instance.isEasypqpOk()) {
        final ProcessBuilder pb = new ProcessBuilder(m.instance.getPython().getCommand(), "-c",
                "import importlib.metadata\n" +
                        "try:\n" +
                        "    print(importlib.metadata.version('easypqp'))\n" +
                        "except importlib.metadata.PackageNotFoundError:\n" +
                        "    print('N/A')"
        );
        m.easypqpLocalVersion = ProcessUtils.captureOutput(pb);
      }
    } catch (Exception ex) {
      m.easypqpLocalVersion = "N/A";
    }

    try {
      if (m.instance.isEasypqpOk()) {
        final ProcessBuilder pb = new ProcessBuilder(m.instance.getPython().getCommand(), "-c",
            "import importlib.metadata\n" +
                "try:\n" +
                "    print(importlib.metadata.version('pandas'))\n" +
                "except importlib.metadata.PackageNotFoundError:\n" +
                "    print('N/A')"
        );
        m.pandasLocalVersion = ProcessUtils.captureOutput(pb);
      }
    } catch (Exception ex) {
      m.pandasLocalVersion = "N/A";
    }

    try {
      if (m.instance.isEasypqpOk()) {
        final ProcessBuilder pb = new ProcessBuilder(m.instance.getPython().getCommand(), "-c",
            "import importlib.metadata\n" +
                "try:\n" +
                "    print(importlib.metadata.version('numpy'))\n" +
                "except importlib.metadata.PackageNotFoundError:\n" +
                "    print('N/A')"
        );
        m.numpyLocalVersion = ProcessUtils.captureOutput(pb);
      }
    } catch (Exception ex) {
      m.numpyLocalVersion = "N/A";
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

    epEasyPQPText.setText(textEasyPQP(m.easypqpLocalVersion, easypqpLatestVersion, m.pandasLocalVersion, m.numpyLocalVersion, true, ""));

    this.revalidate();
  }

  private void showConfigError(Throwable e, String balloonTopic, JComponent balloonParent, boolean exitHeadless) {
    if (e instanceof ValidationException) {
      Bus.post(new MessageBalloon(balloonTopic, balloonParent, e.getMessage(), exitHeadless));
    } else {
      SwingUtils.showErrorDialogWithStacktrace(e, this);
    }
  }

  private static String createOthersCitationBody() {
    final StringBuilder sb = new StringBuilder();

    sb.append("<p style=\"margin-top: 0\">")
        .append("More info and docs: <a href=\"").append("https://diaumpire.nesvilab.org/").append("\">DIA-Umpire</a>")
        .append(", <a href=\"").append("https://www.nesvilab.org/Crystal-C/").append("\">Crystal-C</a>")
        .append(", <a href=\"").append("https://github.com/Nesvilab/MSBooster").append("\">MSBooster</a>")
        .append(", <a href=\"").append("http://percolator.ms/").append("\">Percolator</a>")
        .append(", <a href=\"").append("http://www.tppms.org/tools/ptm/").append("\">PTMProphet</a>")
        .append(", <a href=\"").append("https://ptmshepherd.nesvilab.org/").append("\">PTM-Shepherd</a>")
        .append(", <a href=\"").append("https://github.com/lonelu/PTMLocalization").append("\">O-Pair</a>")
        .append(", <a href=\"").append("https://tmt-integrator.nesvilab.org/").append("\">TMT-Integrator</a>")
        .append(", <a href=\"").append("https://github.com/grosenberger/easypqp").append("\">EasyPQP</a>")
        .append(", <a href=\"").append("https://github.com/Nesvilab/FragPipe-PDV").append("\">FragPipe-PDV</a>")
        .append(", <a href=\"").append("https://skyline.ms/project/home/software/Skyline/begin.view").append("\">Skyline</a>")
        .append(", <a href=\"").append("https://saint-apms.sourceforge.net/Main.html").append("\">SAINT</a>");
    return sb.toString();
  }

  private static String createFraggerCitationBody() {
    final StringBuilder sb = new StringBuilder();
    sb.append("&emsp;More info and docs: <a href=\"").append("https://msfragger.nesvilab.org/").append("\">MSFragger</a>");
    return sb.toString();
  }

  private static String createIonQuantCitationBody() {
    final StringBuilder sb = new StringBuilder();
    sb.append("&emsp;More info and docs: <a href=\"").append("https://ionquant.nesvilab.org/").append("\">IonQuant</a>");
    return sb.toString();
  }

  private static String createDiaTracerCitationBody() {
    final StringBuilder sb = new StringBuilder();
    sb.append("&emsp;More info and docs: <a href=\"").append("https://diatracer.nesvilab.org/").append("\">diaTracer</a>");
    return sb.toString();
  }

  private JPanel createPanelDiann() {
    JPanel p = newMigPanel();
    p.setBorder(new TitledBorder("DIA-NN"));

    final String tip = "Select path to DIA-NN binary";
    uiTextBinDiann = UiUtils.uiTextBuilder().create();
    uiTextBinDiann.addFocusListener(new ContentChangedFocusAdapter(uiTextBinDiann, (s, s2) -> {
      Bus.post(new MessageDiannNewBin(s2));
    }));
    FormEntry feBin = fe(uiTextBinDiann, "bin-diann", TAB_PREFIX).tooltip(tip).create();
    p.add(feBin.comp, ccL().split().growX());

    JButton btnBrowse = feBin.browseButton("Browse", tip, this::createDiannFilechooser, paths -> paths.stream().findFirst().ifPresent(bin -> Bus.post(new MessageDiannNewBin(bin.toString()))));
    p.add(btnBrowse, ccL());
    JButton btnDownload = UiUtils.createButton("Download", this::actionDiannDownload);
    p.add(btnDownload, ccL().wrap());

    p.add(SwingUtils.createClickableHtml("Path to DIA-NN executable file. The file name is <b>DiaNN.exe</b>. Do not use DIA-NN.exe or the installer file. If not customized, use the built-in version " + Diann.fallBackDiannVersion), ccL().spanX().growX().wrap());

    epDiannVer = new HtmlStyledJEditorPane("DIA-NN version: N/A");
    p.add(Fragpipe.rename(epDiannVer, "diann.version-info", TAB_PREFIX, true), ccL().split());
    p.add(SwingUtils.createClickableHtml(createDiannCitationBody()), ccL().spanX().growX().wrap());
    return p;
  }

  private JPanel createPanelPython() {
    JPanel p = newMigPanel();
    p.setBorder(new TitledBorder("Python"));
    final String tip = "Python " + pythonMinVersion + "+ is required for Spectral Library generation and Database Splitting";
    final String ghost = "Select Python " + pythonMinVersion + "+ binary";
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

  @Subscribe(sticky = true, threadMode = ThreadMode.MAIN_ORDERED)
  public void on(MessageInstallEasyPQP m) {
    String binPython = "";
    String pythonPipOutputNew = "";
    boolean ok = true;
    try {
      NoteConfigPython noteConfigPython = Fragpipe.getSticky(NoteConfigPython.class);
      if (noteConfigPython.isValid()) {
        binPython = noteConfigPython.command;
      } else {
        pythonPipOutputNew += noteConfigPython.ex.toString();
        ok = false;
      }
    } catch (NoStickyException ex) {
      pythonPipOutputNew += ex.toString();
      ok = false;
    }

    if (StringUtils.isNotBlank(binPython)) {
      try {
        pythonPipOutputNew += ProcessUtils.captureOutput(new ProcessBuilder(binPython, "-m", "pip", "uninstall", "--yes", "easypqp"));
      } catch (Exception ex) {
        pythonPipOutputNew += ex.toString();
        ok = false;
      }
      try {
        pythonPipOutputNew += ProcessUtils.captureOutput(new ProcessBuilder(binPython, "-m", "pip", "uninstall", "--yes", "pyopenms"));
      } catch (Exception ex) {
        pythonPipOutputNew += ex.toString();
        ok = false;
      }
      final ProcessBuilder pb2 = new ProcessBuilder(binPython, "-m", "pip", "install", "easypqp", "lxml");
      PyInfo.modifyEnvironmentVariablesForPythonSubprocesses(pb2); // without this, on Windows, will fail with an error related to TLS/SSL
      try {
        pythonPipOutputNew += ProcessUtils.captureOutput(pb2);
      } catch (Exception ex) {
        pythonPipOutputNew += ex.toString();
        ok = false;
      }
      SwingUtils.showInfoDialog(this, pythonPipOutputNew, "EasyPQP install " + (ok ? "success" : "fail"));
    } else {
      SwingUtils.showInfoDialog(this, pythonPipOutputNew, "EasyPQP install fail");
    }

    toConsole(pythonPipOutputNew, m.console);
    Bus.post(new MessageUiRevalidate(false, true));
  }

  private JPanel createPanelDbsplit() {
    JPanel p = mu.newPanel("Database Splitting", true);

    StringBuilder tip = new StringBuilder()
        .append("Used for searching very large databases by splitting into smaller chunks.<br/>")
        .append("Requires <b>Python " + pythonMinVersion + "+</b> with packages <b>Numpy, Pandas</b>.\n")
        .append("Ways to get everything set up:").append("<ul>")
        .append("<li>Install Python " + pythonMinVersion + "+ if you don't yet have it.</li>")
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
    epDbsplitErr = new HtmlStyledJEditorPane("Requires Python " + pythonMinVersion + "+ with modules Numpy and Pandas.");
    epDbsplitErr.setPreferredSize(dim);

    mu.add(p, epDbsplitText).growX().pushX().wrap();
    mu.add(p, epDbsplitErr).growX().pushX().wrap();

    return p;
  }

  private JPanel createPanelSpeclibgen() {
    JPanel p = mu.newPanel("Spectral Library Generation", true);

    p.setToolTipText(SwingUtils.makeHtml("EasyPQP: Requires <b>Python " + pythonMinVersion + "+</b> with package <b>EasyPQP</b> and <b>lxml</b>"));
    Dimension dim = new Dimension(200, 25);

    epEasyPQPText = new HtmlStyledJEditorPane("Configuring EasyPQP.");
    epEasyPQPText.setToolTipText(SwingUtils.makeHtml("EasyPQP: Requires <b>Python " + pythonMinVersion + "+</b> with package <b>EasyPQP</b> and <b>lxml</b>"));
    epEasyPQPText.setPreferredSize(dim);

    final JButton btnInstallEasyPQP = UiUtils.createButton("Install/Upgrade EasyPQP", e -> Bus.post(new MessageInstallEasyPQP(console)));

    mu.add(p, epEasyPQPText).growX().wrap();
    mu.add(p, btnInstallEasyPQP).split().wrap();

    return p;
  }

  private JFileChooser createDiannFilechooser() {
    JFileChooser fc = FileChooserUtils.create("Select DIA-NN binary", "Select", false, FcMode.FILES_ONLY, true);
    if (OsUtils.isWindows()) {
      fc.addChoosableFileFilter(new FileNameExtensionFilter("Executables", "exe"));
    }
    FileChooserUtils.setPath(fc, Stream.of(uiTextBinDiann.getNonGhostText(), Fragpipe.propsVarGet(ThisAppProps.PROP_BINARIES_IN), JarUtils.getCurrentJarPath()));
    return fc;
  }

  private JFileChooser createPythonFilechooser() {
    JFileChooser fc = FileChooserUtils.create("Select Python " + pythonMinVersion + "+ binary", "Select",
        false, FcMode.FILES_ONLY, true);
    if (OsUtils.isWindows()) {
      fc.addChoosableFileFilter(new FileNameExtensionFilter("Executables", "exe"));
    }

    PyInfo sysPy = null;
    try {
      sysPy = PyInfo.findSystemPython(pythonMinVersion);
    } catch (UnexpectedException e) {
      log.debug("Something happened while checking system python for Python bin file chooser", e);
    }

    Stream<String> pathsToCheck = Stream
        .of(uiTextBinPython.getNonGhostText(), sysPy == null ? "" : sysPy.getCommand())
        .filter(StringUtils::isNotBlank)
        .flatMap(text -> {
          try {
            Path p = Paths.get(text);
            return p.isAbsolute() ? Stream.of(p, p.toAbsolutePath().getParent()) : Stream.of(p);
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

  private String createDiannCitationBody() {
    StringBuilder sb = new StringBuilder();
    sb.append("&emsp;More info and docs: <a href=\"https://github.com/vdemichev/DiaNN\">DIA-NN</a>");
    return sb.toString();
  }

  private void actionDiannDownload(ActionEvent e) {
    Diann.downloadDiannManually();
  }
}
