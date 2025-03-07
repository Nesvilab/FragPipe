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

package org.nesvilab.fragpipe.tabs;

import static org.nesvilab.fragpipe.Fragpipe.fe;
import static org.nesvilab.fragpipe.Fragpipe.headless;
import static org.nesvilab.fragpipe.Version.version;
import static org.nesvilab.fragpipe.messages.MessagePrintToConsole.toConsole;

import org.nesvilab.fragpipe.Fragpipe;
import org.nesvilab.fragpipe.FragpipeLocations;
import org.nesvilab.fragpipe.api.Bus;
import org.nesvilab.fragpipe.api.Notifications;
import org.nesvilab.fragpipe.api.PyInfo;
import org.nesvilab.fragpipe.dialogs.DownloadToolsPanel;
import org.nesvilab.fragpipe.exceptions.NoStickyException;
import org.nesvilab.fragpipe.exceptions.UnexpectedException;
import org.nesvilab.fragpipe.exceptions.ValidationException;
import org.nesvilab.fragpipe.messages.MessageBalloon;
import org.nesvilab.fragpipe.messages.MessageClearCache;
import org.nesvilab.fragpipe.messages.MessageDiaTracerNewBin;
import org.nesvilab.fragpipe.messages.MessageDiaTracerUpdateAvailable;
import org.nesvilab.fragpipe.messages.MessageDiannNewBin;
import org.nesvilab.fragpipe.messages.MessageFindSystemPython;
import org.nesvilab.fragpipe.messages.MessageInstallEasyPQP;
import org.nesvilab.fragpipe.messages.MessageIonQuantNewBin;
import org.nesvilab.fragpipe.messages.MessageIonQuantUpdateAvailable;
import org.nesvilab.fragpipe.messages.MessageMsfraggerNewBin;
import org.nesvilab.fragpipe.messages.MessageMsfraggerUpdateAvailable;
import org.nesvilab.fragpipe.messages.MessagePythonNewBin;
import org.nesvilab.fragpipe.messages.MessageShowAboutDialog;
import org.nesvilab.fragpipe.messages.MessageUiRevalidate;
import org.nesvilab.fragpipe.messages.NoteConfigDbsplit;
import org.nesvilab.fragpipe.messages.NoteConfigDiaTracer;
import org.nesvilab.fragpipe.messages.NoteConfigDiann;
import org.nesvilab.fragpipe.messages.NoteConfigIonQuant;
import org.nesvilab.fragpipe.messages.NoteConfigMsfragger;
import org.nesvilab.fragpipe.messages.NoteConfigPython;
import org.nesvilab.fragpipe.messages.NoteConfigSpeclibgen;
import org.nesvilab.fragpipe.messages.NoteFragpipeUpdate;
import org.nesvilab.fragpipe.params.ThisAppProps;
import org.nesvilab.fragpipe.tools.diann.Diann;
import org.nesvilab.fragpipe.tools.diatracer.DiaTracer;
import org.nesvilab.fragpipe.tools.diatracer.DiaTracerVersionFetcherServer;
import org.nesvilab.fragpipe.tools.fragger.Msfragger;
import org.nesvilab.fragpipe.tools.fragger.Msfragger.Version;
import org.nesvilab.fragpipe.tools.fragger.MsfraggerVersionFetcherServer;
import org.nesvilab.fragpipe.tools.ionquant.IonQuant;
import org.nesvilab.fragpipe.tools.ionquant.IonQuantVersionFetcherServer;
import org.nesvilab.utils.JarUtils;
import org.nesvilab.utils.OsUtils;
import org.nesvilab.utils.PathUtils;
import org.nesvilab.utils.ProcessUtils;
import org.nesvilab.utils.StringUtils;
import org.nesvilab.utils.SwingUtils;
import org.nesvilab.utils.swing.ContentChangedFocusAdapter;
import org.nesvilab.utils.swing.FileChooserUtils;
import org.nesvilab.utils.swing.FileChooserUtils.FcMode;
import org.nesvilab.utils.swing.FormEntry;
import org.nesvilab.utils.swing.HtmlStyledJEditorPane;
import org.nesvilab.utils.swing.JPanelWithEnablement;
import org.nesvilab.utils.swing.MigUtils;
import org.nesvilab.utils.swing.TextConsole;
import org.nesvilab.utils.swing.UiText;
import org.nesvilab.utils.swing.UiUtils;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.image.BufferedImage;
import java.io.IOException;
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
import javax.swing.JScrollPane;
import javax.swing.border.TitledBorder;
import javax.swing.filechooser.FileNameExtensionFilter;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.apache.commons.text.WordUtils;
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
  private HtmlStyledJEditorPane epEasyPQPText;
  private JButton btnFinishPythonInstall;
  private TextConsole pythonTextConsole;
  private JScrollPane pythonTextScroll;
  private boolean dbsplitEnabled = false, easyPQPEnabled = false;
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
            return map.lastEntry().getValue().toAbsolutePath().normalize().toString();
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
            + "FragPipe: v" + version(false)));
            //+ FragpipeLocations.get().getJarPath().toString()));

    sysInfo.setVerticalAlignment(JLabel.TOP);
    //p.add(UiUtils.createButton("Find tools", e -> post(new MessageFindTools())), ccL.get().split().spanX());

    JLabel imageLabel = new JLabel();
    try {
      BufferedImage image = ImageIO.read(Objects.requireNonNull(getClass().getResource("/org/nesvilab/fragpipe/icons/fragpipe-128.png")));
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
    uiTextToolsFolder.setText(FragpipeLocations.get().getDirTools().toAbsolutePath().normalize().toString());
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
                Bus.post(new MessageMsfraggerNewBin(msfraggerPath.toAbsolutePath().normalize().toString()));
              } else {
                Bus.postSticky(new NoteConfigMsfragger("N/A", "N/A", null));
              }
            } catch (Exception ex) {
              Bus.postSticky(new NoteConfigMsfragger("N/A", "N/A", ex));
            }
          }).start();
        } else {
          Bus.post(new MessageMsfraggerNewBin(getJarPath(toolsPath.toAbsolutePath().normalize().toString(), msfraggerRegex)));
        }

        if (p.downloadIonQuant()) {
          IonQuantVersionFetcherServer ionQuantVersionFetcherServer = new IonQuantVersionFetcherServer(p.getName(), p.getEmail(), p.getInstitution(), p.wantReceiveEmail());
          new Thread(() -> {
            try {
              Path ionquantPath = ionQuantVersionFetcherServer.autoUpdate(toolsPath);
              if (ionquantPath != null) {
                Bus.post(new MessageIonQuantNewBin(ionquantPath.toAbsolutePath().normalize().toString()));
              } else {
                Bus.postSticky(new NoteConfigIonQuant("N/A", "N/A", false, false, new ValidationException("IonQuant path is null.")));
              }
            } catch (Exception ex) {
              Bus.postSticky(new NoteConfigIonQuant("N/A", "N/A", false, false, ex));
            }
          }).start();
        } else {
          Bus.post(new MessageIonQuantNewBin(getJarPath(toolsPath.toAbsolutePath().normalize().toString(), ionquantRegex)));
        }

        if (p.downloadDiaTracer()) {
          DiaTracerVersionFetcherServer diaTracerVersionFetcherServer = new DiaTracerVersionFetcherServer(p.getName(), p.getEmail(), p.getInstitution(), p.wantReceiveEmail());
          new Thread(() -> {
            try {
              Path diatracerPath = diaTracerVersionFetcherServer.autoUpdate(toolsPath);
              if (diatracerPath != null) {
                Bus.post(new MessageDiaTracerNewBin(diatracerPath.toAbsolutePath().normalize().toString()));
              } else {
                Bus.postSticky(new NoteConfigDiaTracer("N/A", "N/A", false, false, new ValidationException("diaTracer path is null.")));
              }
            } catch (Exception ex) {
              Bus.postSticky(new NoteConfigDiaTracer("N/A", "N/A", false, false, ex));
            }
          }).start();
        } else {
          Bus.post(new MessageDiaTracerNewBin(getJarPath(toolsPath.toAbsolutePath().normalize().toString(), diatracerRegex)));
        }

        uiTextToolsFolder.setText(toolsPath.toAbsolutePath().normalize().toString());
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
      if (true) {
        List<Path> pyPaths;
        if (OsUtils.isWindows()) {
          pyPaths = FragpipeLocations.checkToolsMissing(Seq.of(PyInfo.pythonWinPath));
        } else if (OsUtils.isUnix()) {
          pyPaths = FragpipeLocations.checkToolsMissing(Seq.of(PyInfo.pythonLinuxPath));
        } else {
          throw new RuntimeException("FragPipe only works in Windows and Linux. FragPipe not supported in this OS");
        }
        final var command = pyPaths.get(0).toString();
        m = new MessagePythonNewBin(command);
      }

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
      epDbsplitText.setText(textDbsplitEnabled(false));
      this.revalidate();
      return;
    }
    if (m.instance == null) {
      throw new IllegalStateException("If no exception is reported from DBSplit init, instance should not be null");
    }
    epDbsplitText.setText(textDbsplitEnabled(true));
    dbsplitEnabled = true;
    btnFinishPythonInstall.setVisible(!easyPQPEnabled);
    pythonTextScroll.setVisible(!easyPQPEnabled);
    this.revalidate();
  }

  private String textEasyPQP(String easypqpLocalVersion, boolean enableEasypqp, String errMsg) {
    StringBuilder sb = new StringBuilder();
    if (enableEasypqp && !easypqpLocalVersion.contentEquals("N/A")) {
      sb.append("EasyPQP: <b>Available</b>. Version: " + easypqpLocalVersion + ". Used for spectral library building.<br><br>");
      easyPQPEnabled = true;
      btnFinishPythonInstall.setVisible(!dbsplitEnabled);
      pythonTextScroll.setVisible(!dbsplitEnabled);
    } else {
      if (errMsg.isEmpty()) {
        sb.append("EasyPQP: <b>Not available</b>. Used for spectral library building.<br><br>");
      } else {
        sb.append("EasyPQP: <b>Not available</b>. Used for spectral library building.<br><br>").append(errMsg);
      }
    }
    return sb.toString();
  }

  @Subscribe(sticky = true, threadMode = ThreadMode.MAIN_ORDERED)
  public void on(NoteConfigSpeclibgen m) {
    epEasyPQPText.setVisible(true);
    if (m.ex != null) {
      log.debug("Got NoteConfigSpeclibgen with exception set");
      epEasyPQPText.setText(textEasyPQP("N/A", false, m.ex.getMessage()));
      showConfigError(m.ex, TIP_SPECLIBGEN, epEasyPQPText, false);
      this.revalidate();
      return;
    }

    if (m.instance == null) {
      throw new IllegalStateException("If no exception is reported from Speclibgen init, instance should not be null");
    }

    try {
      if (m.instance.isEasypqpOk()) {
        final ProcessBuilder pb = new ProcessBuilder(m.instance.getPython().getCommand(), "-Ic",
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
        final ProcessBuilder pb = new ProcessBuilder(m.instance.getPython().getCommand(), "-Ic",
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
        final ProcessBuilder pb = new ProcessBuilder(m.instance.getPython().getCommand(), "-Ic",
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

    epEasyPQPText.setText(textEasyPQP(m.easypqpLocalVersion, true, ""));

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
    epDbsplitText = new HtmlStyledJEditorPane(textDbsplitEnabled(false));
    epEasyPQPText = new HtmlStyledJEditorPane(textEasyPQP("N/A", false, ""));

    btnFinishPythonInstall = UiUtils.createButton("Finish Python Install", e -> Bus.post(new MessageInstallEasyPQP(console)));

    pythonTextConsole = new TextConsole(!true);
    final var currentFont = pythonTextConsole.getFont();
    pythonTextConsole.setFont(new Font(Font.MONOSPACED, currentFont.getStyle(), currentFont.getSize()));
    pythonTextConsole.setContentType("text/plain; charset=UTF-8");
    pythonTextScroll = SwingUtils.wrapInScroll(pythonTextConsole);
    pythonTextScroll.setMinimumSize(new Dimension(300, 50));
    pythonTextScroll.setMaximumSize(new Dimension(900, 150));
    pythonTextScroll.getViewport().setBackground(pythonTextConsole.getBackground());

    p.add(epPythonVer, ccL().wrap());
    mu.add(p, epDbsplitText).growX().wrap();
    mu.add(p, epEasyPQPText).growX().wrap();
    mu.add(p, btnFinishPythonInstall).split().wrap();
    mu.add(p, pythonTextScroll).grow().push().wrap();
    return p;
  }

  @Subscribe(sticky = true, threadMode = ThreadMode.BACKGROUND)
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

    final TextConsole c = pythonTextConsole;
    final Font currentFont = c.getFont();
    c.setFont(new Font(Font.MONOSPACED, currentFont.getStyle(), currentFont.getSize()));
    c.setContentType("text/plain; charset=UTF-8");

    if (StringUtils.isNotBlank(binPython)) {
      final Path pythonPackagesPath0;
      if (OsUtils.isWindows()) {
        pythonPackagesPath0 = Path.of(PyInfo.pythonWinPath).subpath(0, 1).resolve("python_packages");
      } else if (OsUtils.isUnix()) {
        pythonPackagesPath0 = Path.of(PyInfo.pythonLinuxPath).subpath(0, 1).resolve("python_packages");
      } else {
        throw new RuntimeException("FragPipe only works in Windows and Linux. FragPipe not supported in this OS");
      }
      final Path pythonPackagesPath = FragpipeLocations.checkToolsMissing(Seq.of(pythonPackagesPath0.toString())).get(0);

      final var pb2 = new ProcessBuilder(pythonPackagesPath.getParent().resolve("uv").toString(),
              "pip", "install", "--system", "--no-cache", "--no-deps", "-r", pythonPackagesPath.resolve("requirements.txt").toString(),
              "--no-index", "--find-links", pythonPackagesPath.toString());
      final var path_env_key = OsUtils.isWindows() ? "Path" : "PATH";
      pb2.environment().put(path_env_key, Path.of(binPython).getParent() + java.io.File.pathSeparator + pb2.environment().get(path_env_key));

      PyInfo.modifyEnvironmentVariablesForPythonSubprocesses(pb2); // without this, on Windows, will fail with an error related to TLS/SSL
      try {
        ProcessUtils.consumeLines(pb2, (s) -> {
          c.appendANSI(WordUtils.wrap(s, 100, "\n↦", false));
          c.appendANSI("\n");
          return true;
        });
      } catch (Exception ex) {
        c.appendANSI(ex.toString());
        ok = false;
      }
    }
    SwingUtils.showInfoDialog(this, pythonPipOutputNew+"\n"+"Python software install " + (ok ? "success" : "fail"), "Python software install ");
    toConsole(c.getText(), m.console);
    if (ok) {
      c.setText("Python software install success!");
      Notifications.tryClose(TIP_SPECLIBGEN);
    }

    btnFinishPythonInstall.setVisible(!ok);
    pythonTextScroll.setVisible(!ok);

    Bus.post(new MessageUiRevalidate(false, true));
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
