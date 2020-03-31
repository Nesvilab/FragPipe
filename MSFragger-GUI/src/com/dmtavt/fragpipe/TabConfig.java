package com.dmtavt.fragpipe;

import static com.dmtavt.fragpipe.Fragpipe.fe;

import com.dmtavt.fragpipe.api.BalloonTips;
import com.dmtavt.fragpipe.api.Bus;
import com.dmtavt.fragpipe.exceptions.UnexpectedException;
import com.dmtavt.fragpipe.exceptions.ValidationException;
import com.dmtavt.fragpipe.messages.MessageBalloon;
import com.dmtavt.fragpipe.messages.MessageClearCache;
import com.dmtavt.fragpipe.messages.MessageMsfraggerNewBin;
import com.dmtavt.fragpipe.messages.MessageMsfraggerUpdateAvailable;
import com.dmtavt.fragpipe.messages.MessagePhilosopherNewBin;
import com.dmtavt.fragpipe.messages.MessagePythonNewBin;
import com.dmtavt.fragpipe.messages.MessageShowAboutDialog;
import com.dmtavt.fragpipe.messages.MessageUiInitDone;
import com.dmtavt.fragpipe.messages.MessageUiStateLoaded;
import com.dmtavt.fragpipe.messages.MessageUmpireEnabled;
import com.dmtavt.fragpipe.messages.NoteMsfraggerConfig;
import com.dmtavt.fragpipe.messages.NotePhilosopherConfig;
import com.dmtavt.fragpipe.tools.msfragger.Msfragger;
import com.dmtavt.fragpipe.tools.msfragger.Msfragger.Version;
import com.dmtavt.fragpipe.tools.philosopher.Philosopher;
import com.github.chhh.utils.JarUtils;
import com.github.chhh.utils.OsUtils;
import com.github.chhh.utils.PythonInfo;
import com.github.chhh.utils.StringUtils;
import com.github.chhh.utils.swing.FileChooserUtils;
import com.github.chhh.utils.swing.FileChooserUtils.FcMode;
import com.github.chhh.utils.swing.FormEntry;
import com.github.chhh.utils.swing.FormEntry.Builder;
import com.github.chhh.utils.swing.UiCheck;
import com.github.chhh.utils.swing.UiText;
import com.github.chhh.utils.swing.UiUtils;
import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Desktop;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.Paths;
import java.util.Properties;
import java.util.function.Supplier;
import java.util.stream.Stream;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JEditorPane;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.border.TitledBorder;
import javax.swing.filechooser.FileNameExtensionFilter;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.ThreadMode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import umich.msfragger.gui.MsfraggerGuiFrameUtils;
import umich.msfragger.params.ThisAppProps;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.swing.JPanelWithEnablement;
import umich.msfragger.params.fragger.MsfraggerProps;

public class TabConfig extends JPanelWithEnablement {

  private static final Logger log = LoggerFactory.getLogger(TabConfig.class);

  private UiText uiTextBinFragger;
  private JEditorPane epFraggerVer;
  private UiText uiTextBinPhi;
  private JEditorPane epPhiVer;
  private UiText uiTextBinPython;
  public static final String TIP_MSFRAGGER_BIN = "tip.msfragger.bin";
  public static final String TIP_PHILOSOPHER_BIN = "tip.pholosopher.bin";
  public static final String PREFIX_CONFIG = "fragpipe-config.";


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
    add(createPanelBottomInfo(), new CC().growX().wrap());
    add(createPanelBottomLink(), new CC().growX().wrap());
  }

  private JLabel createPanelBottomInfo() {
    JLabel c = new JLabel();
    c.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
    c.setAlignmentX(Component.CENTER_ALIGNMENT);
    c.setText(SwingUtils.makeHtml(
        "Tabs on top represent processing steps and will be performed sequentially.\n"
            + "Tabs will become enabled once the tools on this panel are configured."));
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
    Supplier<CC> ccL = () -> new CC().alignX("left");
    Supplier<CC> ccR = () -> new CC().alignX("right");
    p.add(UiUtils.createButton("About", e -> Bus.post(new MessageShowAboutDialog())),
        ccL.get().split().spanX());
    p.add(UiUtils.createButton("Clear Cache", e -> Bus.post(new MessageClearCache())), ccL.get());
    UiCheck uiCheckUmpire = UiUtils.createUiCheck("Enable DIA-Umpire", false,
        e -> Bus.post(new MessageUmpireEnabled(((JCheckBox) e.getSource()).isSelected())));
    p.add(uiCheckUmpire, ccL.get());
    JLabel sysInfo = new JLabel(SwingUtils.makeHtml(OsUtils.OsInfo() + "\n" + OsUtils.JavaInfo()));
    sysInfo.setVerticalAlignment(JLabel.TOP);
    p.add(sysInfo, ccR.get().wrap());
    //p.add(UiUtils.createButton("Find tools", e -> post(new MessageFindTools())), ccL.get().split().spanX());
    JLabel label = new JLabel("Main tools configuration");
    label.setFont(new Font(label.getFont().getName(), Font.BOLD, label.getFont().getSize() + 3));
    p.add(label, ccL.get().wrap());
    return p;
  }

  private JPanel createPanelFragger() {
    JPanel p = newMigPanel();
    p.setBorder(new TitledBorder("MSFragger"));

    final String binMsfraggerTip = "Select path to MSFragger.jar";
    uiTextBinFragger = UiUtils.uiTextBuilder().ghost(binMsfraggerTip).create();
    FormEntry feBinMsfragger = fe(uiTextBinFragger, "bin-msfragger", PREFIX_CONFIG)
        .tooltip(binMsfraggerTip).create();
    p.add(feBinMsfragger.comp, ccL().split().growX());

    JButton btnBrowse = feBinMsfragger
        .browseButton("Browse", this::createFraggerFilechooser, binMsfraggerTip, paths -> {
          paths.stream().findFirst()
              .ifPresent(jar -> Bus.post(new MessageMsfraggerNewBin(jar.toString())));
        });
    p.add(btnBrowse, ccL());
    JButton btnUpdate = UiUtils.createButton("Update", this::actionMsfraggerUpdate);
    JButton btnDownload = UiUtils.createButton("Download", this::actionMsfraggerDownload);

    btnUpdate.setToolTipText(SwingUtils.makeHtml("Open MSFragger upgrader tool in browser.\n" +
        "In order to update you <b>must</b> download an\n" +
        "original copy from the <b>download</b> website once."));
    p.add(btnUpdate, ccL());
    p.add(btnDownload, ccL().wrap());
    epFraggerVer = SwingUtils.createClickableHtml("MSFragger version: N/A");
    p.add(Fragpipe.rename(epFraggerVer, "msfragger.version-info", PREFIX_CONFIG, true),
        ccL().spanX().growX().wrap());
    p.add(SwingUtils.createClickableHtml(createFraggerCitationBody()),
        ccL().spanX().growX().wrap());
    return p;
  }

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void onMsfraggerUpdateAvailable(MessageMsfraggerUpdateAvailable m) {
    StringBuilder sb = new StringBuilder();
    sb.append(
        String.format("There is a newer version of MSFragger available [%s].<br>\n", m.newVersion));

    JEditorPane ep = SwingUtils.createClickableHtml(sb.toString(), BalloonTips.BG_COLOR);
    JPanel content = new JPanel(new BorderLayout());
    content.setBackground(ep.getBackground());

    JPanel pBtns = new JPanel();
    pBtns.setBackground(ep.getBackground());
    pBtns.setLayout(new FlowLayout(FlowLayout.LEFT, 5, 5));

    if (!StringUtils.isNullOrWhitespace(m.manualDownloadUrl)) {
      JButton btnManualUpdate = new JButton("Download update");
      btnManualUpdate.addActionListener(e -> {
        try {
          SwingUtils.openBrowserOrThrow(new URI(m.manualDownloadUrl));
        } catch (URISyntaxException ex) {
          throw new IllegalStateException("Incorrect url/uri", ex);
        }
      });
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
    //return new JPanel(new MigLayout(new LC().fillX().insetsAll("0px")));
    return new JPanel(
        new MigLayout(new LC().fillX())); // TODO: check if setting insets makes a big difference
  }

  private void actionMsfraggerUpdate(ActionEvent evt) {
    try {
      String url = MsfraggerProps.getProperties()
          .getProperty(MsfraggerProps.PROP_UPDATESERVER_WEBSITE_URL);
      Desktop.getDesktop().browse(URI.create(url));
    } catch (IOException ex) {
      throw new IllegalStateException("Could not open MSFragger update link in browser.", ex);
    }
  }

  private void actionMsfraggerDownload(ActionEvent e) {
    try {
      final String downloadUrl = MsfraggerProps.getProperties()
          .getProperty(MsfraggerProps.PROP_DOWNLOAD_URL, "");
      Desktop.getDesktop().browse(URI.create(downloadUrl));
    } catch (IOException ex) {
      throw new IllegalStateException("Could not open MSFragger download link in browser.", ex);
    }
  }

  private JFileChooser createFraggerFilechooser() {
    JFileChooser fc = FileChooserUtils.create("Select MSFragger jar", "Select",
        false, FcMode.FILES_ONLY, true,
        new FileNameExtensionFilter("JAR files", "jar"));
    FileChooserUtils.setPath(fc, Stream.of(
        uiTextBinFragger.getNonGhostText(),
        ThisAppProps.load(ThisAppProps.PROP_BINARIES_IN),
        JarUtils.getCurrentJarPath()));
    return fc;
  }

  @Subscribe(threadMode = ThreadMode.ASYNC)
  public void onPhilosopherNewBin(MessagePhilosopherNewBin m) {
    if (StringUtils.isBlank(m.path)) {
      return;
    }
    try {
      Philosopher.Version v = Philosopher.validate(m.path);
      String version = v.version + (StringUtils.isBlank(v.build) ? "" : " (build " + v.build + ")");
      Bus.postSticky(new NotePhilosopherConfig(m.path, version));
    } catch (ValidationException | UnexpectedException e) {
      Bus.postSticky(new NotePhilosopherConfig(m.path, "N/A", e));
    }
  }

  @Subscribe
  public void onPhilosopherConfig(NotePhilosopherConfig m) {
    log.debug("Got {}", m);
    uiTextBinPhi.setText(m.path);

    if (m.validation != null) {
      SwingUtils.setJEditorPaneContent(epPhiVer, "Philosopher version: N/A");
      if (m.validation instanceof ValidationException) {
        Bus.post(new MessageBalloon(TIP_PHILOSOPHER_BIN, uiTextBinPhi, m.validation.getMessage()));
      } else {
        SwingUtils.showErrorDialogWithStacktrace(m.validation, this);
      }
    } else {
      SwingUtils.setJEditorPaneContent(epPhiVer, "Philosopher version: " + m.version);
    }
  }


  @Subscribe(threadMode = ThreadMode.ASYNC)
  public void onMsfraggerNewBin(MessageMsfraggerNewBin m) {
    if (StringUtils.isBlank(m.binPath)) {
      return;
    }
    Version v;
    try {
      Msfragger.validateJar(m.binPath);
      v = Msfragger.version(Paths.get(m.binPath));
      if (v.isVersionParsed) {
        Bus.postSticky(new NoteMsfraggerConfig(m.binPath, v.version));
      } else {
        Bus.postSticky(new NoteMsfraggerConfig(m.binPath, v.version, true, null));
      }
    } catch (ValidationException | UnexpectedException e) {
      Bus.postSticky(new NoteMsfraggerConfig(m.binPath, "N/A", e));
    }
  }

  @Subscribe
  public void onMsfraggerConfig(NoteMsfraggerConfig m) {
    log.debug("Got {}", m);
    uiTextBinFragger.setText(m.path);

    if (m.validation != null) {
      SwingUtils.setJEditorPaneContent(epFraggerVer, "MSFragger version: N/A");
      if (m.validation instanceof ValidationException) {
        Bus.post(
            new MessageBalloon(TIP_MSFRAGGER_BIN, uiTextBinFragger, m.validation.getMessage()));
      } else {
        SwingUtils.showErrorDialogWithStacktrace(m.validation, this);
      }
    } else if (m.isTooOld) {
      SwingUtils
          .setJEditorPaneContent(epFraggerVer, "MSFragger version: too old, not supported anymore");
      Bus.post(new MessageBalloon(TIP_MSFRAGGER_BIN, uiTextBinFragger,
          "This version is not supported anymore.\n"
              + "Download a newer one."));
    } else {
      SwingUtils.setJEditorPaneContent(epFraggerVer, "MSFragger version: " + m.version);
    }
    if (m.isValid()) {
      Msfragger.checkUpdates(m);
    }
  }

  @Subscribe
  public void onUiStateLoaded(MessageUiStateLoaded m) {
    log.debug("Got MessageUiStateLoaded");
    String binFragger = uiTextBinFragger.getNonGhostText();
    if (StringUtils.isNotBlank(binFragger)) {
      Bus.post(new MessageMsfraggerNewBin(binFragger));
    }
    String binPhi = uiTextBinPhi.getNonGhostText();
    if (StringUtils.isNotBlank(binPhi)) {
      Bus.post(new MessagePhilosopherNewBin(binPhi));
    }
  }

  public static String createFraggerCitationHtml(Font font) {
    return SwingUtils.wrapInStyledHtml(createFraggerCitationBody(), font);
  }

  private static String createFraggerCitationBody() {
    final Properties p = ThisAppProps.getRemotePropertiesWithLocalDefaults();
    final String linkMsfragger = p
        .getProperty(MsfraggerProps.PROP_FRAGGER_SITE_URL, "https://nesvilab.github.io/MSFragger/");
    final String linkFragpipe = p
        .getProperty(ThisAppProps.PROP_FRAGPIPE_SITE_URL, "https://github.com/Nesvilab/FragPipe");
    final String doi = p.getProperty(ThisAppProps.PROP_MANUSCRIPT_DOI, "10.1038/nmeth.4256");
    final String linkManuscript = p.getProperty(ThisAppProps.PROP_MANUSCRIPT_URL,
        "http://www.nature.com/nmeth/journal/v14/n5/full/nmeth.4256.html");
    final StringBuilder sb = new StringBuilder();

    sb.append("<p style=\"margin-top: 0\">");
    sb.append("<b>Please cite: </b>");
    sb.append(
        "<a href=\"").append(linkManuscript).append(
        "\">MSFragger: ultrafast and comprehensive peptide identification in mass spectrometry–based proteomics</a>");
    sb.append("<br/>");
    sb.append("<b>DOI: </b>").append(doi);
    sb.append("</p>");

    sb.append("<p style=\"margin-top: 10\">");
    sb.append("More info and docs: <a href=\"").append(linkMsfragger)
        .append("\">MSFragger website</a>")
        .append(", <a href=\"").append(linkFragpipe).append("\">FragPipe GitHub page</a>");
    return sb.toString();
  }

  private JPanel createPanelPhilosopher() {
    JPanel p = newMigPanel();
    p.setBorder(new TitledBorder("Philosopher"));

    final String tip = "Select path to Philosopher binary";
    uiTextBinPhi = UiUtils.uiTextBuilder().ghost(tip).create();
    FormEntry feBin = fe(uiTextBinPhi, "bin-philosopher", PREFIX_CONFIG)
        .tooltip(tip).create();
    p.add(feBin.comp, ccL().split().growX());

    JButton btnBrowse = feBin
        .browseButton("Browse", this::createPhilosopherFilechooser, tip,
            paths -> paths.stream().findFirst()
                .ifPresent(bin -> Bus.post(new MessagePhilosopherNewBin(bin.toString()))));
    p.add(btnBrowse, ccL());
    JButton btnUpdate = UiUtils.createButton("Update", this::actionPhilosopherDownload);
    JButton btnDownload = UiUtils.createButton("Download", this::actionPhilosopherDownload);
    p.add(btnUpdate, ccL());
    p.add(btnDownload, ccL().wrap());

    epPhiVer = SwingUtils.createClickableHtml("Philosopher version: N/A");
    p.add(Fragpipe.rename(epPhiVer, "philosopher.version-info", PREFIX_CONFIG, true),
        ccL().spanX().growX().wrap());
    p.add(SwingUtils.createClickableHtml(createPhilosopherCitationBody()),
        ccL().spanX().growX().wrap());
    return p;
  }

  private JPanel createPanelPython() {
    JPanel p = newMigPanel();
    p.setBorder(new TitledBorder("Python"));
    final String tip = "Python 3 is required for Spectral Library generation and DB splitting";
    final String ghost = "Select Python 3 binary (Anaconda Python recommended)";
    uiTextBinPython = UiUtils.uiTextBuilder().ghost(ghost).create();
    FormEntry fe = fe(uiTextBinPython, "bin-python", PREFIX_CONFIG).tooltip(tip).create();

    p.add(fe.comp, ccL().split().growX());
    JButton btnBrowse = fe.browseButton("Browse", this::createPythonFilechooser, ghost, paths ->
        paths.stream().findFirst()
            .ifPresent(bin -> Bus.post(new MessagePythonNewBin(bin.toString()))));
    p.add(btnBrowse, ccL());
    final String url = ThisAppProps.def().getProperty(ThisAppProps.PROP_PYTHON_DOWNLOAD_URL);
    if (StringUtils.isNotBlank(url)) {
      JButton btnDonwload = UiUtils.createButton("Download", e -> SwingUtils.openBrowserOrThrow(url));
      p.add(btnDonwload, ccL().wrap());
    }

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
        ThisAppProps.load(ThisAppProps.PROP_BINARIES_IN),
        JarUtils.getCurrentJarPath()));
    return fc;
  }

  private JFileChooser createPythonFilechooser() {
    JFileChooser fc = FileChooserUtils.create("Select Python 3 binary", "Select",
        false, FcMode.FILES_ONLY, true);
    if (OsUtils.isWindows()) {
      fc.addChoosableFileFilter(new FileNameExtensionFilter("Executables", "exe"));
    }
    try {
      PythonInfo.get().findPythonCommand();
    } catch (Exception e) { }
    FileChooserUtils.setPath(fc, Stream.of(
        uiTextBinPython.getNonGhostText()
    ));
    return fc;
  }

  private String createPhilosopherCitationBody() {
    StringBuilder sb = new StringBuilder();
    sb.append("<p style=\"margin-top: 0\">");
    sb.append(
        "More info: <a href=\"https://nesvilab.github.io/philosopher/\">Philosopher GitHub page</a>");
    sb.append("<br/>");
    sb.append("</p>");
    return sb.toString();
  }

  private void actionPhilosopherDownload(ActionEvent e) {
    MsfraggerGuiFrameUtils.downloadPhilosopher();
  }
}
