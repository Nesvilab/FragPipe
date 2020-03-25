package com.dmtavt.fragpipe;

import com.dmtavt.fragpipe.api.Bus;
import com.dmtavt.fragpipe.exceptions.ValidationException;
import com.dmtavt.fragpipe.messages.MessageClearCache;
import com.dmtavt.fragpipe.messages.MessageMsfraggerNewJarPath;
import com.dmtavt.fragpipe.messages.MessageShowAboutDialog;
import com.dmtavt.fragpipe.messages.MessageUmpireEnabled;
import com.dmtavt.fragpipe.messages.NoteConfigMsfragger;
import com.dmtavt.fragpipe.tools.msfragger.Msfragger;
import com.github.chhh.utils.JarUtils;
import com.github.chhh.utils.SwingUtils.FcMode;
import com.github.chhh.utils.swing.UiCheck;
import com.github.chhh.utils.swing.UiText;
import com.github.chhh.utils.swing.UiUtils;
import java.awt.Component;
import java.awt.Font;
import java.awt.event.ActionEvent;
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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import umich.msfragger.params.ThisAppProps;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.swing.JPanelWithEnablement;
import umich.msfragger.params.fragger.MsfraggerProps;

public class TabConfig extends JPanelWithEnablement {
  private static final Logger log = LoggerFactory.getLogger(TabConfig.class);

  private UiText uiTextBinFragger;
  private JEditorPane epFraggerVer;

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

    {
      JLabel c = new JLabel();
      c.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
      c.setAlignmentX(Component.CENTER_ALIGNMENT);
      c.setText(SwingUtils.makeHtml(
          "Tabs on top represent processing steps and will be performed sequentially.\n"
              + "Tabs will become enabled once the tools on this panel are configured."));
      add(c, new CC().growX().wrap());
    }

    {
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
      add(p, new CC().growX().wrap());
    }
  }

  private JPanel createPanelTopButtons() {
    JPanel p = newMigPanel();
    Supplier<CC> ccL = () -> new CC().alignX("left");
    Supplier<CC> ccR = () -> new CC().alignX("right");
    p.add(UiUtils.createButton("About", e -> Bus.post(new MessageShowAboutDialog())), ccL.get().split().spanX());
    p.add(UiUtils.createButton("Clear Cache", e -> Bus.post(new MessageClearCache())), ccL.get());
    UiCheck uiCheckUmpire = UiUtils.createUiCheck("Enable DIA-Umpire", false,
        e -> Bus.post(new MessageUmpireEnabled(((JCheckBox) e.getSource()).isSelected())));
    p.add(uiCheckUmpire, ccL.get().wrap());
    //p.add(UiUtils.createButton("Find tools", e -> post(new MessageFindTools())), ccL.get().split().spanX());
    JLabel label = new JLabel("Main tools configuration");
    label.setFont(new Font(label.getFont().getName(), Font.BOLD, label.getFont().getSize()+3));
    p.add(label, ccL.get().wrap());
    return p;
  }

  private JPanel createPanelFragger() {
    JPanel p = newMigPanel();
    p.setBorder(new TitledBorder("MSFragger"));
    uiTextBinFragger = UiUtils.uiTextBuilder().ghost("Select path to MSFragger.jar").create();
    p.add(Fragpipe.rename(uiTextBinFragger, "config.bin-msfragger"), ccL().split().growX());
    p.add(UiUtils.createButton("Browse", this::actionBrowseBinMsfragger), ccL());
    JButton btnUpdate = UiUtils.createButton("Update", this::actionUpdateBinMsfragger);
    btnUpdate.setToolTipText(SwingUtils.makeHtml("Open MSFragger upgrader tool in browser.\n" +
        "In order to update you <b>must</b> download an\n" +
        "original copy from the <b>download</b> website once."));
    p.add(btnUpdate, ccL().wrap());
    epFraggerVer = SwingUtils.createClickableHtml("MSFragger version: N/A");
    Fragpipe.renameNoCache(epFraggerVer, name("msfragger.version-info"));
    p.add(epFraggerVer, ccL().spanX().growX().wrap());
    p.add(SwingUtils.createClickableHtml(createFraggerCitationBody()), ccL().spanX().growX().wrap());
    return p;
  }

  private String name(String name) {
    return name.startsWith("config.") ? name : "config." + name;
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

  private void actionUpdateBinMsfragger(ActionEvent evt) {
    throw new UnsupportedOperationException("Updating MSFragger has not been reimplemented yet");
  }

  private void actionBrowseBinMsfragger(ActionEvent evt) {
    JFileChooser fc = SwingUtils.newFileChooser("Select MSFragger jar", "Select",
        false, FcMode.FILES_ONLY, true,
        new FileNameExtensionFilter("JAR files", "jar"));
    SwingUtils.setFileChooserPath(fc, Stream.of(
        uiTextBinFragger.getNonGhostText(),
        ThisAppProps.load(ThisAppProps.PROP_BINARIES_IN),
        JarUtils.getCurrentJarPath()));

    int result = fc.showOpenDialog(SwingUtils.findParentFrameForDialog(this));
    if (JFileChooser.APPROVE_OPTION == result) {
      Bus.post(new MessageMsfraggerNewJarPath(fc.getSelectedFile().toString()));
    }
  }

  @Subscribe
  public void onMsfraggerNewJarPath(MessageMsfraggerNewJarPath m) {
    String version;
    try {
      Msfragger.validateJar(m.binPath);
      version = Msfragger.version(Paths.get(m.binPath));
    } catch (ValidationException e) {
      SwingUtils.showErrorDialog(this, SwingUtils.makeHtml(e.getMessage()), "Invalid MSFragger jar");
      return;
    }
    Bus.postSticky(new NoteConfigMsfragger(m.binPath, version));
  }

  @Subscribe
  public void onNoteConfigMsfragger(NoteConfigMsfragger m) {
    uiTextBinFragger.setText(m.jarPath);
    epFraggerVer.setText("MSFragger version: " + m.version);
  }

  public static String createFraggerCitationHtml(Font font) {
    StringBuilder sb = new StringBuilder();
    sb.append("<html>");

    sb.append("<head>");
    sb.append("</head>");

    sb.append("<body style=\"").append(SwingUtils.createHtmlBodyStyle(font)).append("\"");

    sb.append(createFraggerCitationBody());

    sb.append("</body>");
    sb.append("</html>");

    return sb.toString();
  }

  public static String createFraggerCitationBody() {
    final Properties p = ThisAppProps.getRemotePropertiesWithLocalDefaults();
    final String linkMsfragger = p.getProperty(MsfraggerProps.PROP_FRAGGER_SITE_URL, "https://nesvilab.github.io/MSFragger/");
    final String linkFragpipe = p.getProperty(ThisAppProps.PROP_FRAGPIPE_SITE_URL, "https://github.com/Nesvilab/FragPipe");
    final String doi= p.getProperty(ThisAppProps.PROP_MANUSCRIPT_DOI, "10.1038/nmeth.4256");
    final String linkManuscript= p.getProperty(ThisAppProps.PROP_MANUSCRIPT_URL, "http://www.nature.com/nmeth/journal/v14/n5/full/nmeth.4256.html");
    final StringBuilder sb = new StringBuilder();

    sb.append("<p style=\"margin-top: 0\">");
    sb.append("<b>Please cite: </b>");
    sb.append(
        "<a href=\"").append(linkManuscript).append("\">MSFragger: ultrafast and comprehensive peptide identification in mass spectrometry–based proteomics</a>");
    sb.append("<br/>");
    sb.append("<b>DOI: </b>").append(doi);
    sb.append("</p>");

    sb.append("<p style=\"margin-top: 10\">");
    sb.append("More info and docs: <a href=\"").append(linkMsfragger).append("\">MSFragger website</a>")
        .append(", <a href=\"").append(linkFragpipe).append("\">FragPipe GitHub page</a>");
    return sb.toString();
  }
}
