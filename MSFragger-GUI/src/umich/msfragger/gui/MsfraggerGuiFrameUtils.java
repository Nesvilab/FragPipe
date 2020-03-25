package umich.msfragger.gui;

import static umich.msfragger.params.fragger.FraggerMigPanel.PROP_FILECHOOSER_LAST_PATH;

import com.dmtavt.fragpipe.tools.msfragger.Msfragger;
import com.dmtavt.fragpipe.tools.msfragger.Msfragger.FraggerRunResult;
import com.github.chhh.utils.StringUtils;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Desktop;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.StandardOpenOption;
import java.nio.file.attribute.BasicFileAttributes;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.Properties;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.logging.Level;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JEditorPane;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTable;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;
import javax.swing.filechooser.FileNameExtensionFilter;
import javax.swing.table.DefaultTableModel;
import javax.swing.text.JTextComponent;
import net.java.balloontip.BalloonTip;
import net.java.balloontip.styles.RoundedBalloonStyle;
import org.apache.commons.lang3.JavaVersion;
import org.apache.commons.lang3.SystemUtils;
import org.greenrobot.eventbus.EventBus;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import umich.msfragger.Version;
import umich.msfragger.cmd.CmdMsfragger;
import umich.msfragger.cmd.ProcessBuilderInfo;
import umich.msfragger.gui.api.SearchTypeProp;
import umich.msfragger.gui.api.TableModelColumn;
import umich.msfragger.gui.api.UniqueLcmsFilesTableModel;
import umich.msfragger.gui.api.VersionFetcher;
import com.dmtavt.fragpipe.messages.MessageSearchType;
import com.dmtavt.fragpipe.messages.MessageShowAboutDialog;
import com.dmtavt.fragpipe.messages.MessageValidityFragger;
import com.dmtavt.fragpipe.messages.MessageValidityMassCalibration;
import com.dmtavt.fragpipe.messages.MessageValidityMsadjuster;
import umich.msfragger.params.ThisAppProps;
import umich.msfragger.params.dbslice.DbSlice;
import umich.msfragger.params.dbslice.DbSlice.MessageInitDone;
import umich.msfragger.params.fragger.MsfraggerProps;
import com.dmtavt.fragpipe.tools.msfragger.MsfraggerVersionComparator;
import umich.msfragger.params.fragger.MsfraggerVersionFetcherGithub;
import umich.msfragger.params.fragger.MsfraggerVersionFetcherLocal;
import umich.msfragger.params.fragger.MsfraggerVersionFetcherServer;
import umich.msfragger.params.philosopher.PhilosopherProps;
import umich.msfragger.params.speclib.SpecLibGen;
import com.github.chhh.utils.FileListing;
import com.github.chhh.utils.IValidateString;
import com.github.chhh.utils.LogUtils;
import com.github.chhh.utils.OsUtils;
import com.github.chhh.utils.PathUtils;
import com.github.chhh.utils.PythonInfo;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.ValidateTrue;
import com.github.chhh.utils.VersionComparator;
import com.github.chhh.utils.swing.ISimpleTextComponent;
import com.github.chhh.utils.swing.TextConsole;

public class MsfraggerGuiFrameUtils {
  private static final Logger log = LoggerFactory.getLogger(MsfraggerGuiFrameUtils.class);
  private MsfraggerGuiFrameUtils() {}


  public static Path userShowLoadFileDialog(String title, FileNameExtensionFilter filter, Component owner) {
    JFileChooser fc = new JFileChooser();
    fc.setApproveButtonText("Load");
    fc.setDialogTitle(title);
    fc.setMultiSelectionEnabled(false);

    fc.setAcceptAllFileFilterUsed(true);
    if (filter != null) {
      fc.setFileFilter(filter);
    }

    final String propName = ThisAppProps.PROP_FRAGGER_PARAMS_FILE_IN;
    ThisAppProps.load(propName, fc);

    Component parent = SwingUtils.findParentFrameForDialog(owner);
    int saveResult = fc.showOpenDialog(parent);
    if (JFileChooser.APPROVE_OPTION == saveResult) {
      File selectedFile = fc.getSelectedFile();
      Path path = Paths.get(selectedFile.getAbsolutePath());
      ThisAppProps.save(propName, path.toString());
      if (Files.exists(path)) {
        return path;
      } else {
        JOptionPane.showMessageDialog(parent, "<html>This is strange,<br/> "
                + "but the file you chose to load doesn't exist anymore.", "Strange",
            JOptionPane.ERROR_MESSAGE);
      }
    }
    return null;
  }

  /**
   * @param selectedFn can be null.
   * @param owner can be null. Used for positioning the dialog on the screen.
   */
  public static Path userShowSaveFileDialog(String title, String selectedFn, Component owner) {
    JFileChooser fc = new JFileChooser();
    fc.setApproveButtonText("Save");
    fc.setDialogTitle(title);
    fc.setMultiSelectionEnabled(false);
    SwingUtils.setFileChooserPath(fc, ThisAppProps.load(PROP_FILECHOOSER_LAST_PATH));
//    SimpleDateFormat df = new SimpleDateFormat("yyyy-MM-dd_HH-mm-ss");
//    Date now = new Date();
//    fc.setSelectedFile(new File(String.format("log_%s.txt", df.format(now))));
    if (selectedFn != null) {
      fc.setSelectedFile(new File(selectedFn));
    }
    Component parent = SwingUtils.findParentFrameForDialog(owner);
    int saveResult = fc.showSaveDialog(parent);

    if (JFileChooser.APPROVE_OPTION != saveResult) {
      return null;
    }
    File selectedFile = fc.getSelectedFile();
    Path path = Paths.get(selectedFile.getAbsolutePath());
    // if exists, overwrite
    if (Files.exists(path)) {
      int overwrite = JOptionPane
          .showConfirmDialog(parent, "<html>File exists, overwrtie?<br/><br/>" + path.toString(), "Overwrite",
              JOptionPane.OK_CANCEL_OPTION);
      if (JOptionPane.OK_OPTION == overwrite) {
        try {
          Files.delete(path);
        } catch (IOException ex) {
          JOptionPane.showMessageDialog(parent, "Could not overwrite", "Overwrite",
              JOptionPane.ERROR_MESSAGE);
          return null;
        }
      }
    }
    // do something with the path
    return path;
  }

  public static void validateAndSavePython(final String binPath, boolean showPopupOnError, Component popupParent) {
    log.debug("Inside validateAndSavePython, thread not yet started");
    new Thread(() -> {
      log.debug("Inside validateAndSavePython, from started thread");
      boolean ok;
      PythonInfo pi = PythonInfo.get();
      try {
        ok = PythonInfo.get().setPythonCommand(binPath);
      } catch (Exception e) {
        ok = false;
      }
      if (ok) {
        ThisAppProps.save(ThisAppProps.PROP_BIN_PATH_PYTHON, pi.getCommand());
      } else {
        ThisAppProps.save(ThisAppProps.PROP_BIN_PATH_PYTHON, "");
      }
      if (!ok && showPopupOnError) {
        JOptionPane.showMessageDialog(popupParent,
            "Not a valid Python binary path:\n\n" + binPath, "Not a Python binary",
            JOptionPane.WARNING_MESSAGE);
      }
    }).start();
  }

  public static void validatePhilosopherVersion(MsfraggerGuiFrame msfraggerGuiFrame, String binPath,
      Pattern regexNewerVerFound,
      Pattern regexVersion, Pattern regexOldPhiVer, VersionComparator vc) {
    // get the vesrion reported by the current executable
    // if we couldn't download remote properties, try using local ones
    // if we have some philosopher properties (local or better remote)
    // then check if this version is known to be compatible

    ProcessBuilder pb = new ProcessBuilder(binPath, "version");
    pb.redirectErrorStream(true);

    boolean isNewVersionStringFound = false;
    String curVersionAndBuild = null;
    String curPhiVer = null;

    // get the vesrion reported by the current executable
    String oldUnusedDownloadLink = null;
    try {
      Process pr = pb.start();
      BufferedReader in = new BufferedReader(new InputStreamReader(pr.getInputStream()));
      String line;
      while ((line = in.readLine()) != null) {
        Matcher m = regexNewerVerFound.matcher(line);
        if (m.find()) {
          isNewVersionStringFound = true;
          oldUnusedDownloadLink = m.group(1);
        }
        Matcher mVer = regexVersion.matcher(line);
        if (mVer.find()) {
          curVersionAndBuild = mVer.group("version") + " (build " + mVer.group("build") + ")";
          curPhiVer = mVer.group("version");
          log.debug("Detected philosopher version: {}", curPhiVer);
        }
      }

      msfraggerGuiFrame.philosopherVer = StringUtils
          .isNullOrWhitespace(curVersionAndBuild) ? MsfraggerGuiFrame.UNKNOWN_VERSION
          : curVersionAndBuild;
      msfraggerGuiFrame.getLabelPhilosopherInfo().setText(String.format(
          "Philosopher version: %s. %s", msfraggerGuiFrame.philosopherVer, OsUtils.OsInfo()));

      int returnCode = pr.waitFor();
      JEditorPane ep = null;

      String vCurMajor = Version.version().split("[-_]+")[0];
      Properties props = PhilosopherProps.getProperties();
      String propKeyStubMin = PhilosopherProps.PROP_LOWEST_COMPATIBLE_VERSION + "." + vCurMajor;
      Optional<String> propKeyMin = props.stringPropertyNames().stream()
          .filter(name -> name.startsWith(propKeyStubMin)).findFirst();
      String minPhiVer = propKeyMin.map(props::getProperty).orElse(null);
      String propKeyStubMax = PhilosopherProps.PROP_LATEST_COMPATIBLE_VERSION + "." + vCurMajor;
      Optional<String> propKeyMax = props.stringPropertyNames().stream()
          .filter(name -> name.startsWith(propKeyStubMax)).findFirst();
      String maxPhiVer = propKeyMax.map(props::getProperty).orElse(null);

      String link = PhilosopherProps.getProperties().getProperty(PhilosopherProps.PROP_DOWNLOAD_URL, "https://github.com/Nesvilab/philosopher/releases");

      boolean isOldVersionScheme = curPhiVer != null && regexOldPhiVer.matcher(curPhiVer).find();
      if (isOldVersionScheme)
        log.debug("Old philosopher versioning scheme detected");

      if (returnCode != 0 || isOldVersionScheme || curPhiVer == null) {
        StringBuilder sb = new StringBuilder("This Philosopher version is no longer supported by FragPipe.<br/>\n");
        if (minPhiVer != null)
          sb.append("Minimum required version: ").append(minPhiVer).append("<br/>\n");
        if (maxPhiVer != null)
          sb.append("Latest known compatible version: ").append(maxPhiVer).append("<br/>\n");
        sb.append("Please <a href=\"").append(link).append("\">click here</a> to download a newer one.");
        ep = SwingUtils.createClickableHtml(sb.toString(), msfraggerGuiFrame.balloonBgColor);

      } else {

        if (minPhiVer != null && vc.compare(curPhiVer, minPhiVer) < 0) {
          // doesn't meet min version requirement
          StringBuilder sb = new StringBuilder("Philosopher version ")
              .append(curPhiVer).append(" is no longer supported by FragPipe.<br/>\n");
          sb.append("Minimum required version: ").append(minPhiVer).append("<br/>\n");
          if (maxPhiVer != null)
            sb.append("Latest known compatible version: ").append(maxPhiVer).append("<br/>\n");
          sb.append("Please <a href=\"").append(link).append("\">click here</a> to download a newer one.");
          ep = SwingUtils.createClickableHtml(sb.toString(), msfraggerGuiFrame.balloonBgColor);

        } else if (isNewVersionStringFound) {
          StringBuilder sb = new StringBuilder();
          sb.append("Newer version of Philosopher available.<br/>\n");
          sb.append("<a href=\"").append(link).append("\">Click here</a> to download.<br/>\n");

          if (maxPhiVer == null) {
            sb.append(
                "<br>\nHowever, we have not yet checked if it's fully compatible with this version of ")
                .append(Version.PROGRAM_TITLE).append(".");
          } else { // max ver != null
            int cmp = vc.compare(curPhiVer, maxPhiVer);
            if (cmp == 0) {
              sb.append(
                  "<br>\nHowever, <b>you currently have the latest known tested version</b>.");
            } else if (cmp < 0) {
              sb.append("<br>\nThe latest known tested version is<br>\n")
                  .append("<b>Philosopher ").append(maxPhiVer).append("</b>.<br/>\n");
              sb.append(
                  "It is not recommended to upgrade to newer versions unless they are tested.");
            } else if (cmp > 0) {
              sb.append("<br>\nYour current version is higher than the last known tested version.");
            }
          }
          ep = SwingUtils.createClickableHtml(sb.toString(), msfraggerGuiFrame.balloonBgColor);
        }
      }

      if (ep != null) {
        if (msfraggerGuiFrame.balloonPhilosopher != null) {
          msfraggerGuiFrame.balloonPhilosopher.closeBalloon();
        }
        msfraggerGuiFrame.balloonPhilosopher = new BalloonTip(msfraggerGuiFrame.getTextBinPhilosopher(), ep,
            new RoundedBalloonStyle(5, 5, msfraggerGuiFrame.balloonBgColor, Color.BLACK), true);
        msfraggerGuiFrame.balloonPhilosopher.setVisible(true);
      }

    } catch (IOException | InterruptedException e) {
      throw new IllegalStateException(
          "Error while creating a java process for Philosopher test.");
    }
  }

  public static void findToolsAction(MsfraggerGuiFrame msfraggerGuiFrame) {
    String fraggerFoundPath = null;
    String philosopherFoundPath = null;

    JFileChooser fileChooser = new JFileChooser();
    fileChooser.setApproveButtonText("Search here");
    fileChooser.setApproveButtonToolTipText("Search this directory recursively");
    fileChooser.setDialogTitle("Select path to search for binaries");
    fileChooser.setMultiSelectionEnabled(false);
    fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

    List<String> props = Arrays
        .asList(ThisAppProps.PROP_BIN_PATH_MSFRAGGER, ThisAppProps.PROP_BINARIES_IN,
            ThisAppProps.PROP_BIN_PATH_PHILOSOPHER);
    String fcPath = ThisAppProps.tryFindPath(props, true);
    SwingUtils.setFileChooserPath(fileChooser, fcPath);

    int showOpenDialog = fileChooser.showOpenDialog(SwingUtils.findParentFrameForDialog(
        msfraggerGuiFrame));
    switch (showOpenDialog) {
      case JFileChooser.APPROVE_OPTION:
        File f = fileChooser.getSelectedFile();

        // Fragger first
        Pattern regexFragger = Pattern
            .compile(".*?MSFragger[^\\/]+?\\.jar", Pattern.CASE_INSENSITIVE);
        FileListing listing = new FileListing(Paths.get(f.getAbsolutePath()), regexFragger);
        List<Path> foundFiles = listing.findFiles();
        for (Path foundFile : foundFiles) {
          if (validateAndSaveMsfraggerPath(msfraggerGuiFrame, foundFile.toString())) {
            fraggerFoundPath = foundFile.toString();
            ThisAppProps.save(ThisAppProps.PROP_BINARIES_IN, fraggerFoundPath);
            JOptionPane.showMessageDialog(msfraggerGuiFrame, "Found MSFragger jar.\n"
                + fraggerFoundPath, "Info", JOptionPane.INFORMATION_MESSAGE);
            break;
          }
        }
        if (fraggerFoundPath == null) {
          JOptionPane.showMessageDialog(msfraggerGuiFrame, "Could not locate MSFragger jar.", "Info",
              JOptionPane.INFORMATION_MESSAGE);
        }

        // now philosopher
        Pattern regexPhilosopher = Pattern
            .compile(".*?philosopher[^\\/]*", Pattern.CASE_INSENSITIVE);
        foundFiles = new FileListing(Paths.get(f.getAbsolutePath()), regexPhilosopher).findFiles();
        for (Path foundFile : foundFiles) {
          if (validateAndSavePhilosopherPath(msfraggerGuiFrame, foundFile.toString())) {
            philosopherFoundPath = foundFile.toString();
            ThisAppProps.save(ThisAppProps.PROP_BINARIES_IN, philosopherFoundPath);
            JOptionPane.showMessageDialog(msfraggerGuiFrame, "Found Philosopher.\n"
                + philosopherFoundPath, "Info", JOptionPane.INFORMATION_MESSAGE);
            break;
          }
        }
        if (philosopherFoundPath == null) {
          JOptionPane.showMessageDialog(msfraggerGuiFrame, "Could not locate Philosopher.", "Info",
              JOptionPane.INFORMATION_MESSAGE);
        }

        break;
    }
  }

  public static void userBrowsePhilosopherBin(MsfraggerGuiFrame msfraggerGuiFrame) {
    JFileChooser fc = new JFileChooser();
    fc.setApproveButtonText("Select");
    fc.setDialogTitle("Select Philosopher binary");
    fc.setMultiSelectionEnabled(false);
//    if (OsUtils.isWindows()) {
//      FileNameExtensionFilter fileNameExtensionFilter = new FileNameExtensionFilter("Executables",
//          "exe");
//      fc.setFileFilter(fileNameExtensionFilter);
//    }

    fc.setFileSelectionMode(JFileChooser.FILES_ONLY);

    // ==============================================================
    Path current = tryFindStartingPath(msfraggerGuiFrame.getTextBinPhilosopher().getText());
    if (current != null) {
      SwingUtils.setFileChooserPath(fc, current);
    } else {
      List<String> props = Arrays.asList(ThisAppProps.PROP_BIN_PATH_PHILOSOPHER, ThisAppProps.PROP_BINARIES_IN);
      String fcPath = ThisAppProps.tryFindPath(props, false);
      SwingUtils.setFileChooserPath(fc, fcPath);
    }
    // ==============================================================

    if (JFileChooser.APPROVE_OPTION == fc
        .showOpenDialog(SwingUtils.findParentFrameForDialog(msfraggerGuiFrame))) {
      String path = fc.getSelectedFile().getAbsolutePath();
      if (validateAndSavePhilosopherPath(msfraggerGuiFrame, path)) {
        // already saved to PROP_PHILOSOPHER, now save to general PROP_BINARIES
        ThisAppProps.save(ThisAppProps.PROP_BINARIES_IN, path);
      }
    }
  }

  public static boolean validateAndSave(final HashMap<String, BalloonTip> tipMap,
      final JTextComponent comp, final String propName,
      final String newText, final IValidateString valid) {

    final String updText = newText != null ? newText : comp.getText().trim();
    final boolean isValid = valid.test(updText);
    comp.setText(updText);
    if (isValid) {
      ThisAppProps.save(propName, updText);
    }

    SwingUtilities.invokeLater(new Runnable() {
      @Override
      public void run() {
        BalloonTip tip = tipMap.get(propName);
        if (tip != null) {
          tip.closeBalloon();
        }

        if (!isValid) {
          tip = new BalloonTip(comp, "Invalid format.");
          tip.setVisible(true);
          tipMap.put(propName, tip);
        }
      }
    });

    return isValid;
  }

  /**
   * Collects all tabs' components that have names with values from the map.
   * @param tabPane
   */
  public static Map<String, String> formToMap(JTabbedPane tabPane) {
    // getting tab names
    Map<Integer, String> mapTabNameToIdx = new HashMap<>();
    for (int i = 0, tabCount = tabPane.getTabCount(); i < tabCount; i++) {
      mapTabNameToIdx.put(i, tabPane.getTitleAt(i));
    }

    final Function<Component, Map<String, String>> compToMap = awtComponent -> {
      if (!(awtComponent instanceof Container)) {
        return Collections.emptyMap();
      }
      Container awtContainer = (Container)awtComponent;
//      final Pattern re = Pattern.compile("ui\\.name\\..*");
//      Predicate<String> filter = re.asPredicate();
      Predicate<String> filter = s -> true;
      Map<String, String> map = SwingUtils.valuesToMap(awtContainer, filter);
      return map;
    };

    Map<String, String> whole = new HashMap<>();
    for (int i = 0; i < tabPane.getTabCount(); i++) {
      Component compAt = tabPane.getComponentAt(i);
      final String tabname = mapTabNameToIdx.getOrDefault(i, "?");
      Map<String, String> map = compToMap.apply(compAt);
      final String badName = "Spinner.formattedTextField";
      if (map.containsKey(badName)) {
        map.remove(badName);
      }
      if (map.isEmpty()) {
        log.debug("No mapping for Tab #{} [{}]", i, tabname);
      } else {

        log.debug("Got mapping for Tab #{} [{}]: {}", i, tabname, map);
        for (Entry<String, String> e : map.entrySet()) {
          whole.merge(e.getKey(), e.getValue(), (s1, s2) -> {
            String msg = String.format("Duplicate ui-element key '%s' in tab [%s]", e.getKey(), tabname);
            throw new IllegalStateException(msg);
          });
        }
      }
    }
    return whole;
  }

  public static void onShowAbout(MsfraggerGuiFrame guiFrame, MessageShowAboutDialog m) {
    // for copying style
    JLabel label = new JLabel();
    Font font = label.getFont();

    // create some css from the label's font
    StringBuilder style = new StringBuilder("font-family:" + font.getFamily() + ";");
    style.append("font-weight:").append(font.isBold() ? "bold" : "normal").append(";");
    style.append("font-size:").append(font.getSize()).append("pt;");


    final Properties p = ThisAppProps.getRemotePropertiesWithLocalDefaults();
    String linkDl = p.getProperty(Version.PROP_DOWNLOAD_URL, "");
    String linkSite = p.getProperty(ThisAppProps.PROP_LAB_SITE_URL, "http://nesvilab.org");
    String linkToPaper = p.getProperty(ThisAppProps.PROP_MANUSCRIPT_URL, "http://www.nature.com/nmeth/journal/v14/n5/full/nmeth.4256.html");

    JEditorPane ep = new JEditorPane("text/html", "<html><body style=\"" + style + "\">"
        + "MSFragger - Ultrafast Proteomics Search Engine<br/>"
        + "FragPipe (v" + Version.version() + ")<br/>"
        + "Dmitry Avtonomov<br/>"
        + "University of Michigan, 2017<br/><br/>"
        + "<a href=\"" + linkDl
        + "\">Click here to download</a> the latest version<br/><br/>"
        + "<a href=\"" + linkSite + "\">Alexey Nesvizhskii lab</a><br/>&nbsp;<br/>&nbsp;"
        + "MSFragger authors and contributors:<br/>"
        + "<ul>"
        + "<li>Andy Kong</li>"
        + "<li>Dmitry Avtonomov</li>"
        + "<li>Guo-Ci Teo</li>"
        + "<li>Fengchao Yu</li>"
        + "<li>Alexey Nesvizhskii</li>"
        + "</ul>"
        + "<a href=\"" + linkToPaper + "\">Link to the research manuscript</a><br/>"
        + "Reference: <b>doi:10.1038/nmeth.4256</b><br/><br/>"
        + "Components and Downstream tools:"
        + "<ul>"
        + "<li><a href='https://philosopher.nesvilab.org/'>Philosopher</a>: Felipe Leprevost</li>"
        + "<li>PTM-Shepherd: Andy Kong</li>"
        + "<li>Crystal-C: Hui-Yin Chang</li>"
        + "<li>Spectral library generation: Guo-Ci Teo</li>"
        + "<li><a href='https://diaumpire.nesvilab.org/'>DIA-Umpire</a>: Chih-Chiang Tsou</li>"
        + "</ul>"
        + "</body></html>");

    // handle link messages
    ep.addHyperlinkListener(new HyperlinkListener() {
      @Override
      public void hyperlinkUpdate(HyperlinkEvent e) {
        if (e.getEventType().equals(HyperlinkEvent.EventType.ACTIVATED)) {
          try {
            Desktop.getDesktop().browse(e.getURL().toURI());
          } catch (URISyntaxException | IOException ex) {
            java.util.logging.Logger
                .getLogger(MsfraggerGuiFrame.class.getName()).log(Level.SEVERE, null, ex);
          }
        }
      }
    });
    ep.setEditable(false);
    ep.setBackground(label.getBackground());

    // show
    JOptionPane.showMessageDialog(guiFrame, ep, "About", JOptionPane.INFORMATION_MESSAGE);
  }

  public static void checkPreviouslySavedParams(MsfraggerGuiFrame guiFrame) {
    log.debug("entered checkPreviouslySavedParams");
    ThisAppProps cached = ThisAppProps.loadFromTemp();
    if (cached != null) {
      // if there was a cached version of properties
      VersionComparator vc = new VersionComparator();
      String storedVer = cached.getProperty(Version.PROP_VER, "0.0");
      if (vc.compare(storedVer, "4.0") < 0) {
        // and the version was less than 4.0
        String msg = String.format(Locale.ROOT, "Looks like you've upgraded from an "
            + "older version to 4.0+,\n"
            + "it is HIGHLY recommended to reset the default parameters.\n\n"
            + "Reset the parameters now? \n\n"
            + "This message won't be displayed again.");
        String[] options = {"Cancel", "Load defaults for Closed", "Load defaults of Open"};
        int result = JOptionPane.showOptionDialog(guiFrame, msg, "Reset to defautls",
            JOptionPane.DEFAULT_OPTION, JOptionPane.QUESTION_MESSAGE, null, options, options[0]);
        switch (result) {
          case 1:
            EventBus.getDefault().post(new MessageSearchType(SearchTypeProp.closed));
            break;
          case 2:
            EventBus.getDefault().post(new MessageSearchType(SearchTypeProp.open));
            break;
        }

        // rewrite the cached params file with a versioned one
        ThisAppProps.save(Version.PROP_VER, Version.version());
      } else if (vc.compare(storedVer, "4.0") >= 0 && vc.compare(storedVer, "5.1") <= 0) {
        // and the version between 4.0 and 5.1
        final String prop = ThisAppProps.PROP_TEXT_CMD_PEPTIDE_PROPHET;
        String oldPepProphStr = ThisAppProps.load(prop);
        Pattern re = Pattern.compile("--clevel\\s+2");
        if (oldPepProphStr != null) {
          Matcher m = re.matcher(oldPepProphStr);
          if (m.find()) {
            String replaced = oldPepProphStr.replaceAll(re.pattern(), "--clevel -2");
            ThisAppProps.save(prop, replaced);
            ThisAppProps.load(prop, guiFrame.getTextPepProphCmd());

            String msg = String.format(Locale.ROOT,
                "<html>We've noticed a cached leftover buggy option for PeptideProphet "
                    + "'--clevel 2' and automatically replaced \n"
                    + "it with '--clevel -2'.\n\n"
                    + "If you know what you're doing and intended it to be '--clevel 2' "
                    + "please change it back on PeptideProphet tab.\n\n"
                    + "You also have the option to reload defaults. "
                    + "This message won't be displayed again.");
            String[] options = {"Ok", "Load defaults for Closed Search",
                "Load defaults of Open Search"};
            int result = JOptionPane
                .showOptionDialog(guiFrame, msg, "Cached option automatically replaced",
                    JOptionPane.DEFAULT_OPTION, JOptionPane.QUESTION_MESSAGE, null, options,
                    options[0]);
            switch (result) {
              case 1:
                EventBus.getDefault().post(new MessageSearchType(SearchTypeProp.closed));
                break;
              case 2:
                EventBus.getDefault().post(new MessageSearchType(SearchTypeProp.open));
                break;
            }
          }
        }

        // rewrite the cached params file with a versioned one
        ThisAppProps.save(Version.PROP_VER, Version.version());
      }
    }
  }

  public static void actionSelectWorkingDir(MsfraggerGuiFrame guiFrame) {
    JFileChooser fc = new JFileChooser();
    //FileNameExtensionFilter fileNameExtensionFilter = new FileNameExtensionFilter("FASTA files", "fa", "fasta");
    //fileChooser.setFileFilter(fileNameExtensionFilter);
    fc.setApproveButtonText("Select directory");
    fc.setApproveButtonToolTipText("Select");
    fc.setDialogTitle("Choose working directory");
    fc.setMultiSelectionEnabled(false);
    fc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

    // use either current text in the field or saved cache
    log.debug("Preparing work dir file chooser, ThisAppProps.PROP_FILE_OUT is: {}", ThisAppProps.load(ThisAppProps.PROP_FILE_OUT));
    final String text = guiFrame.getTxtWorkingDir().getText().trim();
    if (!StringUtils.isNullOrWhitespace(text)) {
      try {
        Path p = Paths.get(guiFrame.getTxtWorkingDir().getText());
        if (Files.exists(p)) {
          fc.setSelectedFile(p.toFile());
        }
      } catch (Exception ignored) {}
    } else {
      ThisAppProps.load(ThisAppProps.PROP_FILE_OUT, fc);
    }

    int showOpenDialog = fc.showOpenDialog(guiFrame);
    switch (showOpenDialog) {
      case JFileChooser.APPROVE_OPTION:
        File f = fc.getSelectedFile();
        guiFrame.getTxtWorkingDir().setText(f.getAbsolutePath());
        ThisAppProps.save(ThisAppProps.PROP_FILE_OUT, f.getAbsolutePath());
        break;
    }
  }

  public static void exportLogToFile(MsfraggerGuiFrame guiFrame) {
    if (guiFrame.console == null) {
      return;
    }

    JFileChooser fc = new JFileChooser();
    fc.setApproveButtonText("Save");
    fc.setDialogTitle("Export to");
    fc.setMultiSelectionEnabled(false);
    SwingUtils.setFileChooserPath(fc, ThisAppProps.load(PROP_FILECHOOSER_LAST_PATH));
    SimpleDateFormat df = new SimpleDateFormat("yyyy-MM-dd_HH-mm-ss");
    Date now = new Date();
    fc.setSelectedFile(new File(String.format("log_%s.txt", df.format(now))));
    Component parent = SwingUtils.findParentFrameForDialog(guiFrame);
    int saveResult = fc.showSaveDialog(parent);
    if (JFileChooser.APPROVE_OPTION == saveResult) {
      File selectedFile = fc.getSelectedFile();
      Path path = Paths.get(selectedFile.getAbsolutePath());
      // if exists, overwrite
      if (Files.exists(path)) {
        int overwrite = JOptionPane
            .showConfirmDialog(parent, "<html>File exists, overwrtie?<br/><br/>" + path.toString(), "Overwrite",
                JOptionPane.OK_CANCEL_OPTION);
        if (JOptionPane.OK_OPTION == overwrite) {
          try {
            Files.delete(path);
          } catch (IOException ex) {
            JOptionPane.showMessageDialog(parent, "Could not overwrite", "Overwrite",
                JOptionPane.ERROR_MESSAGE);
            return;
          }
        }
      }
      saveLogToFile(guiFrame.console, path);
    }

  }

  public static void saveLogToFile(TextConsole console, Path path) {
    final String text = console.getText().replaceAll("[^\n]+\u200B" + System.getProperty("line.separator"), "");
    byte[] bytes = text.getBytes(StandardCharsets.UTF_8);
    try {
      Files.write(path, bytes, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING);
    } catch (IOException e) {
      log.error("Error writing log to file", e);
    }
  }

  public static void checkPython(MsfraggerGuiFrame guiFrame) {
    String path = ThisAppProps.load(ThisAppProps.PROP_BIN_PATH_PYTHON);
    PythonInfo pi = PythonInfo.get();
    if (path != null) {
      try {
        if (!pi.setPythonCommand(path)) {
          throw new Exception("Could not set python command to the old value");
        }
      } catch (Exception e) {
        ThisAppProps.save(ThisAppProps.PROP_BIN_PATH_PYTHON, "");
        int yesNo = JOptionPane.showConfirmDialog(guiFrame,
            "Previously stored Python location is now invalid:\n"
                + "\t" + path + "\n\nDo you want to try automatically find the Python binary?",
            "Previously used Python not available", JOptionPane.YES_NO_OPTION,
            JOptionPane.WARNING_MESSAGE);
        if (JOptionPane.YES_OPTION == yesNo) {
          try {
            pi.findPythonCommand();
            if (!pi.isInitialized()) {
              throw new Exception("Python command not found");
            }
          } catch (Exception e1) {
            JOptionPane.showMessageDialog(guiFrame,
                "Python not found.\n\n"
                    + "You can manually select Python binary\n"
                    + "if you know where it is located.",
                "Error", JOptionPane.WARNING_MESSAGE);
          }
        }
      }
      return;
    }
    // No python location was stored. It's only stored when a user manually changes the location.
    // try to auto-detect Python binary
    try {
      PythonInfo.get().findPythonCommand();
    } catch (Exception ignored) {
    }
  }

  public static void actionDbspliceInitDone(JEditorPane epDbsliceInfo, MessageInitDone m) {
    final Map<MessageInitDone.REASON, String> map = new HashMap<>();
    map.put(MessageInitDone.REASON.PY_VER, "Python 3 is required.");
    map.put(MessageInitDone.REASON.WRONG_FRAGGER, "Latest version of MSFragger is required.");
    map.put(MessageInitDone.REASON.PY_MODULES, "Python modules required.");
    map.put(MessageInitDone.REASON.NOT_UNPACKED, "Error unpacking.");
    StringBuilder sb = new StringBuilder();
    sb.append(m.isSuccess ? "Database Splitting enabled." : "Database Splitting disabled.");
    if (!m.isSuccess) {
      String reasons = m.reasons.stream().flatMap(reason ->
          map.containsKey(reason) ? Stream.of(map.get(reason)) : Stream.empty())
          .collect(Collectors.joining(" <br/>"));
      if (reasons.length() > 0) {
        sb.append(" <br/>").append(reasons);
      }
      sb.append(" <br/>").append("FragPipe will work fine without this functionality.");
    }
    FragpipeUiHelpers.messageToTextComponent(ISimpleTextComponent.from(epDbsliceInfo),
        new DbSlice.Message2(true, false, sb.toString()));

    if (!m.isSuccess) {
      // attach link with instructions
      Properties p = ThisAppProps.getRemotePropertiesWithLocalDefaults();
//      Properties p = ThisAppProps.getLocalProperties(); // for testing
      String linkUrl = p.getProperty(MsfraggerProps.PROP_DBSPLIT_INSTRUCTIONS_URL,
          "https://msfragger.nesvilab.org/tutorial_setup_fragpipe.html");
      String instructions = String.format(
          "<br/>See <a href='%s'>configuration help</a> online for instructions how to enable.",
          linkUrl);
      FragpipeUiHelpers.messageToTextComponent(ISimpleTextComponent.from(epDbsliceInfo),
          new DbSlice.Message2(true, false, instructions));
    }
  }

  public static void initEditorPaneSeqDb(JEditorPane editorSequenceDb) {
    // for copying style
    JLabel label = new JLabel();
    Font font = label.getFont();

    // create some css from the label's font
    StringBuilder style = new StringBuilder("font-family:" + font.getFamily() + ";");
    style.append("font-weight:").append(font.isBold() ? "bold" : "normal").append(";");
    style.append("font-size:").append(font.getSize()).append("pt;");

    JEditorPane ep = editorSequenceDb;
    ep.setContentType("text/html");
    final String codeTag = "<code style=\" font-size:" + font.getSize() + "; \">";
    final String bin = OsUtils.isWindows() ? "philosopher_windows_amd64.exe" : "philosopher";
    ep.setText("<html><body style=\"" + style + "\">"
        + "<b>To create protein sequence database for FragPipe analysis either:</b><br/><br/>"
        + "1) Simply click 'Download' button next to the text field above.<br/><br/>"
        + "or<br/><br/>"
        + "2) Run Philosopher from the command line to download protein sequences from UniProt.<br/>"
        + "Execute the following two commands (see <a href=\"https://github.com/Nesvilab/philosopher/wiki/Database\">here</a> for detailed instructions): <br/>"
        + "<br/>"
        + codeTag
        + "&nbsp;&nbsp;&nbsp;&nbsp;" + bin + " workspace --init <br/>"
        + "&nbsp;&nbsp;&nbsp;&nbsp;" + bin + " database --reviewed --contam --id UP000005640<br/>"
        + "</code>"
        + "<br/>"
        + "This will generate a human UniProt (reviewed sequences only) database, with common contaminants and decoys (with a prefix rev_) added.<br/>"
        + "<br/>"
        + "For full UniProt, remove " + codeTag + "--reviewed</code> tag.<br/>"
        + "To include isoforms, add " + codeTag + "--isoform</code> tag.<br/>"
        + "<br/>"
        + "For mouse use UP000000589, to find the proteome ID for other organisms visit <a href=\"http://www.uniprot.org/proteomes/\">UniProt website</a>.<br/>"
        + "<br/>"
        + "<br/>"
        + "<b>If you have your own custom database:</b><br/><br/>"
        + "The headers in the custom sequence database should follow a certain format. <br/>"
        + "<br/>"
        + "For detailed information on creating and formatting databases for FragPipe analysis, please see <a href=\"https://github.com/Nesvilab/philosopher/wiki/How-to-Prepare-a-Protein-Database\">https://github.com/Nesvilab/philosopher/wiki/How-to-Prepare-a-Protein-Database</a>.<br/>"
        + "<br/>"
        + "<br/>"
        + "</body></html>");

    // handle link messages
    ep.addHyperlinkListener(e -> {
      if (e.getEventType().equals(HyperlinkEvent.EventType.ACTIVATED)) {
        try {
          Desktop.getDesktop().browse(e.getURL().toURI());
        } catch (URISyntaxException | IOException ex) {
          java.util.logging.Logger
              .getLogger(MsfraggerGuiFrame.class.getName()).log(Level.SEVERE, null, ex);
        }
      }
    });
    ep.setEditable(false);
    ep.setBackground(label.getBackground());
  }

   public static boolean validateAndSavePhilosopherPath(final MsfraggerGuiFrame guiFrame, final String path) {

    Path p = null;
    try {
      p = Paths.get(path);
    } catch (Exception e) {
      // path not parseable
    }

    if (p == null || !Files.exists(p) || Files.isDirectory(p)) {
      // invalid input
      SwingUtilities.invokeLater(new Runnable() {
        @Override
        public void run() {
          if (guiFrame.balloonPhilosopher != null) {
            guiFrame.balloonPhilosopher.closeBalloon();
            guiFrame.balloonPhilosopher = null;
          }

          String linkHardcoded = "https://github.com/Nesvilab/philosopher/releases/latest";
          String link = linkHardcoded;
          try {
            link = PhilosopherProps.getProperties().getProperty(PhilosopherProps.PROP_DOWNLOAD_URL, linkHardcoded);
            log.debug("philosopher link acquired: {}", link);
          } catch (Exception ignored) {}

          boolean areEqual = linkHardcoded.equals(link);

          String msg = "Could not find Philosopher binary file at this location.<br/>\n"
              + "Corresponding panel won't be active.<br/><br/>"
              + "<b>If that's the first time you're using " + Version.PROGRAM_TITLE + "</b>,<br/>"
              + "you will need to <a href=\"" + link + "\">download Philosopher (click here)</a> first.<br/>"
              + "Use the button on the right to proceed to the download website.";
          JEditorPane ep = SwingUtils.createClickableHtml(msg, guiFrame.balloonBgColor);

          guiFrame.balloonPhilosopher = new BalloonTip(guiFrame.getTextBinPhilosopher(), ep,
              new RoundedBalloonStyle(5, 5, guiFrame.balloonBgColor, Color.BLACK), true);

          guiFrame.balloonPhilosopher.setVisible(true);
          guiFrame.enablePhilosopherPanels(false);
        }
      });
      return false;
    }

    final boolean isPathAbsolute = p.isAbsolute();
    if (p.isAbsolute()) {
      p = p.normalize().toAbsolutePath();
    }
    final boolean isPathExists = Files.exists(p);
    final boolean isPathRunnable = Files.isExecutable(p);

    final String validatedPath = validatePhilosopherPath(path);
    final boolean isPathValid = validatedPath != null;

    if (isPathValid) {
      guiFrame.getTextBinPhilosopher().setText(validatedPath);
      ThisAppProps.save(ThisAppProps.PROP_BIN_PATH_PHILOSOPHER, validatedPath);
    }

    Thread t = new Thread(() -> {
      if (guiFrame.balloonPhilosopher != null) {
        guiFrame.balloonPhilosopher.closeBalloon();
        guiFrame.balloonPhilosopher = null;
      }

      final StringBuilder sb = new StringBuilder();
      boolean needsDisplay = false;
      if (isPathAbsolute) {
        sb.append("<html>Absolute path for Philosopher binary provided: <br/>\n")
            .append(path).append("<br/>\n");
        if (!isPathExists) {
          sb.append("\nBut the file does not exist.");
          needsDisplay = true;
        } else if (!isPathRunnable) {
          sb.append("\nBut the file is not runnable.");
          needsDisplay = true;
          if (OsUtils.isWindows()) {
            sb.append("Right click the file, Properties -> Security -> Advanced<br/>\n")
                .append("And change the executable permissions for the file.<br/>\n")
                .append("All the security implications are your responsibility.");
          } else {
            sb.append("Check that the file has execute permission for the JVM.<br/>\n")
                .append("Or you can just try `chmod a+x <philosopher-binary-file>`.<br/>\n")
                .append("All the security implications are your responsibility.");
          }
        } else if (!isPathValid) {
          sb.append("\nBut the file is invalid. It can't be run by the JVM.");
          needsDisplay = true;
        }
      } else {
        // relative path given, i.e. philosopher must be on PATH
        sb.append("<html>Relative path for Philosopher binary provided: <br/>\n")
            .append(path).append("<br/>\n");
        if (!isPathValid) {
          sb.append("But it couldn't be launched properly for some reason.");
          needsDisplay = true;
        }
      }

      if (needsDisplay) {
        SwingUtilities.invokeLater(() -> {
          if (guiFrame.balloonPhilosopher != null) {
            guiFrame.balloonPhilosopher.closeBalloon();
          }
          guiFrame.balloonPhilosopher = new BalloonTip(guiFrame.getTextBinPhilosopher(), sb.toString());
          guiFrame.balloonPhilosopher.setVisible(true);
        });
      } else {
        validatePhilosopherVersion(guiFrame, validatedPath);
      }
      guiFrame.enablePhilosopherPanels(isPathValid);
    });
    t.start();

    return isPathValid;
  }

   public static void validatePhilosopherVersion(MsfraggerGuiFrame guiFrame, final String binPath) {
    if (guiFrame.balloonPhilosopher != null) {
      guiFrame.balloonPhilosopher.closeBalloon();
    }

    final Pattern regexNewerVerFound = Pattern
        .compile("new version.*available.*?:\\s*(\\S+)", Pattern.CASE_INSENSITIVE);
    final Pattern regexVersion = Pattern
        .compile("build.*?=(?<build>\\S+).*version.*?=v?\\.?(?<version>\\S+)",
            Pattern.CASE_INSENSITIVE);
    final Pattern regexOldPhiVer = Pattern.compile("\\d{6,}");
    final VersionComparator vc = new VersionComparator();

    // Check releases on github by running `philosopher version`.
    new Thread(() -> {
      validatePhilosopherVersion(guiFrame, binPath, regexNewerVerFound, regexVersion, regexOldPhiVer, vc);
    }).start();
  }

   public static boolean validateMsfraggerPath(String path) {
    File f = new File(path);
    if (!f.getName().toLowerCase().endsWith(".jar")) {
      return false;
    }
    Path p = Paths.get(path).toAbsolutePath();
    return Files.exists(p);

  }

   public static boolean validateMsfraggerVersion(MsfraggerGuiFrame guiFrame, final String jarPath) {
    // only validate Fragger version if the current Java version is 1.8 or higher
    if (!SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_8)) {
      // we can't test fragger binary verison when java version is less than 1.8
      return true;
    }

    // get the vesrion reported by the current executable
    final FraggerRunResult jarTest = Msfragger.testJar(jarPath);
    final String localVer = jarTest.isVersionPrintedAtAll ? jarTest.version : "0.0";
    guiFrame.fraggerVer = localVer;

    // update the version label
    guiFrame.fraggerVer = StringUtils.isNullOrWhitespace(localVer) ? MsfraggerGuiFrame.UNKNOWN_VERSION : localVer;
    guiFrame.getLblFraggerJavaVer().setText(String.format(
        "MSFragger version: %s. %s", guiFrame.fraggerVer, OsUtils.JavaInfo()));

    // Now check the versions on remotes.
    final MsfraggerVersionComparator vc = new MsfraggerVersionComparator();
    Thread t = new Thread(() -> {

      MsfraggerVersionFetcherServer vfServer = new MsfraggerVersionFetcherServer();
      MsfraggerVersionFetcherGithub vfGithub = new MsfraggerVersionFetcherGithub();
//      MsfraggerVersionFetcherServer vfServer = null;
//      MsfraggerVersionFetcherGithub vfGithub = null;
      MsfraggerVersionFetcherLocal vfLocal = new MsfraggerVersionFetcherLocal();
      List<VersionFetcher> verFetchers = Arrays.asList(vfServer, vfGithub, vfLocal);
      for (final VersionFetcher vf : verFetchers) {
        if (vf == null) {
          continue;
        }
        try {
          final String updateVer = vf.fetchVersion();
          if (StringUtils.isNullOrWhitespace(updateVer)) {
            continue;
          }
          // we got a non-empty version from some version fetcher
          if (vc.compare(localVer, updateVer) < 0) {
            // local versin is older, than the fetched version
            // show balloon popup, must be done on EDT
            String url = vf.getDownloadUrl();
            final String manualDownloadUrl = StringUtils.isNullOrWhitespace(url)
                ? vfLocal.getDownloadUrl() : url;
            SwingUtilities.invokeLater(() -> {
              if (guiFrame.balloonMsfragger != null) {
                guiFrame.balloonMsfragger.closeBalloon();
              }

              StringBuilder sb = new StringBuilder();
              if (jarTest.isVersionPrintedAtAll) {
                sb.append(String.format("Your version is [%s]<br>\n"
                        + "There is a newer version of MSFragger available [%s].<br>\n",
                    localVer, updateVer));
              } else {
                sb.append(
                    String.format("<b>This version is not supported anymore</b><br>\n"
                        + "Get a new version of MSFragger [%s].<br>\n", updateVer));
              }
              if (vf.canAutoUpdate()) {
                sb.append("<br>If you choose to auto-update a new version will be downloaded<br>\n"
                    + "and placed in the same folder as the old one. The old one will be kept.");
              }
              JEditorPane ep = SwingUtils.createClickableHtml(sb.toString(),
                  guiFrame.balloonBgColor);

              JPanel panel = new JPanel();
              panel.setBackground(ep.getBackground());
              panel.setLayout(new BorderLayout());

              JPanel panelButtons = new JPanel();
              panelButtons.setBackground(ep.getBackground());
              panelButtons.setLayout(new FlowLayout(FlowLayout.LEFT, 5, 5));

              if (vf.canAutoUpdate()) {
                JButton btnAutoUpdate = new JButton("Auto-update");
                btnAutoUpdate.addActionListener(e -> {
                  if (guiFrame.balloonMsfragger == null) {
                    return;
                  }
                  guiFrame.balloonMsfragger.setVisible(false);
                  guiFrame.balloonMsfragger = null;

                  final JDialog dlg = new JDialog(guiFrame, "Updating MSFragger",
                      true);
                  JProgressBar pb = new JProgressBar(0, 100);
                  pb.setIndeterminate(true);
                  Dimension d = new Dimension(300, 75);
                  pb.setMinimumSize(d);
                  pb.setSize(d);
                  dlg.add(pb, BorderLayout.CENTER);
                  dlg.setSize(d);
                  dlg.setLocationRelativeTo(guiFrame);

                  Thread updateThread = new Thread(() -> {
                    try {

                      Path updated = vf.autoUpdate(Paths.get(jarPath));
                      validateAndSaveMsfraggerPath(guiFrame, updated.toAbsolutePath().toString());

                    } catch (Exception ex) {
                      throw new IllegalStateException(
                          "Something happened during MSFragger auto-update", ex);
                    } finally {
                      dlg.setVisible(false);
                    }
                  });
                  updateThread.start();

                  // show the dialog, this blocks until dlg.setVisible(false) is called
                  // so this call is made in the finally block
                  dlg.setVisible(true);
                });
                panelButtons.add(btnAutoUpdate);
              }

              if (!StringUtils.isNullOrWhitespace(manualDownloadUrl)) {
                JButton btnManualUpdate = new JButton("Manual update");
                btnManualUpdate.addActionListener(e -> {
                  try {
                    SwingUtils.openBrowserOrThrow(new URI(manualDownloadUrl));
                  } catch (URISyntaxException ex) {
                    throw new IllegalStateException("Incorrect url/uri", ex);
                  }
                });
                panelButtons.add(btnManualUpdate);
              }

              JButton btnClose = new JButton("Close");
              btnClose.addActionListener(e -> {
                if (guiFrame.balloonMsfragger == null) {
                  return;
                }
                guiFrame.balloonMsfragger.setVisible(false);
                guiFrame.balloonMsfragger = null;
              });

              panel.add(ep, BorderLayout.CENTER);
              panelButtons.add(btnClose);
              panel.add(panelButtons, BorderLayout.SOUTH);

              guiFrame.balloonMsfragger = new BalloonTip(guiFrame.getTextBinMsfragger(), panel,
                  new RoundedBalloonStyle(5, 5, guiFrame.balloonBgColor, Color.BLACK), true);
              guiFrame.balloonMsfragger.setVisible(true);
            });
          }
          return; // stop iterations, we've found that there is no better version than the current

        } catch (Exception ex) {
          // no biggie
          continue;
        }
      }
    });
    t.start();

    return true;
  }

   public static void validateAndSaveDecoyTagSeqDb(String textDecoyTagFocusGained,
      JTextField textDecoyTagSeqDb,
      HashMap<String, BalloonTip> tipMap,
      final String newText,
      boolean updateOtherTags) {

    final JTextComponent comp = textDecoyTagSeqDb;
    final boolean isValid = validateAndSave(tipMap, comp, ThisAppProps.PROP_TEXTFIELD_DECOY_TAG,
        newText, ValidateTrue.getInstance());

    if (!isValid) {
      return;
    }

    // check if the filter line has changed since focus was gained
    final String savedText = textDecoyTagFocusGained;
    final String oldText = savedText != null ? savedText : comp.getText().trim();
    final String updText = newText != null ? newText : comp.getText().trim();

    // newText == null means it was a programmatic update
    if (!updateOtherTags || oldText.equals(updText)) {
      return;
    }

  }

  /**
   * Checks if a file is a JAR file and that it contains MSFragger.class at the top level.
   *
   * @param guiFrame
   * @param path file to check.
   * @return True if it's a real JAR file with MSFragger.class at the top level inside.
   */
   public static boolean validateAndSaveMsfraggerPath(MsfraggerGuiFrame guiFrame, final String path) {
    boolean isJarValid = validateMsfraggerJarContents(path);
    if (isJarValid) {
      guiFrame.getTextBinMsfragger().setText(path);
      ThisAppProps.save(ThisAppProps.PROP_BIN_PATH_MSFRAGGER, path);
    }

    if (guiFrame.balloonMsfragger != null) {
      guiFrame.balloonMsfragger.closeBalloon();
      guiFrame.balloonMsfragger = null;
    }

    boolean isPathValid = validateMsfraggerPath(path);
    boolean isVersionValid = isJarValid && validateMsfraggerVersion(guiFrame, path);
    boolean isJavaValid = isVersionValid && validateMsfraggerJavaVersion(guiFrame.balloonBgColor, guiFrame.fraggerVer,
            guiFrame.getLblFraggerJavaVer(), guiFrame.getTextBinMsfragger(), guiFrame.tipMap);

    if (!isPathValid) {
      final String downloadUrl = MsfraggerProps.getProperties().getProperty(MsfraggerProps.PROP_DOWNLOAD_URL, "");
      JEditorPane ep = SwingUtils.createClickableHtml(String.format(
          "<html>Could not find MSFragger jar file at this location.<br/>\n"
              + "Corresponding panel won't be active.<br/><br/>"
              + "<b>If that's the first time you're using %s</b>,<br/>"
              + "you will need to <a href=\"%s\">download MSFragger.jar (click here)</a> first.<br/>"
              + "Use the button on the right to proceed to the download website.",
          Version.PROGRAM_TITLE, downloadUrl), guiFrame.balloonBgColor);

      guiFrame.balloonMsfragger = new BalloonTip(guiFrame.getTextBinMsfragger(), ep,
          new RoundedBalloonStyle(5, 5, guiFrame.balloonBgColor, Color.BLACK), true);
      guiFrame.balloonMsfragger.setVisible(true);
    } else if (!isJarValid) {
      final String downloadUrl = MsfraggerProps.getProperties().getProperty(MsfraggerProps.PROP_DOWNLOAD_URL, "");
      JEditorPane ep = SwingUtils.createClickableHtml(String.format(
          "<html>Looks like you selected an existing jar file, but we<br/>\n"
              + "don't recognize it as a valid MSFragger distribution.<br/><br/>"
              + "<b>If that's the first time you're using %s</b>,<br/>"
              + "you will need to <a href=\"%s\">download MSFragger.jar (click here)</a> first.<br/>"
              + "Use the button on the right to proceed to the download website.",
          Version.PROGRAM_TITLE, downloadUrl), guiFrame.balloonBgColor);

      guiFrame.balloonMsfragger = new BalloonTip(guiFrame.getTextBinMsfragger(), ep,
          new RoundedBalloonStyle(5, 5, guiFrame.balloonBgColor, Color.BLACK), true);
      guiFrame.balloonMsfragger.setVisible(true);
    }

    final boolean msfraggerEnabled = isJarValid && isVersionValid && isJavaValid;
    EventBus.getDefault().postSticky(new MessageValidityFragger(msfraggerEnabled));

    // rerun slicing checks
    validateMsadjusterEligibility(guiFrame.fraggerVer);
    validateMsfraggerMassCalibrationEligibility(guiFrame.fraggerVer);
    validateDbslicing(guiFrame.fraggerVer);

    return isJarValid;
  }

   public static boolean validateMsfraggerJarContents(String path) {
    if (!validateMsfraggerPath(path)) {
      return false;
    }
    Path p = Paths.get(path).toAbsolutePath();
    final boolean[] found = {false};
    try (FileSystem fs = FileSystems.newFileSystem(p, null)) {
      for (Path root : fs.getRootDirectories()) {
        Files.walkFileTree(root, new SimpleFileVisitor<Path>() {
          Pattern regex = Pattern.compile("msfragger.*\\.jar", Pattern.CASE_INSENSITIVE);

          @Override
          public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) {
            String fileName = file.getFileName().toString();
            if ("MSFragger.class".equalsIgnoreCase(fileName)) {
              found[0] = true;
              return FileVisitResult.TERMINATE;
            } else if (regex.matcher(fileName).find()) {
              found[0] = true;
              return FileVisitResult.TERMINATE;
            }
            return FileVisitResult.CONTINUE;
          }
        });
      }
    } catch (IOException ex) {
      // doesn't matter
      java.util.logging.Logger
          .getLogger(MsfraggerGuiFrame.class.getName()).log(Level.SEVERE, null, ex);
    }

    return found[0];
  }

   public static boolean validateMsfraggerJavaVersion(Color balloonBgColor, String fraggerVer,
      JLabel lblFraggerJavaVer, JTextField textBinMsfragger, HashMap<String, BalloonTip> tipMap) {
    final boolean javaAtLeast18 = SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_8);
    final boolean is64bitJava = System.getProperty("sun.arch.data.model").equals("64");
    final VersionComparator vc = new VersionComparator();
    final MsfraggerVersionComparator mvc = new MsfraggerVersionComparator();
    SwingUtilities.invokeLater(() -> {
      BalloonTip tip = tipMap.remove(MsfraggerGuiFrame.TIP_NAME_FRAGGER_JAVA_VER);
      if (tip != null) {
        tip.closeBalloon();
      }
      String msg = null;

      if (!is64bitJava) {
        msg = "MSFragger requires <b>64-bit</b> Java.";
      } else if (!javaAtLeast18) {
        msg = "Msfragger requires Java 1.8. Your version is lower.";
      } else {
        // check for Java 9
        final String jver = SystemUtils.JAVA_SPECIFICATION_VERSION;
        final String fver = fraggerVer != null ? fraggerVer
            : Msfragger.testJar(textBinMsfragger.getText()).version;
        if (jver != null && fver != null) {
          if (mvc.compare(fver, "20180316") < 0 && vc.compare(jver, "1.9") >= 0) {
            msg = "Looks like you're "
                + "running Java 9 or higher with MSFragger v20180316 or lower.<br/>"
                + "That version of MSFragger only supports Java 8.";
          }
        }
      }

      if (msg != null) {
        JEditorPane ep = SwingUtils.createClickableHtml(msg
                + "<br/>Download <a href=\"https://www.java.com/en/download/manual.jsp\">here</a> or see the configuration help page (link below).\n.",
            true, false, balloonBgColor);
        tip = new BalloonTip(lblFraggerJavaVer, ep,
            new RoundedBalloonStyle(5, 5, balloonBgColor, Color.BLACK), true);
        tipMap.put(MsfraggerGuiFrame.TIP_NAME_FRAGGER_JAVA_VER, tip);
        tip.setVisible(true);
      }
    });
    return javaAtLeast18 && is64bitJava;
  }

  public static boolean validateAndSaveFastaPath(final MsfraggerGuiFrame guiFrame, String path) {
    boolean isValid = validateFastaPath(path);
    if (isValid) {
      guiFrame.getTextSequenceDbPath().setText(path);
      ThisAppProps.save(ThisAppProps.PROP_DB_FILE_IN, path);
      Thread thread;
      thread = new Thread(() -> {
        Path p = Paths.get(guiFrame.getTextSequenceDbPath().getText());
        if (!Files.exists(p)) {
          return;
        }
        try (BufferedReader br = new BufferedReader(new InputStreamReader(Files.newInputStream(p),
            StandardCharsets.UTF_8))) {
          String line;
          final List<String> descriptors = new ArrayList<>();
          while ((line = br.readLine()) != null) {
            if (!line.startsWith(">")) {
              continue;
            }
            descriptors.add(line);
          }
          SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
              String format = "###,###";
              DecimalFormatSymbols otherSymbols = new DecimalFormatSymbols(Locale.ROOT);
              otherSymbols.setDecimalSeparator(',');
              otherSymbols.setGroupingSeparator(' ');
              DecimalFormat df = new DecimalFormat(format, otherSymbols);
              guiFrame.getLblFastaCount().setText(String.format("%s entries", df.format(descriptors.size())));
            }
          });
        } catch (IOException ex) {
          return;
        }
      });
      thread.start();
    }

    final JComponent anchor = guiFrame.getTextSequenceDbPath();
    final String name = "textSequenceDbPath";
    BalloonTip tip = guiFrame.tipMap.remove(name);
    if (tip != null) {
      tip.closeBalloon();
    }

    if (!isValid) {
      tip = new BalloonTip(anchor, "<html>Could not find database file.");
      tip.setVisible(true);
      guiFrame.tipMap.put(name, tip);
    }

    return isValid;
  }

   public static boolean validateFastaPath(String path) {
    if (StringUtils.isNullOrWhitespace(path)) {
      return false;
    }
    try {
      Path p = Paths.get(path).toAbsolutePath();
      return Files.exists(p) && !Files.isDirectory(p);
    } catch (Exception e) {
      return false;
    }
  }

   public static void validateAndSavePeptideProphetCmdLineOptions(JTextArea textPepProphCmd,
      String textPepProphetFocusGained, HashMap<String, BalloonTip> tipMap) {
    final JTextComponent comp = textPepProphCmd;
    final boolean isValid = validateAndSave(tipMap, comp, ThisAppProps.PROP_TEXT_CMD_PEPTIDE_PROPHET,
        null, ValidateTrue.getInstance());

    if (!isValid) {
      return;
    }

    // check if the filter line has changed since focus was gained
    final String savedText = textPepProphetFocusGained;
    final String oldText = savedText != null ? savedText : comp.getText().trim();
    final String updText = comp.getText().trim();
  }

  public static void validateSpeclibgen() {
    log.debug("entered validateSpeclibgen");
//    new Thread(() -> SpecLibGen.get().init()).start();
    SpecLibGen.get().init();
  }

  public static void validateDbslicing(String fraggerVer) {
    log.debug("entered validateDbslicing");
//    new Thread(() -> DbSlice.get().init(fraggerVer)).start();
    DbSlice.get().init(fraggerVer);
  }

   public static void urlEventHandle(HyperlinkEvent evt) {
    if (evt.getEventType().equals(HyperlinkEvent.EventType.ACTIVATED)) {

      URI uri;
      try {
        uri = evt.getURL().toURI();
      } catch (URISyntaxException ex) {
        JOptionPane.showMessageDialog(null,
            "Could not convert URL to URI: " + evt.getURL(),
            "Cannot Open Link", JOptionPane.WARNING_MESSAGE);
        return;
      }

      if (Desktop.isDesktopSupported()) {
        Desktop desktop = Desktop.getDesktop();
        try {
          desktop.browse(uri);
        } catch (IOException e) {
          JOptionPane.showMessageDialog(null,
              "Failed to open " + uri + " - your computer is likely misconfigured.\n"
                  + "Error Message: " + e.getMessage(),
              "Cannot Open Link", JOptionPane.WARNING_MESSAGE);
        }
      } else {
        JOptionPane.showMessageDialog(null, "Java is not able to open a browser on your computer.",
            "Cannot Open Link", JOptionPane.WARNING_MESSAGE);
      }
    }
  }

   public static void userLoadForms(MsfraggerGuiFrame guiFrame) {
     FileNameExtensionFilter filter = new FileNameExtensionFilter("Config/Properties",
    "config", "properties", "params", "para", "conf", "txt");
    Path p = userShowLoadFileDialog("Load all FragPipe parameters", filter, guiFrame);
    if (p == null) {
      return;
    }
    try {
      guiFrame.formRead(Files.newInputStream(p));
    } catch (IOException e) {
      JOptionPane.showMessageDialog(guiFrame,
              "<html>Could not load the saved file: <br/>" + e.getMessage(), "Error",
              JOptionPane.ERROR_MESSAGE);
    }
  }

  public static String createPhilosopherCitationHtml(JLabel lblFraggerJavaVer) {
    // for copying style
    Font font = lblFraggerJavaVer.getFont();

    // create some css from the label's font
    StringBuilder style = new StringBuilder();
    style.append("font-family:").append(font.getFamily()).append(";");
    style.append("font-weight:").append(font.isBold() ? "bold" : "normal").append(";");
    style.append("font-size:").append(font.getSize()).append("pt;");

    StringBuilder sb = new StringBuilder();
    sb.append("<html>");

    sb.append("<head>");
    sb.append("</head>");

    sb.append("<body style=\"").append(style.toString()).append("\"");
    //sb.append("<body>");

    sb.append("<p style=\"margin-top: 0\">");
    sb.append("More info: <a href=\"https://nesvilab.github.io/philosopher/\">Philosopher GitHub page</a>");
    sb.append("<br/>");
    sb.append("</p>");

    sb.append("</body>");
    sb.append("</html>");

    return sb.toString();
  }

   public static void userSaveForms(MsfraggerGuiFrame guiFrame) {
    Path p = userShowSaveFileDialog("Save all FragPipe parameters", "fragpipe.config", guiFrame);
    if (p == null) {
      return;
    }
    try {
      guiFrame.formWrite(Files.newOutputStream(p));
    } catch (IOException ex) {
      JOptionPane.showMessageDialog(guiFrame, "<html>Could not save file: <br/>" + p.toString()
              + "<br/>" + ex.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
      return;
    }
  }

  /**
   * Fills all tabs' components that have names with values from the map.
   */
  public static void formFromMap(JTabbedPane tabPane, Map<String, String> map) {
    for (int i = 0; i < tabPane.getTabCount(); i++) {
      Component compAt = tabPane.getComponentAt(i);
      if (compAt instanceof Container) {
        SwingUtils.valuesFromMap((Container)compAt, map);
      }
    }
  }

   public static String validatePhilosopherPath(String path) {
    return PathUtils.testBinaryPath(path);
  }

   public static Path tryFindStartingPath(String currentPath) {
    try {
      Path path = Paths.get(currentPath);
      if (Files.exists(path)) {
        return path;
      }
      // didn't find anything yet
      if (currentPath.contains("/") || currentPath.contains("\\")) {
        // if there was a slash character, we can try the parent dir
        Path parent = path.getParent();
        if (Files.exists(parent)) {
          return parent;
        }
      }
    } catch (Exception ignored) {
      // supplied path was likely not good
    }
    return null;
  }

   public static void saveWorkdirText(JTextField txtWorkingDir) {
    final String text = txtWorkingDir.getText().trim();
    try {
      Path p = Paths.get(text);
      if (Files.exists(p)) {
        ThisAppProps.save(ThisAppProps.PROP_FILE_OUT, text);
      }
    } catch (Exception ignore) {}
  }

  public static void downloadPhilosopher() {

    try {
      Desktop.getDesktop()
          .browse(URI.create("https://github.com/Nesvilab/philosopher/releases/latest"));
    } catch (IOException ex) {
      java.util.logging.Logger
          .getLogger(MsfraggerGuiFrame.class.getName()).log(Level.SEVERE, null, ex);
    }
  }

   public static void validateMsadjusterEligibility(String fraggerVer) {
//    new Thread(() -> {
//    }).start();
    boolean enableMsadjuster = false;
    String minFraggerVer = MsfraggerProps.getProperties().getProperty(MsfraggerProps.PROP_MIN_VERSION_MSADJUSTER);
    if (minFraggerVer == null) {
      throw new IllegalStateException(MsfraggerProps.PROP_MIN_VERSION_MSADJUSTER +
          " property needs to be in Msfragger properties");
    }

    MsfraggerVersionComparator cmp = new MsfraggerVersionComparator();
    int fraggerVersionCmp = cmp.compare(fraggerVer, minFraggerVer);
    if (fraggerVersionCmp >= 0) {
      enableMsadjuster = true;
    }
    EventBus.getDefault().postSticky(new MessageValidityMsadjuster(enableMsadjuster));

  }

  public static UniqueLcmsFilesTableModel createTableModelRawFiles() {
    List<TableModelColumn<InputLcmsFile, ?>> cols = new ArrayList<>();

    TableModelColumn<InputLcmsFile, String> colPath = new TableModelColumn<>(
        "Path (can drag & drop from Explorer)",
        String.class, false, data -> data.getPath().toString());
    TableModelColumn<InputLcmsFile, String> colExp = new TableModelColumn<>(
        "Experiment (can be empty)", String.class, true, InputLcmsFile::getExperiment);
    TableModelColumn<InputLcmsFile, Integer> colRep = new TableModelColumn<>(
        "Replicate (can be empty)", Integer.class, true, InputLcmsFile::getReplicate);
    cols.add(colPath);
    cols.add(colExp);
    cols.add(colRep);

    UniqueLcmsFilesTableModel m = new UniqueLcmsFilesTableModel(cols, 0);

    return m;
  }

  public static void validateMsfraggerMassCalibrationEligibility(String fraggerVer) {
//    new Thread(() -> {
//    }).start();
    boolean enableCalibrate = false;
    String minFraggerVer = MsfraggerProps.getProperties().getProperty(MsfraggerProps.PROP_MIN_VERSION_FRAGGER_MASS_CALIBRATE);
    if (minFraggerVer == null) {
      throw new IllegalStateException(MsfraggerProps.PROP_MIN_VERSION_FRAGGER_MASS_CALIBRATE +
          " property needs to be in Msfragger properties");
    }
    MsfraggerVersionComparator cmp = new MsfraggerVersionComparator();
    int fraggerVersionCmp = cmp.compare(fraggerVer, minFraggerVer);
    if (fraggerVersionCmp >= 0) {
      enableCalibrate = true;
    }
    log.debug("Posting enableCalibrate = {}", enableCalibrate);
    EventBus.getDefault().postSticky(new MessageValidityMassCalibration(enableCalibrate));

  }

  public static void printProcessDescription(Color COLOR_CMDLINE, Color COLOR_TOOL,
      Color COLOR_WORKDIR,
      TextConsole console,
      ProcessBuilderInfo pbi) {
    if (!StringUtils.isNullOrWhitespace(pbi.name)) {
      LogUtils.print(COLOR_TOOL, console, true, pbi.name, false);
    }
    if (pbi.pb.directory() != null) {
      LogUtils.print(COLOR_WORKDIR, console, true, " [Work dir: " + pbi.pb.directory() + "]", false);
    }
    LogUtils.println(console, "");
    final String cmd = org.apache.commons.lang3.StringUtils.join(pbi.pb.command(), " ");
    LogUtils.print(COLOR_CMDLINE, console, true, cmd, true);
  }

  public static class LcmsFileAddition {
    List<Path> paths;
    List<Path> toAdd;

    public LcmsFileAddition(List<Path> paths, List<Path> toAdd) {
      this.paths = paths;
      this.toAdd = toAdd;
    }
  }

  public static void processAddedLcmsPaths(LcmsFileAddition files, MsfraggerGuiFrame guiFrame) {
    // vet/check input LCMS files for bad naming
    final javax.swing.filechooser.FileFilter ff = CmdMsfragger.getFileChooserFilter(guiFrame.getExtBinSearchPaths());
    final HashMap<Path, Set<String>> reasonsDir = new HashMap<>();
    final HashMap<Path, Set<String>> reasonsFn = new HashMap<>();
    //final HashMap<String, List<Path>> reasonsRev = new HashMap<>();
    final String allowedChars = "[A-Za-z0-9-_+.\\[\\]()]";
    Pattern re = Pattern.compile(allowedChars + "+");
    final String REASON_NON_ASCII = "Non-ASCII chars";
    final String REASON_PATH_SPACES = "Path contains spaces";
    final String REASON_FN_DOTS = "Filename contains dots";
    final String REASON_UNSUPPORTED = "Not supported";
    final String REASON_DISALLOWED_CHARS = "Contains characters other than: " + allowedChars;

    for (Path path : files.paths) {
      Set<String> why = InputLcmsFile.validatePath(path.getParent().toString());
      if (!why.isEmpty()) {
        reasonsDir.put(path, why);
      }
    }

    for (Path path : files.paths) {
      Set<String> why = InputLcmsFile.validateFilename(path.getFileName().toString());
      if (!why.isEmpty()) {
        reasonsFn.put(path, why);
      }
    }

    // in case there were suspicious paths
    if (!reasonsDir.isEmpty() || !reasonsFn.isEmpty()) {
      HashMap<Path, String> path2reasons = new HashMap<>();
      for (Entry<Path, Set<String>> kv : reasonsDir.entrySet()) {
        for (String reason : kv.getValue()) {
          path2reasons.compute(kv.getKey(), (path, s) -> s == null ? "Direcotry " + reason : s.concat(", Direcotry " + reason));
        }
      }
      for (Entry<Path, Set<String>> kv : reasonsFn.entrySet()) {
        for (String reason : kv.getValue()) {
          path2reasons.compute(kv.getKey(), (path, s) -> s == null ? "File name " + reason : s.concat(", File name " + reason));
        }
      }
      String[] columns = {"Reasons", "Path"};
      String[][] data = new String[path2reasons.size()][2];
      int index = -1;
      for (Entry<Path, String> kv : path2reasons.entrySet()) {
        data[++index][0] = kv.getValue();
        data[index][1] = kv.getKey().toString();
      }

      DefaultTableModel model = new DefaultTableModel(data, columns);
      JTable table = new JTable(model);
      table.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
      JPanel panel = new JPanel(new BorderLayout());
      panel.add(new JLabel("<html>Found problems with some files (" + path2reasons.size() + " of " + files.paths.size() + ").<br/>"
          + "This <b>will likely cause trouble</b> with some of processing tools.<br/><br/>"
          + "What do you want to do with these files?<br/>"), BorderLayout.NORTH);
      panel.add(Box.createVerticalStrut(100), BorderLayout.CENTER);
      panel.add(new JScrollPane(table), BorderLayout.CENTER);
      SwingUtils.makeDialogResizable(panel);

      String[] options;
      if (!reasonsFn.isEmpty()) {
        options = new String[]{"Cancel", "Add anyway", "Only add well-behaved files", "Try rename files"};
      } else {
        options = new String[]{"Cancel", "Add anyway", "Only add well-behaved files"};
      }

      int confirmation = JOptionPane
          .showOptionDialog(guiFrame, panel, "Add these files?",
              JOptionPane.DEFAULT_OPTION, JOptionPane.WARNING_MESSAGE, null, options, options[0]);

      switch (confirmation) {
        case 0:
          return;
        case 1:
          break;
        case 2:
          files.toAdd = files.toAdd.stream().filter(path -> !path2reasons.containsKey(path)).collect(Collectors.toList());
          break;
        case 3: // rename files
          int confirm1 = SwingUtils.showConfirmDialog(guiFrame, new JLabel(
              "<html>Attempt to rename files without moving them.<br/>\n" +
                  "This is a non-reversible operation.<br/><br/>\n" +
                  "We'll show you a preview before proceeding with actual renaming.<br/>\n" +
                  "Do you want to continue?"));
          if (JOptionPane.YES_OPTION != confirm1) {
            return;
          }
          final Map<Path, Path> toRename = reasonsFn.keySet().stream()
              .collect(Collectors.toMap(Function.identity(), InputLcmsFile::renameBadFile));
          Set<Path> uniqueRenamed = new HashSet<>(toRename.values());
          if (uniqueRenamed.size() != reasonsFn.size()) {
            SwingUtils.showDialog(guiFrame, new JLabel(
                    "<html>Renaming given files according to our scheme would result<br/>\n" +
                        "in clashing file paths. Renaming cancelled. Consider renaming manually.<br/>\n" +
                        "It is preferable to not have spaces in file names, not have more than one dot<br/>\n" +
                        "and to not use non-latin characters."),
                "Not safe to rename files", JOptionPane.WARNING_MESSAGE);
            return;
          }

          final Map<Path, Path> existingRenames = new HashMap<>();
          for (Entry<Path, Path> kv : toRename.entrySet()) {
            if (Files.exists(kv.getValue())) {
              existingRenames.put(kv.getKey(), kv.getValue());
            }
          }
          if (!existingRenames.isEmpty()) {
            JPanel pane = new JPanel(new BorderLayout());
            pane.add(new JLabel("<html>Renaming given files according to our scheme would result<br/>\n" +
                "in file paths that already exist on your computer.<br/>\n" +
                "Renaming cancelled."), BorderLayout.NORTH);

            pane.add(new JScrollPane(SwingUtils.tableFromTwoSiblingFiles(existingRenames)));
            SwingUtils.showDialog(guiFrame, pane,
                "Not safe to rename files", JOptionPane.WARNING_MESSAGE);
            return;
          }

        {
          JPanel pane = new JPanel(new BorderLayout());
          pane.add(new JLabel("<html>Proposed renaming scheme, do you agree?<br/>\n"));
          pane.add(new JScrollPane(SwingUtils.tableFromTwoSiblingFiles(toRename)));
          int confirm2 = SwingUtils.showConfirmDialog(guiFrame, pane);
          if (JOptionPane.YES_OPTION != confirm2) {
            return;
          }
        }

        final Map<Path, Path> couldNotRename = new HashMap<>();
        final Map<Path, Path> renamedOk = new HashMap<>();
        Runnable runnable = () -> {
          for (Entry<Path, Path> kv : toRename.entrySet()) {
            try {
              Files.move(kv.getKey(), kv.getValue());
              renamedOk.put(kv.getKey(), kv.getValue());
            } catch (Exception e) {
              log.error(String.format("From '%s' to '%s' at '%s'",
                  kv.getKey().getFileName(), kv.getValue().getFileName(), kv.getKey().getParent()));
              couldNotRename.put(kv.getKey(), kv.getValue());
            }
          }
        };

        SwingUtils.DialogAndThread dat = SwingUtils.runThreadWithProgressBar("Renaming files", guiFrame, runnable);
        dat.thread.start();
        dat.dialog.setVisible(true);

        if (!couldNotRename.isEmpty()) {
          JPanel pane = new JPanel(new BorderLayout());
          pane.add(new JLabel("<html>Unfortunately could not rename some of the files:<br/>"), BorderLayout.NORTH);
          pane.add(new JScrollPane(SwingUtils.tableFromTwoSiblingFiles(couldNotRename)), BorderLayout.CENTER);
          SwingUtils.showDialog(guiFrame, pane, "Renaming failed", JOptionPane.WARNING_MESSAGE);
          return;
        }

        // renaming succeeded, change paths to renamed ones
        files.toAdd = files.toAdd.stream().map(path -> renamedOk.getOrDefault(path, path)).collect(Collectors.toList());

        break;
      }
    }
  }
}
