package com.dmtavt.fragpipe.api;

import static com.dmtavt.fragpipe.tabs.TabMsfragger.PROP_FILECHOOSER_LAST_PATH;

import com.dmtavt.fragpipe.params.ThisAppProps;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.swing.FileChooserUtils;
import java.awt.Component;
import java.awt.Desktop;
import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.event.HyperlinkEvent;
import javax.swing.filechooser.FileNameExtensionFilter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class OldUtilMethods {
  private static final Logger log = LoggerFactory.getLogger(OldUtilMethods.class);
  private OldUtilMethods() {}


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
    FileChooserUtils.setPath(fc, ThisAppProps.load(PROP_FILECHOOSER_LAST_PATH));
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

}
