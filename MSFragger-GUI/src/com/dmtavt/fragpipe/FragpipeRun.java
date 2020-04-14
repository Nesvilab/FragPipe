package com.dmtavt.fragpipe;

import com.dmtavt.fragpipe.api.Bus;
import com.dmtavt.fragpipe.messages.MessageClearConsole;
import com.dmtavt.fragpipe.messages.MessageRun;
import com.dmtavt.fragpipe.messages.MessageRunButtonEnabled;
import com.dmtavt.fragpipe.messages.MessageSaveCache;
import com.dmtavt.fragpipe.tabs.TabRun;
import com.github.chhh.utils.StringUtils;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.swing.JComponent;
import javax.swing.JOptionPane;
import umich.msfragger.gui.MsfraggerGuiFrameUtils;
import umich.msfragger.params.ThisAppProps;

public class FragpipeRun {

  private FragpipeRun() {}

  public static void run(MessageRun m) {
    Bus.post(new MessageSaveCache());
    Bus.post(new MessageClearConsole());
    Bus.post(new MessageRunButtonEnabled(false));

    boolean runConfigurationDone = false;

    try {
      final boolean isDryRun = m.isDryRun;

      TabRun tabRun = Bus.getStickyEvent(TabRun.class);
      if (tabRun == null)
        throw new IllegalStateException("TabRun has not been posted to the bus");

      // workdir
      String wdStr = tabRun.getWorkdirText();
      Fragpipe.propsVarSet(ThisAppProps.PROP_FILE_OUT, wdStr);
      final Path wd = validateWd(tabRun, wdStr);

      if (!isDryRun) {
        if (!Files.exists(wdPath)) {
          int confirmCreation = JOptionPane.showConfirmDialog(msfgf,
              "Output directory doesn't exist. Create?",
              "Create output directory?", JOptionPane.OK_CANCEL_OPTION);
          if (JOptionPane.OK_OPTION != confirmCreation) {
            msfgf.resetRunButtons(true);
            return;
          }
          try {
            Files.createDirectories(wdPath);
          } catch (Exception e) {
            // something went not right during creation of directory structure
            JOptionPane.showMessageDialog(msfgf,
                "Could not create directory structure.\n" + e.getMessage(), "Error",
                JOptionPane.ERROR_MESSAGE);
            msfgf.resetRunButtons(true);
            return;
          }

        } else {
          try (Stream<Path> inWd = Files.list(wdPath)) {
            if (inWd.findAny().isPresent()) {
              int confirm = JOptionPane.showConfirmDialog(msfgf,
                  "The output directory is not empty.\n\n"
                      + "Some files might be overwritten in:\n"
                      + " > " + wdPath.toString() + "\n\n"
                      + "Do you want to proceed?", "Confirmation", JOptionPane.YES_NO_OPTION);
              if (JOptionPane.YES_OPTION != confirm) {
                msfgf.resetRunButtons(true);
                return;
              }
            }
          } catch (Exception e) {
            JOptionPane.showMessageDialog(msfgf,
                "Could not create directory structure.\n" + e.getMessage(), "Error",
                JOptionPane.ERROR_MESSAGE);
            msfgf.resetRunButtons(true);
            return;
          }
        }

        // make sure subdirs for experiments are created
        Set<Path> subdirs = msfgf.getLcmsFileGroups().values().stream().map(g -> g.outputDir(wdPath))
            .collect(Collectors.toSet());
        for (Path subdir : subdirs) {
          if (!Files.exists(subdir)) {
            try {
              Files.createDirectories(subdir);
            } catch (IOException e) {
              JOptionPane.showMessageDialog(msfgf,
                  "Could not create directory structure.\n" + e.getMessage(), "Error",
                  JOptionPane.ERROR_MESSAGE);
              msfgf.resetRunButtons(true);
              return;
            }
          }
        }
      }



      runConfigurationDone = true;
    } finally {
      if (!runConfigurationDone) {
        Bus.post(new MessageRunButtonEnabled(true));
      }
    }
  }

  private static Path validateWd(JComponent parent, String workingDir) {
    if (StringUtils.isBlank(workingDir)) {
      JOptionPane.showMessageDialog(parent, "Output directory can't be left empty.\n"
              + "Please select an existing directory for the output.", "Bad output directory path",
          JOptionPane.WARNING_MESSAGE);
      return null;
    }
    Path testWdPath;
    try {
      testWdPath = Paths.get(workingDir);
    } catch (InvalidPathException e) {
      JOptionPane.showMessageDialog(parent, "Output directory path is not a valid path.\n"
          + "Please select a directory for the output.", "Bad output directory path", JOptionPane.WARNING_MESSAGE);
      return null;
    }
    Pattern reWhitespace = Pattern.compile("\\s");
    if (reWhitespace.matcher(testWdPath.toString()).find()) {
      JOptionPane.showMessageDialog(parent,
          "Output directory path contains whitespace characters.\n"
              + "Some programs in the pipeline might not work properly in this case.\n\n"
              + "Please change output directory to one without spaces.", "Bad output directory path", JOptionPane.WARNING_MESSAGE);
      return null;
    }
    return testWdPath;
  }
}
