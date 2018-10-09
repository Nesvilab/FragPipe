package umich.msfragger.util.swing;

import java.awt.Component;
import java.io.File;
import java.nio.file.Paths;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JTextField;
import umich.msfragger.util.GhostText;
import umich.msfragger.util.StringUtils;
import umich.msfragger.util.SwingUtils;

public class FormEntry {
  public final JComponent comp;
  public final String propName;
  public final String label;

  public FormEntry(String propName, String label, JComponent comp) {
    this.propName = propName;
    this.label = label;
    this.comp = comp;
    comp.setName(propName);
  }

  public JLabel label() {
    JLabel l = new JLabel(label);
    l.setLabelFor(comp);
    return l;
  }

  public JButton browseButton(String buttonText, String approveText, String dialogTitle,
      Component dialogParent, String ghostText) {
    if (!(comp instanceof JTextField))
      throw new IllegalStateException(
          "Can only call browseButton() method for FormEntries which are JTextField");

    final JTextField tf = (JTextField)comp;
    if (!StringUtils.isNullOrWhitespace(ghostText))
      GhostText.register(tf, ghostText, GhostText.LIGHT_GREY);

    final JButton btn = new JButton(buttonText);
    btn.addActionListener(e -> {
      JFileChooser fc = new JFileChooser();
      fc.setApproveButtonText(approveText);
      fc.setDialogTitle(dialogTitle);
      fc.setMultiSelectionEnabled(false);
      fc.setAcceptAllFileFilterUsed(true);

      String s = tf.getText().trim();
      if (!StringUtils.isNullOrWhitespace(s) && !ghostText.equals(s)) {
        try {
          SwingUtils.setFileChooserPath(fc, Paths.get(s).toString());
        } catch (Exception ignore) {}
      }
      if (JFileChooser.APPROVE_OPTION == fc.showOpenDialog(dialogParent)) {
        File f = fc.getSelectedFile();
        tf.setText(f.getAbsolutePath());
      }
    });
    return btn;
  }
}
