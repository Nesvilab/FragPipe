package umich.msfragger.util.swing;

import com.github.chhh.utils.StringUtils;
import com.github.chhh.utils.swing.GhostText;
import com.github.chhh.utils.swing.GhostedTextComponent;
import java.awt.Component;
import java.io.File;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.stream.Collectors;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JTextField;
import umich.msfragger.util.SwingUtils;

public class  FormEntry {
  public final JComponent comp;
  public final String propName;
  public final String labelText;
  public final String tooltip;

  public FormEntry(String propName, String labelText, JComponent comp) {
    this(propName, labelText, comp, null);
  }

  public FormEntry(String propName, String labelText, JComponent comp, String tooltip) {
    this.comp = comp;
    this.propName = propName;
    this.labelText = labelText;
    this.tooltip = tooltip;
    init();
  }

  private void init() {
    comp.setName(propName);
    comp.setToolTipText(tooltip);
  }

  public JLabel label() {
    JLabel l = new JLabel(labelText);
    l.setLabelFor(comp);
    l.setToolTipText(tooltip);
    return l;
  }

  public JButton browseButton(String buttonText, final JFileChooser fc, String ghostText, String concat) {
    if (!(comp instanceof JTextField)) {
      throw new IllegalStateException(
          "Can only call browseButton() method for FormEntries which are JTextField");
    }
    if (fc.isMultiSelectionEnabled() && concat == null) {
      throw new IllegalArgumentException("Concatenation string must be non-null when FileChooser supports multi-selection.");
    }

    final JTextField tf = (JTextField)comp;
    if (!com.github.chhh.utils.StringUtils.isNullOrWhitespace(ghostText)) {
      if (tf instanceof GhostedTextComponent) {
        ((GhostedTextComponent) tf).setGhostText(ghostText);
      }
      com.github.chhh.utils.swing.GhostText.register(tf, ghostText, GhostText.LIGHT_GREY);
    }

    final JButton btn = new JButton(buttonText);
    btn.setToolTipText(tooltip);
    btn.addActionListener(e -> {
      String s = tf.getText().trim();
      if (!com.github.chhh.utils.StringUtils.isNullOrWhitespace(s) && !ghostText.equals(s)) {
        try {
          SwingUtils.setFileChooserPath(fc, Paths.get(s).toString());
        } catch (Exception ignore) {}
      }
      if (JFileChooser.APPROVE_OPTION == fc.showOpenDialog(SwingUtils.findParentFrameForDialog(comp))) {
        String newText = "Could not get selected file paths";

        File[] fs = fc.getSelectedFiles();
        if (fs.length !=  0) {
          newText = Arrays.stream(fs)
              .map(File::getAbsolutePath).collect(Collectors.joining(concat));
        } else {
          File f = fc.getSelectedFile();
          if (f != null && !StringUtils.isBlank(f.toString())) {
            newText = f.toString();
          }
        }


        tf.setText(newText);
      }
    });
    return btn;
  }
}
