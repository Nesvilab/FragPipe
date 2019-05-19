package umich.msfragger.gui.dialogs;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.KeyStroke;
import umich.msfragger.params.ThisAppProps;
import umich.msfragger.util.StringUtils;

public class DbIdDialog extends JDialog {
  private static final String PROP_UNIPROT_IDS = "database.uniprot.ids";
  public static final Pattern RE_UNIPROT_ID = Pattern.compile("Uniprot ID:.*?\b(.+?)\b", Pattern.CASE_INSENSITIVE);

  private JPanel contentPane;
  private JButton buttonOK;
  private JButton buttonCancel;
  private JPanel radioPanel;
  private ButtonGroup btnGroupUniprotIds;
  private List<JRadioButton> radios;
  private JRadioButton radioOther;
  private JTextField textOther;

  public DbIdDialog() {
    setContentPane(contentPane);
    setModal(true);
    setTitle("Select organism / UniProt ID");
    getRootPane().setDefaultButton(buttonOK);

    buttonOK.addActionListener(e -> onOK());

    buttonCancel.addActionListener(e -> onCancel());

    // call onCancel() when cross is clicked
    setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
    addWindowListener(new WindowAdapter() {
      public void windowClosing(WindowEvent e) {
        onCancel();
      }
    });

    // call onCancel() on ESCAPE
    contentPane.registerKeyboardAction(e -> onCancel(), KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0),
        JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);

    initMore();
  }

  private void initMore() {
    radios = new ArrayList<>();
    btnGroupUniprotIds = new ButtonGroup();
    String ids = null;
    try {
      //ids = ThisAppProps.getLocalProperties().getProperty(PROP_UNIPROT_IDS);
    } catch (IllegalStateException ignored) {}
    if (!StringUtils.isNullOrWhitespace(ids)) {
      Arrays.stream(ids.split(";;")).map(String::trim).forEach(id -> radios.add(new JRadioButton(id)));
    } else {
      radios.add(new JRadioButton("Human - Uniprot ID: UP000005640"));
      radios.add(new JRadioButton("Mouse - Uniprot ID: UP000000589"));
    }
    radioOther = new JRadioButton("Other:");
    radios.add(radioOther);
    if (radios.isEmpty())
      throw new IllegalStateException("no radio buttons");
    radios.get(0).setSelected(true);
    radios.forEach(r -> btnGroupUniprotIds.add(r));

    JPanel boxPanel = new JPanel();
    boxPanel.setLayout(new BoxLayout(boxPanel, BoxLayout.Y_AXIS));

    for (JRadioButton radio : radios) {
      boxPanel.add(radio);
    }
    textOther = new JTextField();
    textOther.setToolTipText("<html>Uniprot ID, similar to: UP000005640<br/>See http://www.uniprot.org/proteomes");
    boxPanel.add(textOther);

    JScrollPane scroll = new JScrollPane(boxPanel);
    radioPanel.add(scroll, BorderLayout.CENTER);
  }

  /**
   * Only to be called if user clicked OK on dialog.
   */
  public String getSelectedUniprotId() {
    for (JRadioButton radio : radios) {
      if (!radio.isSelected())
        continue;
      if (radioOther == radio) {
        // selected manual selection
        final String text = textOther.getText().trim();
        return StringUtils.isNullOrWhitespace(text) ? null : text;
      } else {
        final String text = radio.getText();
        Matcher m = RE_UNIPROT_ID.matcher(text);
        if (!m.find())
          throw new IllegalStateException("Could not match radio button text to regular expression.");
        String group = m.group(1);
        return group;
      }
    }
    return null;
  }

  private void onOK() {
    // add your code here
    dispose();
  }

  private void onCancel() {
    // add your code here if necessary
    dispose();
  }

  public static void main(String[] args) {
    DbIdDialog dialog = new DbIdDialog();
    dialog.pack();
    dialog.setVisible(true);
    System.exit(0);
  }
}
