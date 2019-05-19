package umich.msfragger.gui.dialogs;

import java.awt.BorderLayout;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import umich.msfragger.params.ThisAppProps;
import umich.msfragger.util.StringUtils;

public class DbUniprotIdPanel extends JPanel {

  private ButtonGroup btnGroupUniprotIds;
  private List<JRadioButton> radios;
  private JRadioButton radioOther;
  private JTextField textOther;

  private static final String PROP_UNIPROT_IDS = "database.uniprot.ids";
  public static final Pattern RE_UNIPROT_ID = Pattern.compile("Uniprot ID:.*?\\b(.+?)\\b", Pattern.CASE_INSENSITIVE);

  public DbUniprotIdPanel() {
    initMore();
  }

  private void initMore() {
    radios = new ArrayList<>();
    btnGroupUniprotIds = new ButtonGroup();
    String ids = null;
    try {
      ids = ThisAppProps.getLocalProperties().getProperty(PROP_UNIPROT_IDS);
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
    this.setLayout(new BorderLayout());
    this.add(scroll, BorderLayout.CENTER);
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
        Matcher m = RE_UNIPROT_ID.matcher(text); // TODO: the regex doesnt work. Remove the \b modifier
        if (!m.find())
          throw new IllegalStateException("Could not match radio button text to regular expression.");
        String group = m.group(1);
        return group;
      }
    }
    return null;
  }

}
