package umich.msfragger.gui.dialogs;

import com.github.chhh.utils.swing.UiCombo;
import com.github.chhh.utils.swing.UiUtils;
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.KeyStroke;
import javax.swing.ScrollPaneConstants;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.greenrobot.eventbus.EventBus;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import umich.msfragger.cmd.ToolingUtils;
import umich.msfragger.gui.api.SimpleETable;
import umich.msfragger.gui.api.SimpleTableModel;
import umich.msfragger.gui.api.TableModelColumn;
import umich.msfragger.params.tmtintegrator.QuantLabel;
import umich.msfragger.params.tmtintegrator.QuantLabelAnnotation;
import umich.msfragger.params.tmtintegrator.TmtAnnotationTable.TmtTableRow;

public class TmtAnnotationDialog extends javax.swing.JDialog {
  private static final Logger log = LoggerFactory.getLogger(TmtAnnotationDialog.class);
  private final TmtTableRow tmtTableRow;
  private int numChannels;
  private Path file;

  private JPanel p;
  private JButton buttonOK;
  private JButton buttonCancel;
  private JButton buttonLoad;
  private SimpleETable table;
  private UiCombo comboLabelName;
  private SimpleTableModel<QuantLabelAnnotation> model;

  public TmtAnnotationDialog(TmtTableRow tmtTableRow, int numChannels) {
    this.tmtTableRow = tmtTableRow;
    this.numChannels = numChannels;
    init();
  }

  @Override
  public void dispose() {
    super.dispose();
//    EventBus.getDefault().unregister(this);
  }

  public TmtAnnotationDialog(TmtTableRow tmtTableRow, int numChannels, Path file) {
    this(tmtTableRow, numChannels);
    this.file = file;
  }

  private void init() {
    Dimension dim = new Dimension(800, 600);
    this.setPreferredSize(dim);
    this.setLayout(new BorderLayout());

    p = new JPanel();
    JScrollPane scroll = new JScrollPane(p,
        ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
    this.setContentPane(scroll);

    buttonOK = new JButton("OK");
    buttonCancel = new JButton("Cancel");
    table = createTable();

    buttonLoad = new JButton("Populate with labels for");
    buttonLoad.addActionListener(e -> {
      String labelName = (String) comboLabelName.getSelectedItem();
      Optional<QuantLabel> label = QuantLabel.LABELS.stream()
          .filter(ql -> ql.getName().equalsIgnoreCase(labelName)).findFirst();
      if (!label.isPresent()) {
        throw new IllegalStateException("Label from dropdown menu not present: " + labelName);
      }
      model.dataClear();
      for (int i = 0; i < label.get().getReagentNames().size(); i++) {
        String reagent = label.get().getReagentNames().get(i);
        QuantLabelAnnotation annotation = new QuantLabelAnnotation(reagent,
            String.format("sample-%02d", i + 1));
        model.dataAdd(annotation);
      }
    });
    comboLabelName = UiUtils.createUiCombo(QuantLabel.LABELS.stream().map(QuantLabel::getName)
        .collect(Collectors.toList()));
    Optional<QuantLabel> label = QuantLabel.LABELS.stream()
        .filter(ql -> ql.getReagentNames().size() == numChannels).findFirst();
    label.ifPresent(quantLabel -> comboLabelName.setSelectedItem(quantLabel.getName()));

    MigLayout layout = new MigLayout(new LC().fillX().debug());
    p.setLayout(layout);

    p.add(buttonLoad, new CC().split(3));
    p.add(comboLabelName, new CC().alignX("left").wrap());

    p.add(new JScrollPane(table), new CC().grow().spanX().wrap());

    p.add(buttonOK, new CC().tag("ok").split(2));
    p.add(buttonCancel, new CC().tag("cancel").wrap());




    setContentPane(scroll);
    setModal(true);
    setModalityType(ModalityType.APPLICATION_MODAL);
    setTitle("Annotate labeling experiment: '" + tmtTableRow.expName + "'");
    setIconImages(ToolingUtils.loadIcon());
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
    p.registerKeyboardAction(e -> onCancel(), KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0),
        JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);
  }

  private void onOK() {
    // add your code here
    dispose();
  }

  private void onCancel() {
    // add your code here if necessary
    dispose();
  }

  private static class QuantLabelAnnotationModel extends SimpleTableModel<QuantLabelAnnotation> {
    private static final Logger log = LoggerFactory.getLogger(QuantLabelAnnotationModel.class);

    public QuantLabelAnnotationModel(
        List<TableModelColumn<QuantLabelAnnotation, ?>> cols, int initSize) {
      super(cols, initSize);
    }

    @Override
    public void setValueAt(Object aValue, int rowIndex, int columnIndex) {
      super.setValueAt(aValue, rowIndex, columnIndex);
      log.debug("setValueAt() called for row: {}, col: {}", rowIndex, columnIndex);
      switch (columnIndex) {
        case 0:
          break;
        case 1:
          dataGet(rowIndex).setSample((String)aValue);
          this.fireTableCellUpdated(rowIndex, columnIndex);
          break;
      }
    }
  }

  private SimpleETable createTable() {
//    TableModel model = JTableUtils.createTableModelFromBean(QuantLabelAnnotation.class,
//        Arrays.asList(new QuantLabelAnnotation("126", "test-sample")));

    List<TableModelColumn<QuantLabelAnnotation, ?>> cols = new ArrayList<>();
    cols.add(new TableModelColumn<>("Labeling reagent", String.class, false, QuantLabelAnnotation::getLabel));
    cols.add(new TableModelColumn<>("Sample name", String.class, true, QuantLabelAnnotation::getSample));
    final int numRows = 20;
//    model = new SimpleTableModel<>(cols, numRows);
    model = new QuantLabelAnnotationModel(cols, numRows);

    for (int i = 0; i < numRows; i++) {
      model.dataAdd(new QuantLabelAnnotation("", ""));
    }


    SimpleETable table = new SimpleETable(model);
    table.setFullyEditable(true);
    return table;
  }

  public static void main(String[] args) {
    TmtAnnotationDialog dialog = new TmtAnnotationDialog(new TmtTableRow("test-experiment", "Not yet selected"), 6);
    dialog.pack();
    dialog.setVisible(true);
    System.exit(0);
  }
}
