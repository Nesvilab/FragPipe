/*
 * This file is part of FragPipe.
 *
 * FragPipe is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * FragPipe is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with FragPipe. If not, see <https://www.gnu.org/licenses/>.
 */

package org.nesvilab.fragpipe.dialogs;

import java.util.Locale;
import org.nesvilab.utils.swing.MigUtils;
import org.nesvilab.utils.swing.UiCombo;
import org.nesvilab.utils.swing.UiUtils;
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.KeyStroke;
import javax.swing.ScrollPaneConstants;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.nesvilab.fragpipe.cmd.ToolingUtils;
import org.nesvilab.fragpipe.api.SimpleETable;
import org.nesvilab.fragpipe.api.SimpleTableModel;
import org.nesvilab.fragpipe.api.TableModelColumn;
import org.nesvilab.fragpipe.tools.tmtintegrator.QuantLabel;
import org.nesvilab.fragpipe.tools.tmtintegrator.QuantLabelAnnotation;
import org.nesvilab.fragpipe.tools.tmtintegrator.TmtAnnotationTable.ExpNameToAnnotationFile;

public class QuantLabelAnnotationDialog extends javax.swing.JDialog {
  private static final Logger log = LoggerFactory.getLogger(QuantLabelAnnotationDialog.class);
  private final ExpNameToAnnotationFile expNameToFilePathRow;
  private String initLabelName;
  private List<QuantLabelAnnotation> initAnnotations;

  private JPanel p;
  private JButton buttonOK;
  private JButton buttonCancel;
  private JButton buttonLoad;
  private SimpleETable table;
  private UiCombo comboLabelName;
  private SimpleTableModel<QuantLabelAnnotation> model;
  private Frame parent;
  private int dialogResult = JOptionPane.CLOSED_OPTION;

  public QuantLabelAnnotationDialog(java.awt.Frame parent, ExpNameToAnnotationFile expNameToFilePathRow,
      String initLabelName, List<QuantLabelAnnotation> initAnnotations) {
    super(parent);
    this.expNameToFilePathRow = expNameToFilePathRow;
    this.initLabelName = initLabelName;
    this.initAnnotations = initAnnotations == null ? Collections.emptyList() : initAnnotations;
    this.parent = parent;
    init();
    postInit();
  }

  public SimpleTableModel<QuantLabelAnnotation> getModel() {
    return model;
  }

  @Override
  public void dispose() {
    super.dispose();
  }

  private void postInit() {
    this.setDefaultCloseOperation(DISPOSE_ON_CLOSE);
    this.setLocationRelativeTo(parent);
    pack();
  }

  private void init() {
    Dimension dim = new Dimension(800, 600);
    this.setPreferredSize(dim);
    this.setLayout(new BorderLayout());

    p = new JPanel();
    JScrollPane scroll = new JScrollPane(p,
        ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
    this.setContentPane(scroll);

    buttonOK = new JButton("Save");
    buttonCancel = new JButton("Cancel");
    createTable();
    model.dataClear();
    model.dataAddAll(initAnnotations);

    JLabel labelLoad = new JLabel("Load annotation stubs for:");
    buttonLoad = new JButton("Load into table");
    buttonLoad.addActionListener(e -> {
      String labelName = (String) comboLabelName.getSelectedItem();
      Optional<QuantLabel> label = QuantLabel.LABELS.stream()
          .filter(ql -> ql.getName().equalsIgnoreCase(labelName)).findFirst();
      if (!label.isPresent()) {
        throw new IllegalStateException("Label from dropdown menu not present: " + labelName);
      }
      model.dataClear();
      for (String s : label.get().getReagentNames()) {
        QuantLabelAnnotation annotation = new QuantLabelAnnotation(s,
            String.format("%s_%s", expNameToFilePathRow.expName, s));
        model.dataAdd(annotation);
      }
    });
    comboLabelName = UiUtils.createUiCombo(QuantLabel.LABELS.stream().map(QuantLabel::getName)
        .collect(Collectors.toList()));
    Optional<QuantLabel> label = QuantLabel.LABELS.stream()
        .filter(ql -> ql.getName().equalsIgnoreCase(initLabelName)).findFirst();
    label.ifPresent(quantLabel -> comboLabelName.setSelectedItem(quantLabel.getName()));

    MigLayout layout = new MigLayout(new LC().fillX());//.debug());
    p.setLayout(layout);

    p.add(labelLoad, MigUtils.get().ccL().split());
    p.add(comboLabelName, MigUtils.get().ccL());
    p.add(buttonLoad, MigUtils.get().ccL().wrap());

    p.add(new JScrollPane(table), new CC().grow().spanX().wrap());

    p.add(buttonOK, new CC().tag("ok").split());
    p.add(buttonCancel, new CC().tag("cancel").wrap());




    setContentPane(scroll);
    setModal(true);
    setModalityType(ModalityType.APPLICATION_MODAL);
    setTitle("Annotate labeling experiment: '" + expNameToFilePathRow.expName + "'");
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
    dialogResult = JOptionPane.OK_OPTION;
    dispose();
  }

  private void onCancel() {
    // add your code here if necessary
    dialogResult = JOptionPane.CANCEL_OPTION;
    dispose();
  }

  public int getDialogResult() {
    return dialogResult;
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

  private void createTable() {
//    TableModel model = JTableUtils.createTableModelFromBean(QuantLabelAnnotation.class,
//        Arrays.asList(new QuantLabelAnnotation("126", "test-sample")));

    List<TableModelColumn<QuantLabelAnnotation, ?>> cols = new ArrayList<>();
    cols.add(new TableModelColumn<>("Labeling reagent", String.class, false, QuantLabelAnnotation::getLabel));
    cols.add(new TableModelColumn<>("Sample name", String.class, true, QuantLabelAnnotation::getSample));
    final int numRows = 20;
    model = new QuantLabelAnnotationModel(cols, numRows);
    for (int i = 0; i < numRows; i++) {
      model.dataAdd(new QuantLabelAnnotation("", ""));
    }
    table = new SimpleETable(model);
    table.setFullyEditable(true);
  }

  public static void main(String[] args) {
    Locale.setDefault(Locale.US);
    QuantLabelAnnotationDialog dialog = new QuantLabelAnnotationDialog(null,
        new ExpNameToAnnotationFile("test-experiment", Collections.emptyList(), "Not yet selected"),
        "TMT-10", Collections.emptyList());
    dialog.pack();
    dialog.setVisible(true);
    System.exit(0);
  }
}
