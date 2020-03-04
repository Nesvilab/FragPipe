package umich.msfragger.params.tmtintegrator;

import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.List;
import javax.swing.table.JTableHeader;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import umich.msfragger.gui.api.SimpleETable;
import umich.msfragger.gui.api.SimpleUniqueTableModel;
import umich.msfragger.gui.api.TableModelColumn;

public class TmtAnnotationTable extends SimpleETable {
  private static final Logger log = LoggerFactory.getLogger(TmtAnnotationTable.class);
  protected final String[] columnToolTips = {
      "<html>Experiment.<br/>"
          + "Name of the experiment in LCMS files selection tab.", // "Path" assumed obvious
      "<html>Annotation.<br/>\n"
          + "A mapping from label type to sample name. Sample name can "
          + "be any string of your choosing.",
      "Browse filesystem for an existing file."
  };

  public TmtAnnotationTable() {
    super(createTableModel());
    setFullyEditable(false);
  }
//
//  @Override
//  public boolean isCellEditable(int row, int column) {
//    int modelIndex = convertColumnIndexToModel(column);
//    return modelIndex == 1;
//  }

  @Override
  protected JTableHeader createDefaultTableHeader() {
    return new JTableHeader(columnModel) {
      public String getToolTipText(MouseEvent e) {
        try {
          java.awt.Point p = e.getPoint();
          int index = columnModel.getColumnIndexAtX(p.x);
          int realIndex = columnModel.getColumn(index).getModelIndex();
          return columnToolTips[realIndex];
        } catch (Exception ex) {
          log.error("Error getting tooltip", ex);
        }
        return null;
      }
    };
  }

  @SuppressWarnings("unchecked")
  public SimpleUniqueTableModel<TmtTableRow> fetchModel() {
    return (SimpleUniqueTableModel<TmtTableRow>) getModel();
  }

  private static SimpleUniqueTableModel<TmtTableRow> createTableModel() {
    List<TableModelColumn<TmtTableRow, ?>> cols = new ArrayList<>();

    TableModelColumn<TmtTableRow, String> colExp = new TableModelColumn<>(
        "Experiment",
        String.class, false, row -> row.expName);
    TableModelColumn<TmtTableRow, String> colPath = new TableModelColumn<>(
        "Annotation file path", String.class, true, row -> row.path);
    TableModelColumn<TmtTableRow, String> colBrowse = new TableModelColumn<>(
        "", String.class, true, row -> "Browse");
    TableModelColumn<TmtTableRow, String> colCreate = new TableModelColumn<>(
        "", String.class, true, row -> "Create/Edit");
    cols.add(colExp);
    cols.add(colPath);
    cols.add(colBrowse);
    cols.add(colCreate);

    SimpleUniqueTableModel<TmtTableRow> model = new SimpleUniqueTableModel<>(
        cols, 0);

    return model;
  }

  public static class TmtTableRow {
    public String expName;
    public String path;

    public TmtTableRow() {
    }

    public TmtTableRow(String expName, String path) {
      this.expName = expName;
      this.path = path;
    }

    public String getExpName() {
      return expName;
    }

    public void setExpName(String expName) {
      this.expName = expName;
    }

    public String getPath() {
      return path;
    }

    public void setPath(String path) {
      this.path = path;
    }
  }
}
