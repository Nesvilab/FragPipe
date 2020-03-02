package umich.msfragger.params.tmtintegrator;

import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.List;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableModel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import umich.msfragger.gui.api.SimpleETable;
import umich.msfragger.gui.api.SimpleUniqueTableModel;
import umich.msfragger.gui.api.TableModelColumn;

public class TmtAnnotationTable extends SimpleETable {
  private static final Logger log = LoggerFactory.getLogger(umich.msfragger.gui.LcmsInputFileTable.class);
  protected final String[] columnToolTips = {
      "<html>Experiment.<br/>"
          + "Name of the experiment in LCMS files selection tab.", // "Path" assumed obvious
      "<html>Annotation.<br/>\n"
          + "A mapping from label type to sample name. Sample name can "
          + "be any string of your choosing."};

  public TmtAnnotationTable() {
    super(createTableModel());
  }

  public TmtAnnotationTable(TableModel dm) {
    super(dm);
  }

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

  private static SimpleUniqueTableModel<TmtAnnotationRow> createTableModel() {
    List<TableModelColumn<TmtAnnotationRow, ?>> cols = new ArrayList<>();

    TableModelColumn<TmtAnnotationRow, String> colExp = new TableModelColumn<>(
        "Experiment",
        String.class, false, row -> row.expName);
    TableModelColumn<TmtAnnotationRow, String> colPath = new TableModelColumn<>(
        "Annotation file path", String.class, true, row -> row.path);

    cols.add(colExp);
    cols.add(colPath);
    return new SimpleUniqueTableModel<TmtAnnotationRow>(cols, 3);
  }

  public static class TmtAnnotationRow {
    public String expName;
    public String path;
  }
}
