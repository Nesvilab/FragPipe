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

package org.nesvilab.fragpipe.tools.tmtintegrator;

import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import javax.swing.table.JTableHeader;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.nesvilab.fragpipe.api.InputLcmsFile;
import org.nesvilab.fragpipe.api.SimpleETable;
import org.nesvilab.fragpipe.api.SimpleUniqueTableModel;
import org.nesvilab.fragpipe.api.TableModelColumn;

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
          if (columnToolTips != null && realIndex >= 0 && realIndex < columnToolTips.length)
            return columnToolTips[realIndex];
        } catch (Exception ex) {
          log.error("Error getting tooltip", ex);
        }
        return null;
      }
    };
  }

  @SuppressWarnings("unchecked")
  public SimpleUniqueTableModel<ExpNameToAnnotationFile> fetchModel() {
    return (SimpleUniqueTableModel<ExpNameToAnnotationFile>) getModel();
  }

  private static SimpleUniqueTableModel<ExpNameToAnnotationFile> createTableModel() {
    List<TableModelColumn<ExpNameToAnnotationFile, ?>> cols = new ArrayList<>();

    TableModelColumn<ExpNameToAnnotationFile, String> colExp = new TableModelColumn<>(
        "Experiment",
        String.class, false, row -> row.expName);
    TableModelColumn<ExpNameToAnnotationFile, String> colPath = new TableModelColumn<>(
        "Annotation file path", String.class, true, row -> row.path);
    TableModelColumn<ExpNameToAnnotationFile, String> colBrowse = new TableModelColumn<>(
        "", String.class, true, row -> "Browse");
    TableModelColumn<ExpNameToAnnotationFile, String> colCreate = new TableModelColumn<>(
        "", String.class, true, row -> "Edit/Create");
    cols.add(colExp);
    cols.add(colPath);
    cols.add(colBrowse);
    cols.add(colCreate);

    SimpleUniqueTableModel<ExpNameToAnnotationFile> model = new SimpleUniqueTableModel<>(
        cols, 0);

    return model;
  }

  public static class ExpNameToAnnotationFile {
    public String expName;
    public Set<InputLcmsFile> lcmsFiles = new HashSet<>();
    public String path;

    public ExpNameToAnnotationFile() {
    }

    public ExpNameToAnnotationFile(String expName, Collection<? extends InputLcmsFile> lcmsFiles, String path) {
      this.expName = expName;
      if (lcmsFiles != null)
        this.lcmsFiles.addAll(lcmsFiles);
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
