package com.dmtavt.fragpipe.api;

import java.awt.event.MouseEvent;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableModel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class LcmsInputFileTable extends SimpleETable {
  private static final Logger log = LoggerFactory.getLogger(LcmsInputFileTable.class);
  protected final String[] columnToolTips = {
      "<html>Path to LC-MS file.<br/>"
          + "Supports drag & drop from system file explorer.", // "Path" assumed obvious
      "<html>Experiment, Group, or Condition.<br/>"
          + "<b>Can be left blank</b>, in which case all files are<br/>"
          + "assumed to be part of the same experiment.",
      "<html>Replicate number. <b>Integers only. Can be left blank.</b><br/>"
          + "If blank, files from the same experiment will be combined.<br/>"
          + "Leave it blank if it is fractionated data.",
      "<html>Data types (DDA, DIA, DIA-NW).<br/>"
          + "<b>DIA</b> is for wide window DIA.<br/>"
          + "<b>DIA-NW</b> is for narrow window DIA.<br/>"};

  public LcmsInputFileTable() {
    super();
  }

  public LcmsInputFileTable(TableModel dm) {
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
}
