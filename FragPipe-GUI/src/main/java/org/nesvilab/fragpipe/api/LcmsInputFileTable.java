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

package org.nesvilab.fragpipe.api;

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
      "<html>Data types (DDA, DIA, DDA+, DIA-Quant, DIA-Lib, GPF-DIA).<br/>"
          + "<br>"
          + "<b>DDA+:</b> DDA files, full isolation window (chimeric spectrum) search.<br/>"
          + "This data type is compatible with DDA closed search workflows only<br/>"
          + "(e.g. Default, LFQ-MBR, LFQ-phospho, Nonspecific-HLA, etc).<br>"
          + "<b>DIA-Quant:</b> DIA files used for quantification only.<br/>"
          + "<b>DIA-Lib:</b> DIA files used for spectral library generation only<br/>"
          + "(e.g. GPF-DIA or DIA runs from \"boosting\" samples).<br/>"
          + "<br>"
          + "Analysis of diaPASEF data requires ddaPASEF data for building the spectral library.<br/>"
  };

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
