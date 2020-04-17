package com.dmtavt.fragpipe.api;

import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class UniqueLcmsFilesTableModel extends SimpleUniqueTableModel<InputLcmsFile> {
  private static final Logger log = LoggerFactory.getLogger(UniqueLcmsFilesTableModel.class);

  public UniqueLcmsFilesTableModel(
      List<TableModelColumn<InputLcmsFile, ?>> cols, int initSize) {
    super(cols, initSize);
  }

  @Override
  public void setValueAt(Object aValue, int rowIndex, int columnIndex) {
    InputLcmsFile orig = data.get(rowIndex);
    int i = data.indexOf(orig);
    if (i < 0) {
      log.error("The object was not found in table model");
      throw new IllegalStateException("The object was not found in table model");
    }
    String exp = columnIndex == 1 ? (String)aValue : orig.getExperiment();
    Integer rep = columnIndex == 2 ? (Integer)aValue : orig.getReplicate();
    InputLcmsFile fnew = new InputLcmsFile(orig.getPath(), exp, rep);
    dataSet(i, fnew);
  }
}
