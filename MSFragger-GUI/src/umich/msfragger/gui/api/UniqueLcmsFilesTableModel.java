package umich.msfragger.gui.api;

import java.util.List;
import umich.msfragger.gui.InputLcmsFile;

public class UniqueLcmsFilesTableModel extends SimpleUniqueTableModel<InputLcmsFile> {

  public UniqueLcmsFilesTableModel(
      List<TableModelColumn<InputLcmsFile, ?>> cols, int initSize) {
    super(cols, initSize);
  }

  @Override
  public void setValueAt(Object aValue, int rowIndex, int columnIndex) {
    InputLcmsFile f = data.get(rowIndex);
    int i = data.indexOf(f);
    if (i < 0)
      throw new IllegalStateException("The object was not found in table model");
    dataSet(i, new InputLcmsFile(f.path, aValue.toString()));
  }
}
