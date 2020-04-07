package com.dmtavt.fragpipe.api;

import com.dmtavt.fragpipe.exceptions.ValidationException;
import com.github.chhh.utils.swing.StringRepresentable;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;
import javax.swing.JTable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import umich.msfragger.gui.ModificationsTableModel;
import umich.msfragger.params.fragger.Mod;

public class ModsTable extends JTable implements StringRepresentable {
  private static final Logger log = LoggerFactory.getLogger(ModsTable.class);
  public static final String MOD_STRING_DELIMITER = "; ";
  public final ModificationsTableModel model;
  public final Object[] colNames;
  public final Function<List<Mod>, Object[][]> modsToData;

  public ModsTable(ModificationsTableModel model, Object[] colNames, Function<List<Mod>, Object[][]> modsToData) {
    this.model = model;
    this.colNames = colNames;
    this.modsToData = modsToData;
  }

  @Override
  public String asString() {
    return model.getModifications().stream()
        .map(Mod::asString).collect(Collectors.joining(MOD_STRING_DELIMITER));
  }

  @Override
  public void fromString(String s) {
    String[] split = s.split(MOD_STRING_DELIMITER);
    List<Mod> mods = new ArrayList<>();
    for (String chunk : split) {
      try {
        mods.add(Mod.fromString(chunk));
      } catch (ValidationException e) {
        log.warn("Could not deserialize MSFragger mod encoded as string: {}", s);
      }
    }
    Object[][] vec = modsToData.apply(mods);
    model.setRowCount(0);
    model.setRowCount(vec.length);
    model.setDataVector(vec, colNames);
  }
}
