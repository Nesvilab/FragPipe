package umich.msfragger.messages;

import java.util.ArrayList;
import java.util.List;
import umich.msfragger.gui.InputLcmsFile;

public class MessageLcmsFilesTableUpdated {
  public final List<InputLcmsFile> files;
  public MessageLcmsFilesTableUpdated(List<InputLcmsFile> files) {
    this.files = files;
  }
}
