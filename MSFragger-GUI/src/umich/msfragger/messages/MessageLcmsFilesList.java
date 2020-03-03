package umich.msfragger.messages;

import java.util.List;
import umich.msfragger.gui.InputLcmsFile;

public class MessageLcmsFilesList {
  public final MessageType type;
  public final List<InputLcmsFile> files;

  public MessageLcmsFilesList(MessageType type, List<InputLcmsFile> files) {
    this.type = type;
    this.files = files;
  }
}
