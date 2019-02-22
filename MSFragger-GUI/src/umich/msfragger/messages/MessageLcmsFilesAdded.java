package umich.msfragger.messages;

import java.nio.file.Path;
import java.util.List;

public class MessageLcmsFilesAdded {

  public MessageLcmsFilesAdded(List<Path> paths) {
    this.paths = paths;
  }

  public final List<Path> paths;
}
