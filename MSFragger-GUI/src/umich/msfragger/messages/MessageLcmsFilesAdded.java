package umich.msfragger.messages;

import java.nio.file.Path;
import java.util.List;

public class MessageLcmsFilesAdded {

  public MessageLcmsFilesAdded(List<Path> paths, Path recursiveAdditionRoot) {
    this.paths = paths;
    this.recursiveAdditionRoot = recursiveAdditionRoot;
  }

  public MessageLcmsFilesAdded(List<Path> paths) {
    this(paths, null);
  }

  public final List<Path> paths;
  public final Path recursiveAdditionRoot;
}
