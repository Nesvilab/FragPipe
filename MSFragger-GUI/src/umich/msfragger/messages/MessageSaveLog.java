package umich.msfragger.messages;

import java.nio.file.Path;

public class MessageSaveLog {
  public final Path workDir;

  public MessageSaveLog(Path workDir) {
    this.workDir = workDir;
  }
}
