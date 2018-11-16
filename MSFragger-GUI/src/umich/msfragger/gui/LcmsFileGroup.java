package umich.msfragger.gui;

import java.nio.file.Path;
import java.util.List;

public class LcmsFileGroup {
    public final String name;
    public final List<InputLcmsFile> lcmsFiles;

    public LcmsFileGroup(String name, List<InputLcmsFile> lcmsFiles) {
        this.name = name;
        this.lcmsFiles = lcmsFiles;
    }

    public Path outputDir(Path workDir) {
        return workDir.resolve(name);
    }
}
