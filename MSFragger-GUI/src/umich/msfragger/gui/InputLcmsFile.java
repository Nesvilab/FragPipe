package umich.msfragger.gui;

import java.nio.file.Path;

public class InputLcmsFile {
    public final Path path;
    public final String experiment;

    public InputLcmsFile(Path path, String experiment) {
        this.path = path;
        this.experiment = experiment;
    }
}
