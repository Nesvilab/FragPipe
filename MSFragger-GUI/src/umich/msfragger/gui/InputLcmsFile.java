package umich.msfragger.gui;

import java.nio.file.Path;

public class InputLcmsFile {
    public final Path path;
    public final String experiment;

    public InputLcmsFile(Path path, String experiment) {
        this.path = path;
        this.experiment = experiment;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }

        InputLcmsFile that = (InputLcmsFile) o;

        if (!path.equals(that.path)) {
            return false;
        }
        return experiment.equals(that.experiment);
    }

    @Override
    public int hashCode() {
        int result = path.hashCode();
        result = 31 * result + experiment.hashCode();
        return result;
    }

    @Override
    public String toString() {
        return String.format("InputLcmsFile{exp: '%s', path: '%s'}", experiment, path);
    }
}
