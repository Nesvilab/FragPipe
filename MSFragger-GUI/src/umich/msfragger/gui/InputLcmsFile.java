package umich.msfragger.gui;

import java.nio.file.Path;
import umich.msfragger.util.StringUtils;

public class InputLcmsFile {
    private final Path path;
    private final String experiment;
    private final Integer replicate;

    public InputLcmsFile(Path path, String experiment) {
        this(path, experiment, null);
    }

    public InputLcmsFile(Path path, String experiment, Integer replicate) {
        this.path = path;
        this.experiment = experiment;
        this.replicate = replicate;
    }

    public Path outputDir(Path workDir) {
        return workDir.resolve(getGroup());
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

        if (!getPath().equals(that.getPath())) {
            return false;
        }
        return getGroup().equals(that.getGroup());
    }

    @Override
    public int hashCode() {
        int result = getPath().hashCode();
        result = 31 * result + getGroup().hashCode();
        return result;
    }

    @Override
    public String toString() {
        return String.format("InputLcmsFile{exp: '%s', path: '%s'}", getGroup(), getPath());
    }

    public String getGroup() {
        if (getReplicate() != null) {
            if (StringUtils.isNullOrWhitespace(experiment)) {
                return "exp_" + getReplicate();
            } else {
                return experiment + "_" + getReplicate();
            }
        }
        return experiment;
    }

    public String getExperiment() {
        return experiment;
    }

    public Integer getReplicate() {
        return replicate;
    }

    public Path getPath() {
        return path;
    }
}
