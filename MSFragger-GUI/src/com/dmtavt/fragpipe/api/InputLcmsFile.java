package com.dmtavt.fragpipe.api;

import com.dmtavt.fragpipe.params.ThisAppProps;
import com.github.chhh.utils.StringUtils;
import java.nio.file.Path;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Set;
import java.util.regex.Pattern;
import org.jetbrains.annotations.NotNull;

public class InputLcmsFile implements Comparable<InputLcmsFile> {
    private final Path path;
    private final String experiment;
    private final Integer replicate;

    public static final String REASON_NON_ASCII = "has non-ASCII chars";
    public static final String REASON_DOTS = "has dots";
    public static final String REASON_MULTIPLE_DOTS = "has multiple dots";
    public static final String REASON_SPACES = "has spaces";
    public static final String REASON_UNSUPPORTED = "not supported";
    public static final String allowedChars = "[A-Za-z0-9-_+.\\[\\]()]";
    public static final String disallowedChars = "[^A-Za-z0-9-_ +.\\[\\]()]";
    public static final Pattern disallowedExperimentPattern = Pattern.compile("[^A-Za-z0-9-_]");
    public static final String REASON_DISALLOWED_CHARS = "has characters other than: " + allowedChars;

    public InputLcmsFile(Path path, String experiment) {
        this(path, experiment, null);
    }

    public InputLcmsFile(Path path, String experiment, Integer replicate) {
        this.path = path;
        experiment = experiment != null ? experiment.trim() : ThisAppProps.DEFAULT_LCMS_EXP_NAME;
        this.experiment = disallowedExperimentPattern.matcher(experiment).replaceAll("_");
        this.replicate = replicate;
    }

    public Path outputDir(Path workDir) {
        return workDir.resolve(getGroup());
    }

    @Override
    public boolean equals(Object o) {
        if (o instanceof InputLcmsFile) {
            return compareTo((InputLcmsFile) o) == 0;
        } else {
            return false;
        }
    }

    public int compareTo(@NotNull InputLcmsFile other) { // the sorting here must be consistent with the one in LcmsFileGroup
        Comparator<InputLcmsFile> comparator = Comparator.comparing(InputLcmsFile::getGroup).thenComparing(p -> p.path.toAbsolutePath().toString());
        return comparator.compare(this, other);
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

    private static<T> void addNonNull(Collection<? super T> collection, T value) {
        if (value != null) {
            collection.add(value);
        }
    }

    public static Set<String> validatePath(String dir) {
//        if (p.getFileName().toString().contains("2file space")) {
//            int a = 1;
//        }
        Set<String> reasons = new HashSet<>();
        addNonNull(reasons, testIsNotAscii(dir));
        addNonNull(reasons, testHasSpaces(dir));
        return reasons;
    }

    public static Set<String> validateFilename(String fn) {
        Set<String> reasons = new HashSet<>();
        //addNonNull(reasons, testIsNotAscii(fn));
        addNonNull(reasons, testHasSpaces(fn));
        addNonNull(reasons, testHasMoreThanOneDot(fn));
        addNonNull(reasons, testHasNonAllowedChars(fn));
        return reasons;
    }

    private static String testIsNotAscii(String s) {
        return (s != null && !com.github.chhh.utils.StringUtils.isPureAscii(s)) ? REASON_NON_ASCII : null;
    }

    private static String testHasSpaces(String s) {
        return (s != null && s.contains(" ")) ? REASON_SPACES : null;
    }

    private static String testHasDots(String s) {
        return (s != null && s.contains(".")) ? REASON_DOTS : null;
    }

    private static String testHasMoreThanOneDot(String s) {
        return (s != null && s.chars().filter(c -> c == '.').count() > 1) ? REASON_MULTIPLE_DOTS : null;
    }

    private static String testHasNonAllowedChars(String s) {
        if (s == null) {
            return null;
        }
        return Pattern.compile(disallowedChars).matcher(s).find() ? REASON_DISALLOWED_CHARS : null;
    }

    public static Path renameBadFile(Path p) {
        String oldFn = p.getFileName().toString();
        final String replacement = "_";
        String newFn = oldFn.replaceAll(" ", replacement);
        if (testHasDots(newFn) != null) {
            newFn = StringUtils.upToLastDot(newFn).replaceAll("\\.", replacement) + "." + StringUtils.afterLastDot(newFn);
        }
        newFn = newFn.replaceAll(disallowedChars, replacement);
        return p.resolveSibling(newFn);
    }
}
