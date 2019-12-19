package umich.msfragger.cmd;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import umich.msfragger.util.OsUtils;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class CmdBrukerLibLoadTest extends CmdBase {
    private static final Logger log = LoggerFactory.getLogger(CmdBrukerLibLoadTest.class);

    public static final String JAR_NAME = "batmass-io-consumer.jazz";
    public static final String JAR_MAIN_CLASS = "com.dmtavt.batmass.io.consumer.App";
    private static String[] JAR_DEPS = {ToolingUtils.BATMASS_IO_JAZZ, "imquant-1.6.5.jazz"};

    public CmdBrukerLibLoadTest(boolean isRun, Path workDir, String fileCaptureStdout, String fileCaptureStderr) {
        super(isRun, workDir, fileCaptureStdout, fileCaptureStderr);
    }

    public CmdBrukerLibLoadTest(boolean isRun, Path workDir) {
        super(isRun, workDir);
    }

    public boolean configure(Path binFragger) {
        final Path extLibsBruker = CmdMsfragger.searchExtLibsBruker(Collections.singletonList(binFragger.getParent()));
        List<String> jars = Stream.concat(Arrays.stream(JAR_DEPS), Stream.of(JAR_NAME)).collect(Collectors.toList());
        final List<Path> unpacked = new ArrayList<>();
        if (!unpackJars(jars, unpacked, getCmdName())) {
            return false;
        }
        List<String> cmd = new ArrayList<>();
        cmd.add("java");
        if (extLibsBruker != null) {
            cmd.add(createJavaDParamString("bruker.lib.path", extLibsBruker.toString()));
        } else {
            log.warn("extLibsBruker was null");
        }
        cmd.add("-cp");
        cmd.add(constructClasspathString(unpacked));
        cmd.add(JAR_MAIN_CLASS);
        log.info("Constructed cmd: {}", cmd);

        ProcessBuilder pb = new ProcessBuilder(cmd);
        pbis.add(PbiBuilder.from(pb));
        this.isConfigured = true;

        return true;
    }

    @Override
    public String getCmdName() {
        return this.getClass().getSimpleName();
    }
}
