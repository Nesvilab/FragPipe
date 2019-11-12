package umich.msfragger.cmd;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class CmdBrukerLibLoadTest extends CmdBase {
    private static final Logger log = LoggerFactory.getLogger(CmdBrukerLibLoadTest.class);

    public static final String JAR_IMQUANT_NAME = "imquant-1.6.5.jazz";
    public static final String JAR_MSFTBX_NAME = "batmass-io-1.17.1.jazz";
    public static final String JAR_IMQUANT_MAIN_CLASS = "imquant.IMQuant";
    private static String[] JAR_DEPS = {JAR_MSFTBX_NAME};

    public CmdBrukerLibLoadTest(boolean isRun, Path workDir, String fileCaptureStdout, String fileCaptureStderr) {
        super(isRun, workDir, fileCaptureStdout, fileCaptureStderr);
    }

    public CmdBrukerLibLoadTest(boolean isRun, Path workDir) {
        super(isRun, workDir);
    }

    public boolean configure(Path binFragger) {
        final Path extLibsBruker = CmdMsfragger.searchExtLibsBruker(Collections.singletonList(binFragger.getParent()));
        List<String> jars = Stream.concat(Arrays.stream(JAR_DEPS), Stream.of(JAR_IMQUANT_NAME)).collect(Collectors.toList());
        final List<Path> unpacked = new ArrayList<>();
        if (!unpackJars(jars, unpacked, getCmdName())) {
            return false;
        }
        List<String> cmd = new ArrayList<>();
        cmd.add("java");
        if (extLibsBruker != null) {
            cmd.add("-Dbruker.lib.path=\"" + extLibsBruker.toString() + "\"" );
        } else {
            log.warn("extLibsBruker was null");
        }
        cmd.add("-cp");
        cmd.add(constructClasspathString(unpacked));
        log.info("Constructed cmd: {}", cmd);

        return true;
    }

    @Override
    public String getCmdName() {
        return this.getClass().getSimpleName();
    }
}
