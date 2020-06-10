package com.dmtavt.fragpipe.cmd;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.FragpipeLocations;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import org.jooq.lambda.Seq;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CmdBrukerLibLoadTest extends CmdBase {
    private static final Logger log = LoggerFactory.getLogger(CmdBrukerLibLoadTest.class);

    public static final String JAR_NAME = "batmass-consumer.jar";
    public static final String JAR_MAIN_CLASS = "com.dmtavt.batmass.io.consumer.App";
    private static final String[] JAR_DEPS = {ToolingUtils.BATMASS_IO_JAR, "ionquant-1.3.1.jar"};

    public CmdBrukerLibLoadTest(boolean isRun, String title, Path workDir,
        String fileCaptureStdout, String fileCaptureStderr) {
        super(isRun, title, workDir, fileCaptureStdout, fileCaptureStderr);
    }

    public CmdBrukerLibLoadTest(boolean isRun, Path workDir) {
        super(isRun, workDir);
    }

    public boolean configure(Path binFragger) {
        initPreConfig();

        final Path extLibsBruker = CmdMsfragger.searchExtLibsBruker(Collections.singletonList(binFragger.getParent()));
        final List<Path> classpathJars = FragpipeLocations.checkToolsMissing(Seq.of(JAR_NAME).concat(JAR_DEPS));
        if (classpathJars == null) {
            return false;
        }

        List<String> cmd = new ArrayList<>();
        cmd.add(Fragpipe.getBinJava());
        if (extLibsBruker != null) {
            cmd.add(createJavaDParamString("bruker.lib.path", extLibsBruker.toString()));
        } else {
            log.warn("extLibsBruker was null");
        }
        cmd.add("-cp");
        cmd.add(constructClasspathString(classpathJars));
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
