package com.dmtavt.fragpipe.cmd;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.FragpipeLocations;
import com.dmtavt.fragpipe.tabs.TabDownstream;
import com.dmtavt.fragpipe.tools.fpop.FPOP_DiannReport_Editor;
import com.dmtavt.fragpipe.tools.fpop.FPOP_PSMwriter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.awt.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

public class CmdFPOPcoadaptr extends CmdBase {
    private static final Logger log = LoggerFactory.getLogger(CmdFPOPcoadaptr.class);
    public static final String NAME = "FPOP-adapter";

    public CmdFPOPcoadaptr(boolean isRun, Path workDir) {
        super(isRun, workDir);
    }

    @Override
    public String getCmdName() {
        return NAME;
    }

    public boolean configure(boolean isDIA, Path jarFragpipe) {

        initPreConfig();
        final TabDownstream tabDownstream = Fragpipe.getStickyStrict(TabDownstream.class);
        List<String> cmd = new ArrayList<>();

        cmd.add(Fragpipe.getBinJava());
        cmd.add("-cp");
        Path root = FragpipeLocations.get().getDirFragpipeRoot();
        String libsDir = root.resolve("lib").toString() + "/*";
        if (Files.isDirectory(jarFragpipe)) {
            libsDir = jarFragpipe.toAbsolutePath().getParent().getParent().getParent().getParent().resolve("build/install/fragpipe/lib").toString() + "/*";
            log.debug("Dev message: Looks like FragPipe was run from IDE, changing libs directory to: {}", libsDir);
        }
        cmd.add(libsDir);
        if (isDIA) {
            cmd.add(FPOP_DiannReport_Editor.class.getCanonicalName());
        } else {
            cmd.add(FPOP_PSMwriter.class.getCanonicalName());
        }

        cmd.add(wd.toAbsolutePath().toString());
        cmd.add(tabDownstream.pFpopCoadaptr.getFpopMods());

        ProcessBuilder pb = new ProcessBuilder(cmd);
        pb.directory(wd.toFile());

        pbis.add(PbiBuilder.from(pb));
        isConfigured = true;
        return true;
    }

}
