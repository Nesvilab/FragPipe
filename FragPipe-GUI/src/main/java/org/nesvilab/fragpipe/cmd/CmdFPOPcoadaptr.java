package org.nesvilab.fragpipe.cmd;

import org.nesvilab.fragpipe.Fragpipe;
import org.nesvilab.fragpipe.FragpipeLocations;
import org.nesvilab.fragpipe.tabs.TabDownstream;
import org.nesvilab.fragpipe.tools.fpop.FPOP_DiannReport_Editor;
import org.nesvilab.fragpipe.tools.fpop.FPOP_PSMwriter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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
        String libsDir = root.resolve("lib").toAbsolutePath().normalize() + "/*";
        if (Files.isDirectory(jarFragpipe)) {
            libsDir = jarFragpipe.toAbsolutePath().getParent().getParent().getParent().getParent().resolve("build/install/fragpipe/lib").toAbsolutePath().normalize() + "/*";
        }
        cmd.add(libsDir);
        if (isDIA) {
            cmd.add(FPOP_DiannReport_Editor.class.getCanonicalName());
        } else {
            cmd.add(FPOP_PSMwriter.class.getCanonicalName());
        }

        cmd.add(wd.toAbsolutePath().normalize().toString());
        cmd.add(tabDownstream.pFpopCoadaptr.getFpopMods());

        ProcessBuilder pb = new ProcessBuilder(cmd);
        pb.directory(wd.toFile());

        pbis.add(PbiBuilder.from(pb));
        isConfigured = true;
        return true;
    }

}
