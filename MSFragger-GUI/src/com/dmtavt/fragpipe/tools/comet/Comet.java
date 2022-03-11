package com.dmtavt.fragpipe.tools.comet;

import com.dmtavt.fragpipe.api.PyInfo;
import com.dmtavt.fragpipe.exceptions.UnexpectedException;
import com.dmtavt.fragpipe.exceptions.ValidationException;
import com.github.chhh.utils.ProcessUtils;
import org.slf4j.LoggerFactory;

import java.nio.file.Path;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Comet {
    private static final org.slf4j.Logger log = LoggerFactory.getLogger(PyInfo.class);

    public static class CometVer {
        public final boolean isValid;
        public final String version;

        public CometVer(boolean isValid, String version) {
            this.isValid = isValid;
            this.version = version;
        }
    }


    public static CometVer readVersion(Path bin) throws UnexpectedException, ValidationException {
        final String v = tryGetVersion(bin);
        return new CometVer(true, v);
    }

    private static String tryGetVersion(Path bin) throws UnexpectedException, ValidationException {
        ProcessBuilder pb = new ProcessBuilder(bin.toString());
        pb.redirectErrorStream(true);

        String printed;
        log.debug("Trying to check Comet version: {}", String.join(" ", pb.command()));
        printed = ProcessUtils.captureOutput(pb);
        log.debug("Comet version command printed: {}", printed);

        Matcher m = Pattern.compile("Comet version\\s+\"([^\"]+?)\"", Pattern.CASE_INSENSITIVE).matcher(printed);
        if (m.find()) {
            String version = m.group(1);
            log.debug("Found Comet version string in output: {}", version);
            return version;
        }
        log.debug("Did not find Comet version string in output");
        throw new ValidationException("Could not detect Comet version");
    }
}
