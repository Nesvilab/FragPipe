/*
 * This file is part of FragPipe.
 *
 * FragPipe is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * FragPipe is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with FragPipe.  If not, see <https://www.gnu.org/licenses/>.
 */

package com.dmtavt.fragpipe.api;

import com.dmtavt.fragpipe.exceptions.UnexpectedException;
import com.dmtavt.fragpipe.exceptions.ValidationException;
import com.github.chhh.utils.ProcessUtils;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.slf4j.LoggerFactory;
import java.util.StringJoiner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class DotnetInfo {
    public static final String COMMAND = "dotnet";
    private static final org.slf4j.Logger log = LoggerFactory.getLogger(DotnetInfo.class);
    private String command;
    private String version;
    private DefaultArtifactVersion fullVersion;
    private final static String regex_whole_string = "(NETCore\\.App 6\\.[0-9.]+)";
    private final static String regex_version_only = "NETCore\\.App (6\\.[0-9.]+)";

    @Override
    public String toString() {
        return new StringJoiner(", ", DotnetInfo.class.getSimpleName() + "[", "]")
                .add("command='" + command + "'")
                .add("version='" + version + "'")
                .add("fullVersion=" + fullVersion)
                .toString();
    }

    private DotnetInfo() {

    }

    public static DotnetInfo fromCommand(String command) throws ValidationException, UnexpectedException {
        DotnetInfo di = new DotnetInfo();
        di.trySetDotNetCommand(command);
        return di;
    }

    /** @param command The command to start dotnet interpreter. */
    private void trySetDotNetCommand(String command) throws ValidationException, UnexpectedException {
        this.command = command;
        this.version = tryGetVersion(command);

        Matcher m = Pattern.compile(regex_version_only, Pattern.CASE_INSENSITIVE).matcher(this.version);
        if (m.find()) {
            this.fullVersion = new DefaultArtifactVersion(m.group(1).trim());
        } else {
            throw new ValidationException("Could not detect dotnet version.");
        }
    }

    public String getCommand() {
        return command;
    }

    public String getVersion() {
        return version;
    }

    public DefaultArtifactVersion getFullVersion() {
        return fullVersion;
    }

    private static String tryGetVersion(String cmd) throws UnexpectedException, ValidationException {
        ProcessBuilder pb = new ProcessBuilder(cmd, "--info");
        pb.redirectErrorStream(true);

        String printed;
        log.debug("Trying to check dotnet version: {}", String.join(" ", pb.command()));
        printed = ProcessUtils.captureOutput(pb);
        log.debug("Dotnet version command printed: {}", printed);

        Matcher m = Pattern.compile(regex_whole_string, Pattern.CASE_INSENSITIVE).matcher(printed);
        if (m.find()) {
            String version = m.group(1);
            log.debug("Found dotnet version string in output: {}", version);
            return version;
        } else {
            log.debug("Did not find dotnet version string in output");
            throw new ValidationException("Could not detect dotnet version");
        }
    }

}
