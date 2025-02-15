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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with FragPipe. If not, see <https://www.gnu.org/licenses/>.
 */

package org.nesvilab.fragpipe.api;

import org.nesvilab.fragpipe.exceptions.UnexpectedException;
import org.nesvilab.fragpipe.exceptions.ValidationException;
import org.nesvilab.utils.ProcessUtils;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.slf4j.LoggerFactory;
import java.util.StringJoiner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class DotnetInfo {

    public static final String COMMAND = "dotnet";
    private static final org.slf4j.Logger log = LoggerFactory.getLogger(DotnetInfo.class);
    private String command;
    private DefaultArtifactVersion version;
    private final static Pattern pattern = Pattern.compile("6\\.[0-9.]+", Pattern.CASE_INSENSITIVE);
    private static DotnetInfo dotnetInfo = null;

    @Override
    public String toString() {
        return new StringJoiner(", ", DotnetInfo.class.getSimpleName() + "[", "]")
                .add("command='" + command + "'")
                .add("version=" + version)
                .toString();
    }

    private DotnetInfo() {

    }

    public static DotnetInfo fromCommand(String command) throws ValidationException, UnexpectedException {
        if (dotnetInfo == null || dotnetInfo.version == null || dotnetInfo.version.toString().isEmpty()) {
            dotnetInfo = new DotnetInfo();
            dotnetInfo.trySetDotNetCommand(command);
        }
        return dotnetInfo;
    }

    /** @param command The command to start dotnet interpreter. */
    private void trySetDotNetCommand(String command) throws ValidationException, UnexpectedException {
        this.command = command;
        this.version = tryGetVersion(command);
    }

    public String getCommand() {
        return command;
    }

    public DefaultArtifactVersion getVersion() {
        return version;
    }

    private static DefaultArtifactVersion tryGetVersion(String cmd) throws UnexpectedException, ValidationException {
        ProcessBuilder pb = new ProcessBuilder(cmd, "--info");
        pb.redirectErrorStream(true);

        String printed;
        log.debug("Trying to check dotnet version: {}", String.join(" ", pb.command()));
        printed = ProcessUtils.captureOutput(pb);
        log.debug("Dotnet version command printed: {}", printed);

        Matcher m = pattern.matcher(printed);
        if (m.find()) {
            String version = m.group();
            log.debug("Found dotnet version string in output: {}", version);
            return new DefaultArtifactVersion(version);
        } else {
            log.debug("Did not find dotnet version string in output");
            throw new ValidationException("Could not detect dotnet version");
        }
    }

}
