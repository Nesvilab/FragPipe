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

package org.nesvilab.fragpipe.cmd;

import static org.nesvilab.utils.PathUtils.testFilePath;

import java.awt.Component;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;
import org.nesvilab.fragpipe.Fragpipe;
import org.nesvilab.fragpipe.FragpipeLocations;
import org.nesvilab.fragpipe.tools.fragger.Mod;
import org.nesvilab.fragpipe.tools.fragger.MsfraggerParams;
import org.nesvilab.utils.StringUtils;
import org.nesvilab.utils.SwingUtils;
import org.nesvilab.utils.UsageTrigger;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CmdMsfraggerDigest extends CmdBase {

  private static final Logger log = LoggerFactory.getLogger(CmdMsfraggerDigest.class);
  private static final Pattern pattern = Pattern.compile("[A-Znc\\[\\]\\^\\*]+");
  public static final String NAME = "MSFragger Digest";

  public CmdMsfraggerDigest(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  public boolean configure(Component comp,
      boolean isDryRun,
      Path jarFragpipe,
      UsageTrigger binFragger,
      String pathFasta,
      MsfraggerParams params,
      int ramGb,
      final String decoyTag,
      boolean excludeDecoys) {

    initPreConfig();

    for (Mod mod : params.getVariableMods()) {
      if (mod.isEnabled && !pattern.matcher(mod.sites).matches()) {
        SwingUtils.showErrorDialog(null, "Invalid variable modification sites: " + mod.sites + ".\nPlease check the tooltip or documentations.", "Variable modification sites error");
        return false;
      }
    }

    if (StringUtils.isNullOrWhitespace(binFragger.getBin())) {
      SwingUtils.showErrorDialog(comp, "Binary for running MSFragger can not be an empty string.\n", "Error");
      return false;
    }

    if (testFilePath(binFragger.getBin(), "") == null) {
      SwingUtils.showErrorDialog(comp, "Binary for running MSFragger not found or could not be run.\nNeither on PATH, nor in the working directory", "Error");
      return false;
    }

    if (pathFasta == null) {
      SwingUtils.showErrorDialog(comp, "FASTA file path can't be empty", "Error");
      return false;
    }

    params.setDatabaseName(pathFasta);
    params.setDecoyPrefix(decoyTag);
    params.setExcludeDecoys(excludeDecoys);
    Path savedParamsPath = wd.resolve("fragger_digest.params");

    if (!isDryRun) {
      try {
        params.save(Files.newOutputStream(savedParamsPath));
      } catch (IOException ex) {
        SwingUtils.showErrorDialog(comp, "Could not save fragger_*.params file to working dir.\n", "Error");
        return false;
      }
    }

    List<String> cmd = new ArrayList<>();
    cmd.add(Fragpipe.getBinJava());
    if (Fragpipe.headless) {
      cmd.add("-Djava.awt.headless=true");
    }
    cmd.add("-Xmx" + ramGb + "G");
    cmd.add("-jar");
    cmd.add(binFragger.useBin());
    cmd.add(savedParamsPath.toString());

    Path licensePath = FragpipeLocations.locateLicense();
    if (licensePath != null) {
      cmd.add("--license");
      cmd.add(licensePath.toAbsolutePath().normalize().toString());
    }

    ProcessBuilder pb = new ProcessBuilder(cmd);
    pb.directory(wd.toFile());
    pbis.add(PbiBuilder.from(pb));

    isConfigured = true;
    return true;
  }
}
 