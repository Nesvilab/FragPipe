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

import static org.nesvilab.fragpipe.cmd.ToolingUtils.BATMASS_IO_JAR;

import org.nesvilab.fragpipe.Fragpipe;
import org.nesvilab.fragpipe.FragpipeLocations;
import org.nesvilab.fragpipe.api.InputLcmsFile;
import java.awt.Component;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import javax.swing.JOptionPane;
import org.jooq.lambda.Seq;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CmdDiaTracer extends CmdBase {

  private static final Logger log = LoggerFactory.getLogger(CmdDiaTracer.class);
  public static String NAME = "diaTracer";


  public CmdDiaTracer(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  public boolean configure(Component component,
      int ramGb,
      int threads,
      Path extLibsBruker,
      Path binDiaTracer,
      boolean writeIntermediateFiles,
      float imTolerance,
      int apexScanDeltaRange,
      boolean massDefectFilter,
      float massDefectOffset,
      float ms1MS2Corr,
      int rfMax,
      List<InputLcmsFile> inputs) {
    initPreConfig();

    if (extLibsBruker == null) {
      if (Fragpipe.headless) {
        log.error(NAME + " requires native Bruker libraries. Native libraries come with MSFragger zip download, contained in ext sub-directory.");
      } else {
        JOptionPane.showMessageDialog(component,
            "<html>" + NAME + " requires native Bruker libraries.<br/>\n"
                + "Native libraries come with MSFragger zip download, contained in <i>ext</i><br/>\n"
                + "sub-directory. If you don't have an <i>ext</i> directory next to MSFragger.jar<br/>\n"
                + "please go to Config tab and Update MSFragger.",
            NAME + " error", JOptionPane.ERROR_MESSAGE);
      }
      return false;
    }

    for (InputLcmsFile f: inputs) {
      if (!(f.getDataType().contentEquals("DIA") || f.getDataType().contentEquals("GPF-DIA") || f.getDataType().contentEquals("DIA-Lib")) || !f.getPath().getFileName().toString().endsWith(".d")) {
        continue;
      }

      List<String> cmd = new ArrayList<>();
      cmd.add(Fragpipe.getBinJava());
      if (Fragpipe.headless) {
        cmd.add("-Djava.awt.headless=true");
      }
      cmd.add("-Xmx" + ramGb + "G");
      cmd.add(createJavaDParamString("libs.bruker.dir", extLibsBruker.toString()));
      cmd.add("-jar");
      cmd.add(binDiaTracer.toAbsolutePath().toString());
      Path licensePath = FragpipeLocations.locateLicense();
      if (licensePath != null) {
        cmd.add("--license");
        cmd.add(licensePath.toAbsolutePath().normalize().toString());
      }
      cmd.add("--dFilePath");
      cmd.add(f.getPath().toAbsolutePath().normalize().toString());
      cmd.add("--workDir");
      cmd.add(wd.toAbsolutePath().normalize().toString());
      cmd.add("--threadNum");
      cmd.add(String.valueOf(threads));
      cmd.add("--writeInter");
      cmd.add(writeIntermediateFiles ? "1" : "0");
      cmd.add("--deltaApexIM");
      cmd.add(String.valueOf(imTolerance));
      cmd.add("--deltaApexRT");
      cmd.add(String.valueOf(apexScanDeltaRange));
      cmd.add("--massDefectFilter");
      cmd.add(massDefectFilter ? "1" : "0");
      cmd.add("--massDefectOffset");
      cmd.add(String.valueOf(massDefectOffset));
      cmd.add("--ms1MS2Corr");
      cmd.add(String.valueOf(ms1MS2Corr));
      cmd.add("--RFMax");
      cmd.add(String.valueOf(rfMax));

      ProcessBuilder pb = new ProcessBuilder(cmd);
      pb.directory(wd.toFile());
      pbis.add(PbiBuilder.from(pb));
    }

    isConfigured = true;
    return true;
  }

  public List<InputLcmsFile> outputs(List<InputLcmsFile> inputs) {
    if (!isRun) {
      return new ArrayList<>(inputs);
    }

    List<InputLcmsFile> out = new ArrayList<>();
    for (InputLcmsFile f: inputs) {
      if (!(f.getDataType().contentEquals("DIA") || f.getDataType().contentEquals("GPF-DIA") || f.getDataType().contentEquals("DIA-Lib")) || !f.getPath().getFileName().toString().endsWith(".d")) {
        out.add(f);
      } else {
        String s = f.getPath().toAbsolutePath().normalize().toString().replace(".d", "_diatracer.mzML");
        out.add(new InputLcmsFile(Paths.get(s), f.getGroup(), f.getReplicate(), "DDA"));
      }
    }
    return out;
  }
}
