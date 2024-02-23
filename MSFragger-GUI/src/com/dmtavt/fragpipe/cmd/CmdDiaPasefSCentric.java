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

package com.dmtavt.fragpipe.cmd;

import static com.dmtavt.fragpipe.cmd.ToolingUtils.BATMASS_IO_JAR;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.FragpipeLocations;
import com.dmtavt.fragpipe.api.InputLcmsFile;
import java.awt.Component;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import javax.swing.JOptionPane;
import org.jooq.lambda.Seq;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CmdDiaPasefSCentric extends CmdBase {

  private static final Logger log = LoggerFactory.getLogger(CmdDiaPasefSCentric.class);
  public static final String JAR_NAME = "diaPASEFScentric-1.0.4.jar";
  public static String NAME = "diaPASEFSCentric";
  private static final String[] JAR_DEPS = {BATMASS_IO_JAR};


  public CmdDiaPasefSCentric(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  public boolean configure(Component component, int ramGb, int threads, Path binFragger, boolean writeIntermediateFiles, float imTolerance, int mzToleranceUnit, float mzTolerance, int apexScanDeltaRange, float imToleranceMs1Ms2, int apexScanDeltaRangeMs1Ms2, List<InputLcmsFile> inputs) {
    initPreConfig();

    final List<Path> classpathJars = FragpipeLocations.checkToolsMissing(Seq.of(JAR_NAME).concat(JAR_DEPS));
    if (classpathJars == null) {
      return false;
    }

    final Path extLibsBruker = CmdMsfragger.searchExtLibsBruker(Collections.singletonList(binFragger.getParent()));
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
      if (!f.getDataType().contentEquals("DIA") || !f.getPath().getFileName().toString().endsWith(".d")) {
        continue;
      }

      List<String> cmd = new ArrayList<>();
      cmd.add(Fragpipe.getBinJava());
      if (Fragpipe.headless) {
        cmd.add("-Djava.awt.headless=true");
      }
      cmd.add("-Xmx" + ramGb + "G");
      cmd.add(createJavaDParamString("libs.bruker.dir", extLibsBruker.toString()));
      cmd.add("-cp");
      cmd.add(constructClasspathString(classpathJars));
      cmd.add("DIAPASEFScentricMainClass");
      cmd.add(f.getPath().toAbsolutePath().toString());
      cmd.add(wd.toAbsolutePath().toString());
      cmd.add(String.valueOf(threads));
      cmd.add(writeIntermediateFiles ? "1" : "0");
      cmd.add(String.valueOf(imTolerance));
      cmd.add(String.valueOf(mzToleranceUnit));
      cmd.add(String.valueOf(mzTolerance));
      cmd.add(String.valueOf(apexScanDeltaRange));
      cmd.add(String.valueOf(imToleranceMs1Ms2));
      cmd.add(String.valueOf(apexScanDeltaRangeMs1Ms2));

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
      if (!f.getDataType().contentEquals("DIA") || !f.getPath().getFileName().toString().endsWith(".d")) {
        out.add(f);
      } else {
        String s = f.getPath().toAbsolutePath().toString().replace(".d", "_centric.mzML");
        out.add(new InputLcmsFile(Paths.get(s), f.getGroup(), f.getReplicate(), "DDA"));
      }
    }
    return out;
  }
}
