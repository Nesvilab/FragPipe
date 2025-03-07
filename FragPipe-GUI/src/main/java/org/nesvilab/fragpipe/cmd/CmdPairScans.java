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

import org.nesvilab.fragpipe.Fragpipe;
import org.nesvilab.fragpipe.FragpipeLocations;
import org.nesvilab.fragpipe.api.InputLcmsFile;
import org.nesvilab.fragpipe.tools.opair.OPairParams;
import org.nesvilab.fragpipe.util.PairScans;
import org.nesvilab.utils.SwingUtils;
import java.awt.Component;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.jooq.lambda.Seq;

public class CmdPairScans extends CmdBase {

    public static final String NAME = "PairScans";
    public static final String JAR_MSFTBX_NAME = ToolingUtils.BATMASS_IO_JAR;
    private static String[] JAR_DEPS = {JAR_MSFTBX_NAME};
    private static final List<String> SUPPORTED_FORMATS = Arrays.asList("mzML", "mzXML");

    public CmdPairScans(boolean isRun, Path workDir) {
        super(isRun, workDir);
    }

    @Override
    public String getCmdName() {
        return NAME;
    }

    public boolean configure(Component component,
        Path extLibsThermo,
        Path extLibsBruker,
        Path jarFragpipe,
        int ramGb,
        int nThreads,
        List<InputLcmsFile> lcmsFiles,
        OPairParams params) {
        initPreConfig();

        final List<Path> classpathJars = FragpipeLocations.checkToolsMissing(Seq.of(jarFragpipe.toAbsolutePath().normalize().toString()).concat(JAR_DEPS));
        if (classpathJars == null || classpathJars.isEmpty()) {
            SwingUtils.showErrorDialog(component, "Could not find " + String.join(", ", JAR_DEPS) + " for scan pairing.", "Error");
            return false;
        }

        List<String> sup = new ArrayList<>(SUPPORTED_FORMATS);
        if (extLibsBruker != null) {
            sup.add("d");
        }
        if (extLibsThermo != null) {
            sup.add("raw");
        }
        if (!checkCompatibleFormats(component, lcmsFiles, sup)) {
            return false;
        }

        String brukerLib = "";
        String thermoLib = "";

        if (extLibsBruker != null) {
            brukerLib = createJavaDParamString("libs.bruker.dir", extLibsBruker.toString());
        } else {
            if (lcmsFiles.stream().anyMatch(f -> f.getPath().getFileName().toString().toLowerCase().endsWith(".d"))) {
                SwingUtils.showErrorDialog(component, "When processing .d files PairScans requires native Bruker libraries. Native libraries come with MSFragger zip download, contained in ext sub-directory.", NAME + " error");
                return false;
            }
        }

        if (extLibsThermo != null) {
            thermoLib = createJavaDParamString("libs.thermo.dir", extLibsThermo.toString());
        } else {
            if (lcmsFiles.stream().anyMatch(f -> f.getPath().getFileName().toString().toLowerCase().endsWith(".raw"))) {
                SwingUtils.showErrorDialog(component, "When processing .RAW files PairScans requires native Thermo libraries. Native libraries come with MSFragger zip download, contained in ext sub-directory.", NAME + " error");
                return false;
            }
        }

        for (InputLcmsFile lcms : lcmsFiles) {
            List<String> cmd = new ArrayList<>();
            cmd.add(Fragpipe.getBinJava());
            cmd.add("-Xmx" + ramGb + "G");
            if (!brukerLib.equals("")) {
                cmd.add(brukerLib);
            }
            if (!thermoLib.equals("")) {
                cmd.add(thermoLib);
            }
            cmd.add("-cp");
            cmd.add(constructClasspathString(classpathJars));
            cmd.add(PairScans.class.getCanonicalName());
            cmd.add(lcms.getPath().toAbsolutePath().normalize().toString());
            cmd.add(nThreads + "");
            cmd.add(params.getActivation1());
            cmd.add(params.getActivation2());
            cmd.add(String.valueOf(params.isReverseScanOrder()));
            cmd.add(String.valueOf(params.isSingleScanType()));
            ProcessBuilder pb = new ProcessBuilder(cmd);
            pbis.add(PbiBuilder.from(pb));
        }

        isConfigured = true;
        return true;
    }

    private boolean checkCompatibleFormats(Component comp, List<InputLcmsFile> inputLcmsFiles, List<String> supportedFormats) {
        List<String> notSupportedExts = getNotSupportedExts(inputLcmsFiles, supportedFormats);
        if (!notSupportedExts.isEmpty()) {
            SwingUtils.showErrorDialog(comp, String.format(".%s files need the \"ext\" folder from MSFragger.", String.join(", ", notSupportedExts)), NAME + " error");
            return false;
        }
        return true;
    }
}
