package com.dmtavt.fragpipe.cmd;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.FragpipeLocations;
import com.dmtavt.fragpipe.tabs.TabWorkflow.InputDataType;
import com.github.chhh.utils.StringUtils;
import com.github.chhh.utils.SwingUtils;
import java.awt.Component;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import javax.swing.JOptionPane;
import org.jooq.lambda.Seq;
import org.jooq.lambda.function.Function3;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.dmtavt.fragpipe.api.InputLcmsFile;
import com.dmtavt.fragpipe.api.LcmsFileGroup;

public class CmdIonquant extends CmdBase {
  private static final Logger log = LoggerFactory.getLogger(CmdIonquant.class);

  public static final String NAME = "IonQuant";
  public static final String JAR_IONQUANT_NAME = "ionquant-1.4.4.jar";
  public static final String JAR_MSFTBX_NAME = ToolingUtils.BATMASS_IO_JAR;
  public static final String JAR_IONQUANT_MAIN_CLASS = "ionquant.IonQuant";
  private static String[] JAR_DEPS = {JAR_MSFTBX_NAME};
  public static final List<String> SUPPORTED_FORMATS = Arrays.asList("mzML", "mzXML");

  private static final String UNPACK_SUBDIR_IN_TEMP = "fragpipe";

  public CmdIonquant(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }


  @Override
  public String getCmdName() {
    return NAME;
  }

  public boolean configure(Component comp, Path binFragger, int ramGb,
      Map<String, String> uiCompsRepresentation,
      InputDataType dataType,
      Map<InputLcmsFile, List<Path>> lcmsToFraggerPepxml,
      Map<LcmsFileGroup, Path> mapGroupsToProtxml, int nThreads) {

//    Usage:
//    java -jar IonQuant.jar <options> <.d/.mzML/.mzXML/.pepXML/_quant.csv files>
//        Options:
//        --mztol <float>        # MS1 tolerance in PPM. Default: 20.0
//        --imtol <float>        # 1/K0 tolerance. Default: 0.1
//        --plot 0/1             # Plot traced features or not. Default: 0
//        --psm <string>         # Path to Philosopher's psm.tsv. Optional.
//        --multidir <string>    # Output dir for the multi experimental result. Optional.
    initPreConfig();

    List<String> sup = new ArrayList<>(SUPPORTED_FORMATS);
    final Path extLibsBruker = CmdMsfragger.searchExtLibsBruker(Collections.singletonList(binFragger.getParent()));
    if (extLibsBruker != null) {
      sup.add("d");
    }
    final Path extLibsThermo = CmdMsfragger.searchExtLibsThermo(Collections.singletonList(binFragger.getParent()));
    if (extLibsThermo != null) {
      sup.add("raw");
    }
    if (!checkCompatibleFormats(comp, lcmsToFraggerPepxml, sup)) {
      return false;
    }

    final List<Path> classpathJars = FragpipeLocations.checkToolsMissing(Seq.of(JAR_IONQUANT_NAME).concat(JAR_DEPS));
    if (classpathJars == null) {
      return false;
    }

    List<String> cmd = new ArrayList<>();
    cmd.add(Fragpipe.getBinJava());
    if (ramGb > 0) {
      cmd.add("-Xmx" + ramGb + "G");
    }

    if (extLibsBruker != null) {
      cmd.add(createJavaDParamString("libs.bruker.dir", extLibsBruker.toString()));
    } else {
      if (lcmsToFraggerPepxml.keySet().stream().anyMatch(f ->
              f.getPath().getFileName().toString().toLowerCase().endsWith(".d"))) {
        JOptionPane.showMessageDialog(comp,
                "<html>When processing .d files IonQuant requires native Bruker libraries.<br/>\n"
                + "Native libraries come with MSFragger zip download, contained in <i>ext</i><br/>\n"
                + "sub-directory. If you don't have an <i>ext</i> directory next to MSfragger.jar<br/>\n"
                + "please go to Config tab and Update MSFragger.",
                NAME + " error", JOptionPane.WARNING_MESSAGE);
        return false;
      }
    }

    if (extLibsThermo != null) {
      cmd.add(createJavaDParamString("libs.thermo.dir", extLibsThermo.toString()));
    } else {
      if (lcmsToFraggerPepxml.keySet().stream().anyMatch(f ->
          f.getPath().getFileName().toString().toLowerCase().endsWith(".raw"))) {
        JOptionPane.showMessageDialog(comp,
            "<html>When processing .RAW files IonQuant requires native Thermo libraries.<br/>\n"
                + "Native libraries come with MSFragger zip download, contained in <i>ext</i><br/>\n"
                + "sub-directory. If you don't have an <i>ext</i> directory next to MSfragger.jar<br/>\n"
                + "please go to Config tab and Update MSFragger.",
            NAME + " error", JOptionPane.WARNING_MESSAGE);
        return false;
      }
    }

    cmd.add("-cp");
    cmd.add(constructClasspathString(classpathJars));
    cmd.add(JAR_IONQUANT_MAIN_CLASS);
    cmd.add("--threads");
    cmd.add(String.valueOf(nThreads));

    cmd.add("--ionmobility");
    cmd.add(dataType == InputDataType.ImMsTimsTof ? "1" : "0");

    // add all other parameters
    List<String> dynamicParams = Arrays.asList(
        "mbr",
        "proteinquant",
        "requantify",
        "mztol",
        "imtol",
        "rttol",
        "mbrmincorr",
        "mbrrttol",
        "mbrimtol",
        "mbrtoprun",
        "ionfdr",
        "proteinfdr",
        "peptidefdr",
        "normalization",
        "minisotopes",
        "light",
        "medium",
        "heavy",
        "tp",
        "minfreq",
        "minions",
        "excludemods",
        "minexps"
        );

    final long namedExpCount = mapGroupsToProtxml.keySet().stream().map(group -> group.name)
        .filter(StringUtils::isNotBlank).distinct().count();
    final boolean isMultidir = namedExpCount > 0;


    for (String dynamicParam : dynamicParams) {
      String v = getOrThrow(uiCompsRepresentation,
          StringUtils.prependOnce(dynamicParam, "ionquant."));
      if ("mbr".equalsIgnoreCase(dynamicParam) && "1".equals(v)) {
        // it's mbr
        if (!isMultidir) {
          // it's not multi exp
          JOptionPane.showMessageDialog(comp, SwingUtils.makeHtml(
              "IonQuant with MBR on (match between runs) requires designating  LCMS runs to experiments.\n"
                  + "See Workflow tab.\n"
                  + "If in doubt how to resolve this error, just assign all LCMS runs to the same experiment name."),
              NAME + " error", JOptionPane.WARNING_MESSAGE);
          return false;
        }
      }
      if (StringUtils.isNotBlank(v)) {
        cmd.add("--" + dynamicParam);
        cmd.add(v);
      }
    }

    for (Entry<LcmsFileGroup, Path> e : mapGroupsToProtxml.entrySet()) {
      LcmsFileGroup group = e.getKey();
      Path psmTsv = group.outputDir(wd).resolve("psm.tsv");
      cmd.add("--psm");
      cmd.add(wd.relativize(psmTsv).toString());
    }

    if (isMultidir) {
      cmd.add("--multidir");
      cmd.add(".");
    }

    // compute unique lcms file directories
    Set<Path> lcmsDirsUnique = Seq.seq(lcmsToFraggerPepxml.keySet()).map(lcms -> lcms.getPath().getParent())
        .toSet();
    for (Path path : lcmsDirsUnique) {
      cmd.add("--specdir");
      cmd.add(StringUtils.appendPrependOnce(path.toString(), "\""));
    }

    for (Entry<InputLcmsFile, List<Path>> e : lcmsToFraggerPepxml.entrySet()) {
      InputLcmsFile lcms = e.getKey();
      for (Path pepxml : e.getValue()) {
        //cmd.add(lcms.getPath().toString()); // lcms files are not needed anymore, auto-searched in --specdir
        cmd.add(wd.relativize(pepxml).toString());
      }
    }

    ProcessBuilder pb = new ProcessBuilder(cmd);
    pb.directory(wd.toFile());
    pbis.add(PbiBuilder.from(pb));

    isConfigured = true;
    return true;
  }

  private String getOrThrow(Map<String, String> m, String key) {
    String s = m.get(key);
    if (s == null)
      throw new IllegalStateException("Could not get key: " + key);
    return s;
  }

  private boolean checkCompatibleFormats(Component comp,  Map<InputLcmsFile, List<Path>> lcmsToPepxml, List<String> supportedFormats) {
    List<String> notSupportedExts = getNotSupportedExts1(lcmsToPepxml, supportedFormats);
    if (!notSupportedExts.isEmpty()) {
      StringBuilder sb = new StringBuilder();
      sb.append(String.format("<html>%s can't work with '.%s' files.<br/>", NAME, String.join(", ", notSupportedExts)));
      if (notSupportedExts.contains(".d") || notSupportedExts.contains("d")) {
        sb.append("Support for Bruker files requires 'ext' folder with 'bruker' sub-folder<br/>\n")
            .append("to be next to your MSFragger.jar. It is shipped with MSFragger.zip distribution.<br/>\n");
      }
      sb.append(String.format("Compatible formats are: %s<br/>", String.join(", ", supportedFormats)));
      sb.append(String.format("Either remove files from input or disable %s<br/>", NAME));
      sb.append("You can also convert files using <i>msconvert</i> from ProteoWizard.");

      JOptionPane.showMessageDialog(comp, sb.toString(), NAME + " error", JOptionPane.WARNING_MESSAGE);
      return false;
    }
    return true;
  }
}
