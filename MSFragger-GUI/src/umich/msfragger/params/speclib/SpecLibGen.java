package umich.msfragger.params.speclib;

import java.nio.file.Path;
import org.greenrobot.eventbus.EventBus;
import umich.msfragger.util.CheckResult;
import umich.msfragger.util.Installed;
import umich.msfragger.util.JarUtils;
import umich.msfragger.util.PythonInfo;
import umich.msfragger.util.PythonModule;

public class SpecLibGen {
  private static SpecLibGen instance = new SpecLibGen();
  public static SpecLibGen get() {
    return instance;
  }
  private static final String DEFAULT_MESSAGE = "Python 3 with cython, msproteomicstools is "
      + "needed for Spectral Library generation functionality";
  private static final String SCRIPT_SPEC_LIB_GEN = "/speclib/gen_con_spec_lib.py";


  private PythonInfo pi;
  private Path scriptSpecLibGenPath;
  private boolean initOk;

  private SpecLibGen() {
    pi = PythonInfo.get();
    scriptSpecLibGenPath = null;
  }

  public static final String[] RESOURCE_LOCATIONS = {
      "/speclib/common_funcs.py",
      "/speclib/detect_decoy_prefix.py",
      SCRIPT_SPEC_LIB_GEN,
      "/speclib/spectrast",
      "/speclib/spectrast.exe",
      "/speclib/spectrast_gen_pepidx.py",
      "/speclib/unite_runs.py",
  };

  public static final PythonModule[] REQUIRED_MODULES = {
      new PythonModule("Cython", "Cython"),
      new PythonModule("msproteomicstools", "msproteomicstoolslib")
  };

  public Path getScriptSpecLibGenPath() {
    return scriptSpecLibGenPath;
  }

  public boolean isInitialized() {
    return scriptSpecLibGenPath != null && pi.isAvailable() && pi.getMajorVersion() >= 3;
  }

  public boolean isEnabled() {
    return initOk;
  }

  public void init() {

    // check python version
    boolean isVersionOk = false;
    try {
      CheckResult res = checkPythonVer();
      isVersionOk = res.isSuccess;
      EventBus.getDefault().post(new MessagePythonInfo(res.message));
    } catch (Exception e) {
      EventBus.getDefault().post(new MessagePythonInfo("<html><b>Error checking python version"));
    }


    // check installed modules
    boolean isModulesInstalled = false;
    if (isVersionOk) {
      try {
        CheckResult res = checkPythonModules();
        isModulesInstalled = res.isSuccess;
        EventBus.getDefault().post(new MessagePythonInfo(res.message));
      } catch (Exception ex) {
        EventBus.getDefault().post(new MessagePythonInfo("<html><b>Error checking installed python modules"));
      }
    }

    boolean isUnpacked = false;
    if (isModulesInstalled) {
      try {
        CheckResult res = unpack();
        isUnpacked = res.isSuccess;
        EventBus.getDefault().post(new MessagePythonInfo(res.message));
      } catch (Exception e) {
        EventBus.getDefault().post(new MessagePythonInfo("<html><b>Error unpacking necessary tools"));
      }
    }

    final boolean isInitSuccess = isVersionOk && isModulesInstalled && isUnpacked;
    initOk = isInitSuccess;
    if (!isInitSuccess) {
      EventBus.getDefault().post(new MessagePythonMoreInfo(DEFAULT_MESSAGE));
    } else {
      EventBus.getDefault().post(new MessagePythonMoreInfo("All looks good, spectral library generation enabled."));
    }
    EventBus.getDefault().post(new InitDone(isInitSuccess));
  }

  private CheckResult checkPythonVer() throws Exception {
    if (!pi.isAvailable())
      pi.findPythonCommand();
    if (!pi.isAvailable()) {
      return new CheckResult(false, "<html<b>Python not found<b>");
    } else if (pi.getMajorVersion() != 3) {
      return new CheckResult(false, "<html>Python: <b>" + pi.getVersion() + "</b>");
    }
    return new CheckResult(true, "<html>Python: " + pi.getVersion() + "");
  }

  private CheckResult checkPythonModules() throws Exception {
    if (!pi.isAvailable())
      throw new IllegalStateException("Checking for installed modules while python is not available");
    boolean isAllModules = true;
    for (PythonModule m : REQUIRED_MODULES) {
      if (pi.checkModuleInstalled(m) != Installed.YES)
        isAllModules = false;
    }
    StringBuilder sb = createPythonVerSb();
    return !isAllModules
        ? new CheckResult(false, sb.toString())
        : new CheckResult(true, sb.toString());
  }

  private StringBuilder createPythonVerSb() {
    StringBuilder sb = new StringBuilder("<html>Python: " + pi.getVersion() + ". Modules:");
    for (PythonModule m: REQUIRED_MODULES) {
      Installed installed = pi.checkModuleInstalled(m);
      switch (installed) {
        case YES:
          sb.append(" ").append(m.installName).append(" - Yes");
          break;
        case NO:
          sb.append(" <b>").append(m.installName).append(" - Yes</b>");
          break;
        case UNKNOWN:
          sb.append(" <b>").append(m.installName).append(" - N/A</b>");
          break;
      }
    }
    return sb;
  }

  private CheckResult unpack() throws Exception {
    for (String rl : RESOURCE_LOCATIONS) {
      Path path = JarUtils.unpackFromJar(SpecLibGen.class, rl, false, false);
      if (SCRIPT_SPEC_LIB_GEN.equals(rl))
        scriptSpecLibGenPath = path;
    }
    return new CheckResult(true, createPythonVerSb().append(" Assets unpacked OK.").toString());
  }

  public static class MessagePythonInfo {
    public final String text;

    public MessagePythonInfo(String text) {
      this.text = text;
    }
  }

  public static class MessagePythonMoreInfo {
    public final String text;

    public MessagePythonMoreInfo(String text) {
      this.text = text;
    }
  }

  public static class InitDone {
    public final boolean isSuccess;

    public InitDone(boolean isSuccess) {
      this.isSuccess = isSuccess;
    }
  }
}
