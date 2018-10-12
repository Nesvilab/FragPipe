package umich.msfragger.params.speclib;

import java.nio.file.Path;
import org.greenrobot.eventbus.EventBus;
import umich.msfragger.params.dbslice.DbSlice;
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
  public static final String DEFAULT_MESSAGE = "Python 3 with cython, msproteomicstools is "
      + "needed for Spectral Library generation functionality.";
  private static final String SCRIPT_SPEC_LIB_GEN = "/speclib/gen_con_spec_lib.py";


  private PythonInfo pi;
  private Path scriptSpecLibGenPath;
  private boolean isInitialized;

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

  public PythonInfo getPi() {
    return pi;
  }

  public Path getScriptSpecLibGenPath() {
    return scriptSpecLibGenPath;
  }

  public static abstract class Message {
    public final boolean append;
    public final boolean isError;
    public final String text;

    public Message(boolean append, boolean isError, String text) {
      this.append = append;
      this.isError = isError;
      this.text = text;
    }

    @Override
    public String toString() {
      return "Message{" +
          "append=" + append +
          ", isError=" + isError +
          ", text='" + text + '\'' +
          '}';
    }
  }

  public static class Message1 extends DbSlice.Message {
    public Message1(boolean append, boolean isError, String text) {
      super(append, isError, text);
    }
  }

  public static class Message2 extends DbSlice.Message {
    public Message2(boolean append, boolean isError, String text) {
      super(append, isError, text);
    }
  }

  public static class InitDone {
    public final boolean isSuccess;

    public InitDone(boolean isSuccess) {
      this.isSuccess = isSuccess;
    }
  }

  public boolean isInitialized() {
    return isInitialized;
  }

  public void init() {
    // reset default label text
    EventBus.getDefault().post(new SpecLibGen.Message1(false, false, ""));
    EventBus.getDefault().post(new SpecLibGen.Message2(false, false, ""));

    // check python version
    boolean isVersionOk = false;
    try {
      CheckResult res = checkPythonVer();
      isVersionOk = res.isSuccess;
      EventBus.getDefault().post(new Message1(true, !res.isSuccess, res.message));
    } catch (Exception e) {
      EventBus.getDefault().post(new Message1(true,true, "Error checking python version."));
    }


    // check installed modules
    boolean isModulesInstalled = false;
    if (isVersionOk) {
      try {
        CheckResult res = checkPythonModules();
        isModulesInstalled = res.isSuccess;
        EventBus.getDefault().post(new Message1(true, !res.isSuccess, res.message));
      } catch (Exception ex) {
        EventBus.getDefault().post(new Message1(true, true, "Error checking installed python modules."));
      }
    }

    boolean isUnpacked = false;
    if (isModulesInstalled) {
      try {
        CheckResult res = unpack();
        isUnpacked = res.isSuccess;
        EventBus.getDefault().post(new Message1(true, !res.isSuccess, res.message));
      } catch (Exception e) {
        EventBus.getDefault().post(new Message1(true, true, "Error unpacking necessary tools."));
      }
    }

    final boolean isInitSuccess = isVersionOk && isModulesInstalled && isUnpacked;
    isInitialized = isInitSuccess;
    EventBus.getDefault().postSticky(new InitDone(isInitSuccess));
  }

  private CheckResult checkPythonVer() throws Exception {
    if (!pi.isAvailable())
      pi.findPythonCommand();
    if (!pi.isAvailable()) {
      return new CheckResult(false, "Python not found.");
    } else if (pi.getMajorVersion() != 3) {
      return new CheckResult(false, "Python: " + pi.getVersion() + ".");
    }
    return new CheckResult(true, "Python: " + pi.getVersion() + ".");
  }

  private CheckResult checkPythonModules() {
    if (!pi.isAvailable())
      throw new IllegalStateException("Checking for installed modules while python is not available.");
    boolean isAllModules = true;
    for (PythonModule m : REQUIRED_MODULES) {
      if (pi.checkModuleInstalled(m) != Installed.YES)
        isAllModules = false;
    }
    StringBuilder sb = createPythonModulesReport();
    return new CheckResult(isAllModules, sb.toString());
  }

  private StringBuilder createPythonModulesReport() {
    if (REQUIRED_MODULES.length == 0) {
      return new StringBuilder("Modules: none required.");
    }
    StringBuilder sb = new StringBuilder("Modules: ");
    for (int i = 0; i < REQUIRED_MODULES.length; i++) {
      PythonModule m = REQUIRED_MODULES[i];
      Installed installed = pi.checkModuleInstalled(m);
      if (i > 0)
        sb.append(", ");
      sb.append(m.installName);
      switch (installed) {
        case YES:
          sb.append(" - Yes");
          break;
        case NO:
          sb.append(" - No");
          break;
        case UNKNOWN:
          sb.append(" - N/A");
          break;
      }
    }
    sb.append(".");
    return sb;
  }

  private CheckResult unpack() throws Exception {
    for (String rl : RESOURCE_LOCATIONS) {
      Path path = JarUtils.unpackFromJar(SpecLibGen.class, rl, false, false);
      if (SCRIPT_SPEC_LIB_GEN.equals(rl))
        scriptSpecLibGenPath = path;
    }
    return new CheckResult(true, "Assets unpacked OK.");
  }
}
