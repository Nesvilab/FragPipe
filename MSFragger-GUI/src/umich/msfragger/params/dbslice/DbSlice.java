package umich.msfragger.params.dbslice;

import java.nio.file.Path;
import java.util.Properties;
import org.greenrobot.eventbus.EventBus;
import umich.msfragger.params.fragger.MsfraggerProps;
import umich.msfragger.params.speclib.SpecLibGen;
import umich.msfragger.util.CheckResult;
import umich.msfragger.util.Installed;
import umich.msfragger.util.JarUtils;
import umich.msfragger.util.PropertiesUtils;
import umich.msfragger.util.PythonInfo;
import umich.msfragger.util.PythonModule;
import umich.msfragger.util.VersionComparator;

public class DbSlice {
  private static DbSlice instance = new DbSlice();
  public static DbSlice get() { return instance; }
  private static final String DEFAULT_MESSAGE = "Python 3 with numpy, pandas is "
      + "needed for DB Slicing functionality.";

  private static final String SCRIPT_SPEC_LIB_GEN = "/speclib/gen_con_spec_lib.py";
  private static final String SCRIPT_SPLITTER = "/" + MsfraggerProps.PYTHON_SPLITTER_NAME;
  public static final String[] RESOURCE_LOCATIONS = {
      SCRIPT_SPLITTER
  };
  public static final PythonModule[] REQUIRED_MODULES = {
      new PythonModule("numpy", "numpy"),
      new PythonModule("pandas", "pandas"),
  };


  private PythonInfo pi;
  private Path scriptDbslicingPath;
  private String msfraggerVer;
  private boolean initOk;

  private DbSlice() {
    pi = PythonInfo.get();
    scriptDbslicingPath = null;
    msfraggerVer = null;
  }

  public Path getScriptDbslicingPath() {
    return scriptDbslicingPath;
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

  public static class Message1 extends Message {
    public Message1(boolean append, boolean isError, String text) {
      super(append, isError, text);
    }
  }

  public static class Message2 extends Message {
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

  public boolean isEnabled() {
    return initOk;
  }

  public void init(String msfraggerVersion) {
    EventBus.getDefault().post(new Message1(false, false, ""));
    EventBus.getDefault().post(new Message2(false, false, ""));

    // check python version
    boolean isPythonOk = false;
    try {
      CheckResult res = checkPythonVer();
      isPythonOk = res.isSuccess;
      EventBus.getDefault().post(new Message1(true, !res.isSuccess, res.message));
    } catch (Exception e) {
      EventBus.getDefault().post(new Message1(true,
          true, "Error checking python version."
      ));
    }


    // check installed modules
    boolean isModulesInstalled = false;
    if (isPythonOk) {
      try {
        CheckResult res = checkPythonModules();
        isModulesInstalled = res.isSuccess;
        EventBus.getDefault().post(new Message1(true, !res.isSuccess, res.message));
      } catch (Exception ex) {
        EventBus.getDefault().post(new Message1(true,
            true, "Error checking installed python modules."
        ));
      }
    }

    boolean isUnpacked = false;
    if (isModulesInstalled) {
      try {
        CheckResult res = unpack();
        isUnpacked = res.isSuccess;
        EventBus.getDefault().post(new Message1(true, !res.isSuccess, res.message));
      } catch (Exception e) {
        EventBus.getDefault().post(new Message1(true,
            true, "Error unpacking necessary tools."
        ));
      }
    }

    CheckResult res = checkFraggerVer(msfraggerVersion);
    boolean isFraggerOk = res.isSuccess;
    if (!res.isSuccess) {
      EventBus.getDefault().post(new Message2(true, true,
          "Update MSFragger to a newer version."));
      EventBus.getDefault().post(new Message2(true, false,
          "Use the Update button next to MSFragger field."));
    }

    final boolean isInitSuccess = isPythonOk && isModulesInstalled && isUnpacked && isFraggerOk;
    initOk = isInitSuccess;
    EventBus.getDefault().post(new InitDone(isInitSuccess));
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
      throw new IllegalStateException("Checking for installed modules while python is not available");
    boolean isAllModulesOk = true;
    for (PythonModule m : REQUIRED_MODULES) {
      if (pi.checkModuleInstalled(m) != Installed.YES)
        isAllModulesOk = false;
    }
    StringBuilder sb = pythonModulesReport();
    return isAllModulesOk
        ? new CheckResult(true, sb.toString())
        : new CheckResult(false, sb.toString());
  }

  private StringBuilder pythonModulesReport() {
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
        scriptDbslicingPath = path;
    }
    return new CheckResult(true, " Assets unpacked OK.");
  }

  private CheckResult checkFraggerVer(String fraggerVer) {
    VersionComparator cmp = new VersionComparator();
    // for the lack of a better default, we'll just hard code this here
    String minFraggerVer = "20180924";
    Properties props = PropertiesUtils
        .loadPropertiesLocal(MsfraggerProps.class, MsfraggerProps.PROPERTIES_FILE_NAME);
    if (props != null)
      minFraggerVer = props.getProperty(MsfraggerProps.PROP_MIN_VERSION_SLICING, minFraggerVer);
    if (minFraggerVer == null) {
      throw new IllegalStateException(MsfraggerProps.PROP_MIN_VERSION_SLICING +
          " property needs to be in the local properties: " + MsfraggerProps.PROPERTIES_FILE_NAME);
    }
    int fraggerVersionCmp = cmp.compare(fraggerVer, minFraggerVer);

    if (fraggerVersionCmp >= 0)
      return new CheckResult(true, "MSfragger: OK");
    else
      return new CheckResult(false, "MSFragger: " + fraggerVer);
  }
}
