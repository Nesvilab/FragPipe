package umich.msfragger.params.dbslice;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import org.greenrobot.eventbus.EventBus;
import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.ThreadMode;
import com.dmtavt.fragpipe.messages.MessageToolInit;
import umich.msfragger.params.dbslice.DbSlice.MessageInitDone.REASON;
import umich.msfragger.params.fragger.MsfraggerProps;
import com.dmtavt.fragpipe.tools.msfragger.MsfraggerVerCmp;
import umich.msfragger.params.speclib.SpecLibGen;
import com.github.chhh.utils.CheckResult;
import com.github.chhh.utils.Installed;
import com.github.chhh.utils.JarUtils;
import com.github.chhh.utils.PythonInfo;
import com.github.chhh.utils.PythonModule;

public class DbSlice {
  private static DbSlice instance = new DbSlice();
  private final Object initLock = new Object();
  public static DbSlice get() { return instance; }
  public static final String DEFAULT_MESSAGE = "Python 3 with numpy, pandas is "
      + "needed for DB Splitting functionality.";

  private static final String UNPACK_SUBDIR_IN_TEMP = "fragpipe";
  private static final String SCRIPT_SPEC_LIB_GEN = "/speclib/gen_con_spec_lib.py";
  private static final String SCRIPT_SPLITTER = "/" + MsfraggerProps.DBSPLIT_SCRIPT_NAME;
  public static final String[] RESOURCE_LOCATIONS = {SCRIPT_SPLITTER};
  public static final PythonModule[] REQUIRED_MODULES = {
      new PythonModule("numpy", "numpy"),
      new PythonModule("pandas", "pandas"),
  };

  private PythonInfo pi;
  private Path scriptDbslicingPath;
  private String msfraggerVer;
  private boolean isInitialized;

  private DbSlice() {
    pi = PythonInfo.get();
    scriptDbslicingPath = null;
    msfraggerVer = null;
    isInitialized = false;
    EventBus.getDefault().register(this);
  }

  @Subscribe(threadMode = ThreadMode.MAIN)
  public void onMessagePythonInfoChanged(PythonInfo.MessageInfoChanged m) {
    init(msfraggerVer);
  }

  public Path getScriptDbslicingPath() {
    return scriptDbslicingPath;
  }

  public static class Message1 extends MessageToolInit {
    public Message1(boolean append, boolean isError, String text) {
      super(append, isError, text);
    }
  }

  public static class Message2 extends MessageToolInit {
    public Message2(boolean append, boolean isError, String text) {
      super(append, isError, text);
    }
  }

  public static class MessageInitDone {
    public enum REASON {NOT_UNPACKED, PY_VER, PY_MODULES, WRONG_FRAGGER}
    public final boolean isSuccess;
    public final EnumSet<REASON> reasons;

    public MessageInitDone(boolean isSuccess, EnumSet<REASON> reasons) {
      this.isSuccess = isSuccess;
      this.reasons = reasons;
    }


  }

  public PythonInfo getPi() {
    return pi;
  }

  public boolean isInitialized() {
    synchronized (initLock) {
      return isInitialized;
    }
  }

  public void init(String msfraggerVersion) {
    synchronized (initLock) {
      EnumSet<REASON> reasons = EnumSet.noneOf(REASON.class);

      // reset default label text
      EventBus.getDefault().post(new Message1(false, false, ""));
      EventBus.getDefault().post(new Message2(false, false, ""));

      // check python version
      boolean isPythonOk = false;
      try {
        CheckResult res = checkPythonVer();
        isPythonOk = res.isSuccess;
        EventBus.getDefault().post(new Message1(true, !isPythonOk, res.message));
        if (!isPythonOk) {
          reasons.add(REASON.PY_VER);
        }
      } catch (Exception e) {
        EventBus.getDefault().post(new Message1(true, true, "Error checking python version."));
        reasons.add(REASON.PY_VER);
      }

      // check ok/installed modules
      if (isPythonOk) {
        try {
          CheckResult res = checkPythonOkModules();
          EventBus.getDefault().post(new Message1(true, false, res.message));
        } catch (Exception ex) {
          EventBus.getDefault()
              .post(new Message1(true, true, "Error checking installed/ok python modules."
              ));
        }
      }

      // check missing/error modules
      boolean isNoErrorModules = false;
      if (isPythonOk) {
        try {
          CheckResult res = checkPythonErrorModules();
          isNoErrorModules = res.isSuccess;
          EventBus.getDefault().post(new Message1(true, !isNoErrorModules, res.message));
          if (!isNoErrorModules) {
            reasons.add(REASON.PY_MODULES);
          }
        } catch (Exception ex) {
          EventBus.getDefault()
              .post(new Message1(true, true, "Error checking installed python modules."
              ));
          reasons.add(REASON.PY_MODULES);
        }

      }

      boolean isUnpacked = false;
      if (isNoErrorModules) {
        try {
          CheckResult res = unpack();
          isUnpacked = res.isSuccess;
          EventBus.getDefault().post(new Message1(true, !isUnpacked, res.message));
          if (!isUnpacked) {
            reasons.add(REASON.NOT_UNPACKED);
          }
        } catch (Exception e) {
          EventBus.getDefault().post(new Message1(true, true, "Error unpacking necessary tools."));
          reasons.add(REASON.NOT_UNPACKED);
        }
      }

      boolean isFraggerOk = true;
      if (msfraggerVersion != null) {
        CheckResult res = checkFraggerVer(msfraggerVersion);
        isFraggerOk = res.isSuccess;
        if (!res.isSuccess) {
          EventBus.getDefault()
              .post(new Message2(true, true, "Update MSFragger to a newer version."));
          EventBus.getDefault()
              .post(new Message2(true, false, "Use the Update button next to MSFragger field."));
        } else {
          this.msfraggerVer = msfraggerVersion;
        }
      }
      if (!isFraggerOk) {
        reasons.add(REASON.WRONG_FRAGGER);
      }

      final boolean isInitSuccess = isPythonOk && isNoErrorModules && isUnpacked && isFraggerOk;
      isInitialized = isInitSuccess;

      EventBus.getDefault().postSticky(new MessageInitDone(isInitSuccess, reasons));
    }
  }

  private CheckResult checkPythonOkModules() {
    StringBuilder sb = new StringBuilder("Modules:");
    if (REQUIRED_MODULES.length == 0) {
      sb.append(" none required");
    } else {
      List<PythonModule> okMods = createPythonModulesStatusList(Installed.YES);
      if (!okMods.isEmpty()) {
        String pmList = okMods.stream().map(pm -> pm.installName).collect(Collectors.joining(", "));
        sb.append(" ").append(pmList).append(" - OK;");
      }
    }
    return new CheckResult(true, sb.toString());
  }

  private CheckResult checkPythonErrorModules() {
    List<Installed> badStatuses = Arrays.stream(Installed.values())
        .filter(installed -> !installed.equals(Installed.YES)).collect(
            Collectors.toList());

    final Map<Installed, String> statusNameMap = new HashMap<>();
    statusNameMap.put(Installed.INSTALLED_WITH_IMPORTERROR, "Error loading module");
    statusNameMap.put(Installed.NO, "Missing");
    statusNameMap.put(Installed.UNKNOWN, "N/A");

    final List<String> badModsByStatus = new ArrayList<>();
    for (Installed badStatus : badStatuses) {
      List<PythonModule> badMods = createPythonModulesStatusList(badStatus);
      if (!badMods.isEmpty()) {
        StringBuilder sb = new StringBuilder();
        sb.append(badMods.stream().map(pm -> pm.installName).collect(Collectors.joining(", ")))
            .append(" - ").append(statusNameMap.getOrDefault(badStatus, badStatus.name()));
        badModsByStatus.add(sb.toString());
      }
    }
    if (badModsByStatus.isEmpty()) {
      return new CheckResult(true, "");
    }
    return new CheckResult(false, " " + String.join(", ", badModsByStatus));
  }

  private CheckResult checkPythonVer() throws Exception {
    if (!pi.isInitialized())
      pi.findPythonCommand();
    if (!pi.isInitialized()) {
      return new CheckResult(false, "Python not found.");
    } else if (pi.getMajorVersion() != 3) {
      return new CheckResult(false, "Python: " + pi.getVersion() + ".");
    }
    return new CheckResult(true, "Python: " + pi.getVersion() + ".");
  }

  private List<PythonModule> createPythonModulesStatusList(Installed installedStatus) {
    return Arrays.stream(REQUIRED_MODULES)
        .filter(pm -> installedStatus.equals(pi.checkModuleInstalled(pm)))
        .collect(Collectors.toList());
  }

  private CheckResult unpack() throws Exception {
    for (String rl : RESOURCE_LOCATIONS) {
      Path subDir = Paths.get(UNPACK_SUBDIR_IN_TEMP);
      Path path = JarUtils.unpackFromJar(SpecLibGen.class, rl, subDir, true, true);
      // record the location of the main script that we'll be running
      if (SCRIPT_SPLITTER.equals(rl))
        scriptDbslicingPath = path;
    }
    return new CheckResult(true, "Assets unpacked OK.");
  }

  private CheckResult checkFraggerVer(String fraggerVer) {
    MsfraggerVerCmp cmp = new MsfraggerVerCmp();
    String minFraggerVer = MsfraggerProps.getProperties().getProperty(MsfraggerProps.PROP_MIN_VERSION_SLICING, "20180924");
    int fraggerVersionCmp = cmp.compare(fraggerVer, minFraggerVer);

    if (fraggerVersionCmp >= 0)
      return new CheckResult(true, "MSfragger: OK.");
    else
      return new CheckResult(false, "MSFragger: " + fraggerVer + ".");
  }
}
