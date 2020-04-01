package umich.msfragger.params.speclib;

import com.dmtavt.fragpipe.api.Bus;
import com.dmtavt.fragpipe.api.PyInfo;
import com.dmtavt.fragpipe.exceptions.ValidationException;
import com.dmtavt.fragpipe.messages.MessageEasypqpInit;
import com.dmtavt.fragpipe.messages.MessageToolInit;
import com.dmtavt.fragpipe.messages.NotePythonConfig;
import com.dmtavt.fragpipe.messages.NoteSpeclibgenConfig;
import com.github.chhh.utils.CheckResult;
import com.github.chhh.utils.Installed;
import com.github.chhh.utils.JarUtils;
import com.github.chhh.utils.PythonInfo;
import com.github.chhh.utils.PythonModule;
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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import umich.msfragger.params.speclib.SpecLibGen.MessageInitDone.REASON;

public class SpecLibGen2 {
//  private static final Logger log = LoggerFactory.getLogger(SpecLibGen2.class);
//  private static SpecLibGen2 instance = new SpecLibGen2();
//  private final Object initLock = new Object();
//
//  public static SpecLibGen2 get() {
//    return instance;
//  }
//  public static final String DEFAULT_MESSAGE = "Python 3 with cython, msproteomicstools, matplotlib is "
//      + "needed for Spectral Library generation functionality.";
//  private static final String SCRIPT_SPEC_LIB_GEN = "/speclib/gen_con_spec_lib.py";
//  private static final String UNPACK_SUBDIR_IN_TEMP = "fragpipe";
//
//  private PythonInfo pi;
//  private Path scriptSpecLibGenPath;
//  private boolean isInitialized;
//
//  private SpecLibGen2() {
//    scriptSpecLibGenPath = null;
//    EventBus.getDefault().register(this);
//  }
//
//  @Subscribe(sticky = true, threadMode = ThreadMode.ASYNC)
//  public void onPythonConfig(NotePythonConfig m) {
//    if (m.isValid()) {
//      init(m.pi);
//    }
//    Bus.postSticky(new NoteSpeclibgenConfig(null, new ValidationException("Python binary not valid")));
//  }
//
//  public static final String[] RESOURCE_LOCATIONS = {
//      "/speclib/common_funcs.py",
//      "/speclib/detect_decoy_prefix.py",
//      SCRIPT_SPEC_LIB_GEN,
//      "/speclib/hela_irtkit.tsv",
//      "/speclib/linux/spectrast",
//      "/speclib/win/spectrast.exe",
//      "/speclib/spectrast_gen_pepidx.py",
//      "/speclib/unite_runs.py",
//  };
//
//  public static final PythonModule[] REQUIRED_MODULES = {
//      new PythonModule("Cython", "Cython"),
//      new PythonModule("msproteomicstools", "msproteomicstoolslib"),
//      new PythonModule("matplotlib", "matplotlib"),
//  };
//  public static final PythonModule[] REQUIRED_FOR_EASYPQP = {
//      new PythonModule("easypqp", "easypqp"),
//  };
//
//  public PythonInfo getPi() {
//    return pi;
//  }
//
//  public Path getScriptSpecLibGenPath() {
//    return scriptSpecLibGenPath;
//  }
//
//  public static abstract class Message {
//    public final boolean append;
//    public final boolean isError;
//    public final String text;
//
//    public Message(boolean append, boolean isError, String text) {
//      this.append = append;
//      this.isError = isError;
//      this.text = text;
//    }
//
//    @Override
//    public String toString() {
//      return "Message{" +
//          "append=" + append +
//          ", isError=" + isError +
//          ", text='" + text + '\'' +
//          '}';
//    }
//  }
//
//  public static class Message1 extends MessageToolInit {
//    public Message1(boolean append, boolean isError, String text) {
//      super(append, isError, text);
//    }
//  }
//
//  public static class Message2 extends MessageToolInit {
//    public Message2(boolean append, boolean isError, String text) {
//      super(append, isError, text);
//    }
//  }
//
//  public static class MessageInitDone {
//    public enum REASON {NOT_UNPACKED, PY_VER, PY_MODULES, WRONG_FRAGGER}
//    public final boolean isSuccess;
//    public final EnumSet<REASON> reasons;
//
//    public MessageInitDone(boolean isSuccess, EnumSet<REASON> reasons) {
//      this.isSuccess = isSuccess;
//      this.reasons = reasons;
//    }
//  }
//
//  public boolean isInitialized() {
//    synchronized (initLock) {
//      return isInitialized;
//    }
//  }
//
//  public void init(PyInfo pi) throws ValidationException {
//    synchronized (initLock) {
//      EnumSet<REASON> reasons = EnumSet.noneOf(REASON.class);
//
//      Map<String, List<String>> errors = new HashMap<>();
//      // check python version
//      final String catPython = "python";
//      CheckResult checkPython = CheckResult.newFail(catPython, "init default");
//      try {
//        checkPython = checkPythonVer();
//
//        EventBus.getDefault().post(new Message1(true, !checkPython.ok(), checkPython.message));
//        if (!checkPython.ok())
//          reasons.add(REASON.PY_VER);
//      } catch (Exception e) {
//        EventBus.getDefault().post(new Message1(true, true, "Error checking python version."));
//        reasons.add(REASON.PY_VER);
//      }
//
//      // check ok/installed modules
//      final String catModules = "modules";
//      CheckResult checkModules = CheckResult.newFail(catModules, "init default");
//      if (checkPython.ok()) {
//        try {
//          checkModules = checkPythonOkModules();
//          EventBus.getDefault().post(new Message1(true, false, res.message));
//          if (!checkModules.ok()) {
//            reasons.add(REASON.PY_MODULES);
//          }
//        } catch (Exception ex) {
//          EventBus.getDefault()
//              .post(new Message1(true, true, "Error checking installed/ok python modules."
//              ));
//          reasons.add(REASON.PY_MODULES);
//        }
//      }
//
//      // check missing/error modules
//      boolean isNoErrorModulesSpectrast = false;
//      boolean isNoErrorModulesEasyPQP = false;
//      if (!checkPython.ok()) {
//        EventBus.getDefault().postSticky(new MessageEasypqpInit(false, false, "Python incompatible"));
//      } else {
//        try {
//          CheckResult res = checkPythonErrorModules();
//          isNoErrorModulesSpectrast = res.isSuccess;
//          EventBus.getDefault().post(new Message1(true, !isNoErrorModulesSpectrast, res.message));
//          if (!res.isSuccess) {
//            reasons.add(REASON.PY_MODULES);
//          }
//        } catch (Exception ex) {
//          EventBus.getDefault()
//              .post(new Message1(true, true, "Error checking installed python modules."
//              ));
//          reasons.add(REASON.PY_MODULES);
//        }
//
//        // check EasyPqp installation separately
//        CheckResult result = checkPythonErrorModulesEasypqp();
//        isNoErrorModulesEasyPQP = result.isSuccess;
//        if (result.isSuccess) {
//          EventBus.getDefault().postSticky(new MessageEasypqpInit(true, true, null));
//        } else {
//          EventBus.getDefault().postSticky(new MessageEasypqpInit(true, false, result.message));
//        }
//      }
//      final boolean isNoErrorModules = isNoErrorModulesSpectrast || isNoErrorModulesEasyPQP;
//      boolean isUnpacked = false;
//      if (isNoErrorModules) {
//        try {
//          CheckResult res = unpack();
//          isUnpacked = res.isSuccess;
//          EventBus.getDefault().post(new Message1(true, !res.isSuccess, res.message));
//          if (!res.isSuccess) {
//            reasons.add(REASON.NOT_UNPACKED);
//          }
//        } catch (Exception e) {
//          EventBus.getDefault().post(new Message1(true, true, "Error unpacking necessary tools."));
//          reasons.add(REASON.NOT_UNPACKED);
//        }
//      }
//
//      final boolean isInitSuccess = isPythonOk && isNoErrorModules && isUnpacked;
//      final boolean isInitSuccessSpectrast = isPythonOk && isNoErrorModulesSpectrast && isUnpacked;
//      isInitialized = isInitSuccess;
//      EventBus.getDefault().postSticky(new MessageInitDone(isInitSuccessSpectrast, reasons));
//    }
//  }
//
//  private CheckResult checkPythonVer() throws Exception {
//    if (!pi.isInitialized())
//      pi.findPythonCommand();
//    if (!pi.isInitialized()) {
//      return new CheckResult(false, "Python not found.");
//    } else if (pi.getMajorVersion() != 3) {
//      return new CheckResult(false, "Python: " + pi.getVersion() + ".");
//    }
//    return new CheckResult(true, "Python: " + pi.getVersion() + ".");
//  }
//
//  private List<PythonModule> createPythonModulesStatusList(Installed installedStatus, PythonModule[] modules) {
//    return Arrays.stream(modules)
//        .filter(pm -> installedStatus.equals(pi.checkModuleInstalled(pm)))
//        .collect(Collectors.toList());
//  }
//
//  private CheckResult checkPythonOkModules() {
//    StringBuilder sb = new StringBuilder("Modules:");
//    if (REQUIRED_MODULES.length == 0) {
//      sb.append(" none required");
//    } else {
//      List<PythonModule> okMods = createPythonModulesStatusList(Installed.YES, REQUIRED_MODULES);
//      if (!okMods.isEmpty()) {
//        String pmList = okMods.stream().map(pm -> pm.installName).collect(Collectors.joining(", "));
//        sb.append(" ").append(pmList).append(" - OK;");
//      }
//    }
//    return new CheckResult(true, sb.toString());
//  }
//
//  private CheckResult checkPythonErrorModules() {
//    List<Installed> badStatuses = Arrays.stream(Installed.values())
//        .filter(installed -> !installed.equals(Installed.YES)).collect(
//            Collectors.toList());
//
//    final Map<Installed, String> statusNameMap = new HashMap<>();
//    statusNameMap.put(Installed.INSTALLED_WITH_IMPORTERROR, "Error loading module");
//    statusNameMap.put(Installed.NO, "Missing");
//    statusNameMap.put(Installed.UNKNOWN, "N/A");
//
//    final List<String> badModsByStatus = new ArrayList<>();
//    for (Installed badStatus : badStatuses) {
//      List<PythonModule> badMods = createPythonModulesStatusList(badStatus, REQUIRED_MODULES);
//      if (!badMods.isEmpty()) {
//        StringBuilder sb = new StringBuilder();
//        sb.append(badMods.stream().map(pm -> pm.installName).collect(Collectors.joining(", ")))
//            .append(" - ").append(statusNameMap.getOrDefault(badStatus, badStatus.name()));
//        badModsByStatus.add(sb.toString());
//      }
//    }
//    if (badModsByStatus.isEmpty()) {
//      return new CheckResult(true, "");
//    }
//    return new CheckResult(false, " " + String.join(", ", badModsByStatus));
//  }
//
//  public CheckResult checkPythonErrorModulesEasypqp() {
//    List<Installed> badStatuses = Arrays.stream(Installed.values())
//        .filter(installed -> !installed.equals(Installed.YES)).collect(
//            Collectors.toList());
//
//    final Map<Installed, String> statusNameMap = new HashMap<>();
//    statusNameMap.put(Installed.INSTALLED_WITH_IMPORTERROR, "Error loading module");
//    statusNameMap.put(Installed.NO, "Missing");
//    statusNameMap.put(Installed.UNKNOWN, "N/A");
//
//    final List<String> badModsByStatus = new ArrayList<>();
//    for (Installed badStatus : badStatuses) {
//      List<PythonModule> badMods = createPythonModulesStatusList(badStatus, REQUIRED_FOR_EASYPQP);
//      if (!badMods.isEmpty()) {
//        StringBuilder sb = new StringBuilder();
//        sb.append(badMods.stream().map(pm -> pm.installName).collect(Collectors.joining(", ")))
//            .append(" - ").append(statusNameMap.getOrDefault(badStatus, badStatus.name()));
//        badModsByStatus.add(sb.toString());
//      }
//    }
//    if (badModsByStatus.isEmpty()) {
//      return new CheckResult(true, "");
//    }
//    return new CheckResult(false, " " + String.join(", ", badModsByStatus));
//  }
//
//  private CheckResult unpack() throws Exception {
//    for (String rl : RESOURCE_LOCATIONS) {
//      Path subDir = Paths.get(UNPACK_SUBDIR_IN_TEMP);
//      Path path = JarUtils.unpackFromJar(SpecLibGen.class, rl, subDir, true, true);
//      if (SCRIPT_SPEC_LIB_GEN.equals(rl))
//        scriptSpecLibGenPath = path;
//    }
//    return new CheckResult(true, "Assets unpacked OK.");
//  }
}
