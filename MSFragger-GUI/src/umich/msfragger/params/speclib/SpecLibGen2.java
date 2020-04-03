package umich.msfragger.params.speclib;

import com.dmtavt.fragpipe.api.Bus;
import com.dmtavt.fragpipe.api.PyInfo;
import com.dmtavt.fragpipe.exceptions.ValidationException;
import com.dmtavt.fragpipe.messages.NoteConfigPython;
import com.dmtavt.fragpipe.messages.NoteConfigSpeclibgen;
import com.github.chhh.utils.Installed;
import com.github.chhh.utils.JarUtils;
import com.github.chhh.utils.PythonModule;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import org.greenrobot.eventbus.EventBus;
import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.ThreadMode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class SpecLibGen2 {

  public static final String DEFAULT_MESSAGE =
      "Python 3 with cython, msproteomicstools, matplotlib is "
          + "needed for Spectral Library Generation functionality.";
  public static final List<PythonModule> REQUIRED_MODULES = Arrays.asList(
      PythonModule.CYTHON,
      PythonModule.MSPROTEOMICSTOOLS,
      PythonModule.MATPLOTLIB);
  public static final List<PythonModule> REQUIRED_FOR_EASYPQP = Arrays.asList(
      new PythonModule("easypqp", "easypqp"));
  private static final Logger log = LoggerFactory.getLogger(SpecLibGen2.class);
  private static final String SCRIPT_SPEC_LIB_GEN = "/speclib/gen_con_spec_lib.py";
  public static final String[] RESOURCE_LOCATIONS = {
      "/speclib/common_funcs.py",
      "/speclib/detect_decoy_prefix.py",
      SCRIPT_SPEC_LIB_GEN,
      "/speclib/hela_irtkit.tsv",
      "/speclib/linux/spectrast",
      "/speclib/win/spectrast.exe",
      "/speclib/spectrast_gen_pepidx.py",
      "/speclib/unite_runs.py",
  };
  private static final String UNPACK_SUBDIR_IN_TEMP = "fragpipe";
  private static SpecLibGen2 INSTANCE = new SpecLibGen2();
  private final Object initLock = new Object();
  private PyInfo pi;
  private Path scriptSpecLibGenPath;
  private boolean isEasypqpOk;
  private boolean isInitialized;

  private SpecLibGen2() {
    pi = null;
    isEasypqpOk = false;
    scriptSpecLibGenPath = null;
    isInitialized = false;
  }

  public static SpecLibGen2 get() {
    return INSTANCE;
  }

  public static void initClass() {
    log.debug("Static initialization initiated");
    SpecLibGen2 o = new SpecLibGen2();
    Bus.register(o);
    SpecLibGen2.INSTANCE = o;
  }

  public boolean isEasypqpOk() {
    return isEasypqpOk;
  }

  @Subscribe(sticky = true, threadMode = ThreadMode.ASYNC)
  public void onNoteConfigPython(NoteConfigPython m) {
    if (!m.isValid()) {
      Bus.postSticky(
          new NoteConfigSpeclibgen(null, new ValidationException("Python binary not valid")));
      return;
    }
    try {
      init(m);
      Bus.postSticky(new NoteConfigSpeclibgen(get(), null));
    } catch (ValidationException e) {
      Bus.postSticky(new NoteConfigSpeclibgen(null, e));
    }
  }

  public PyInfo getPython() {
    return pi;
  }

  public Path getScriptSpecLibGenPath() {
    return scriptSpecLibGenPath;
  }

  public boolean isInitialized() {
    synchronized (initLock) {
      return isInitialized;
    }
  }

  public void init(NoteConfigPython python) throws ValidationException {
    synchronized (initLock) {
      isInitialized = false;

      checkPython(python);
      unpack();
      try {
        checkPythonEasypqp(python.pi);
      } catch (ValidationException e) { // easypqp is optional, don't fail whole validation if it's missing
        log.debug("EasyPQP init error", e);
        isEasypqpOk = false;
      }

      isInitialized = true;
      log.debug("{} init complete", SpecLibGen2.class.getSimpleName());
    }
  }

  private void checkPython(NoteConfigPython m) throws ValidationException {
    checkPythonVer(m);
    checkPythonModules(m.pi);
    this.pi = m.pi;
  }

  private void checkPythonVer(NoteConfigPython m) throws ValidationException {
    if (m.pi == null || !m.isValid() || m.pi.getMajorVersion() != 3) {
      throw new ValidationException("Requires Python version 3.x");
    }
  }

  private void checkPythonModules(PyInfo pi) throws ValidationException {
    Map<Installed, List<PythonModule>> modules = pi.modulesByStatus(REQUIRED_MODULES);
    final Map<Installed, String> bad = new LinkedHashMap<>();
    bad.put(Installed.NO, "Missing");
    bad.put(Installed.INSTALLED_WITH_IMPORTERROR, "Error loading module");
    bad.put(Installed.UNKNOWN, "N/A");

    if (modules.keySet().stream().anyMatch(bad::containsKey)) {
      final List<String> byStatus = new ArrayList<>();
      for (Installed status : bad.keySet()) {
        List<PythonModule> list = modules.get(status);
        if (list != null) {
          byStatus.add(bad.get(status) + " - " + list.stream().map(pm -> pm.installName)
              .collect(Collectors.joining(", ")));
        }
      }
      throw new ValidationException("Python modules: \n" + String.join("\n", byStatus));
    }
  }

  private void checkPythonEasypqp(PyInfo pi) throws ValidationException {
    Map<Installed, List<PythonModule>> modules = pi.modulesByStatus(REQUIRED_FOR_EASYPQP);
    if (modules.keySet().stream().anyMatch(installed -> Installed.YES != installed)) {
      throw new ValidationException("Missing Python EasyPQP module");
    }
    isEasypqpOk = true;
  }

  private void unpack() throws ValidationException {
    for (String rl : RESOURCE_LOCATIONS) {
      Path subDir = Paths.get(UNPACK_SUBDIR_IN_TEMP);
      Path path = null;
      try {
        path = JarUtils.unpackFromJar(SpecLibGen.class, rl, subDir, true, true);
      } catch (IOException e) {
        throw new ValidationException("Error unpacking resources", e);
      }
      if (SCRIPT_SPEC_LIB_GEN.equals(rl)) {
        scriptSpecLibGenPath = path;
      }
    }
  }
}
