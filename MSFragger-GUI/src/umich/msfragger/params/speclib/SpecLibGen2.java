package umich.msfragger.params.speclib;

import com.dmtavt.fragpipe.FragpipeLocations;
import com.dmtavt.fragpipe.api.Bus;
import com.dmtavt.fragpipe.api.PyInfo;
import com.dmtavt.fragpipe.exceptions.ValidationException;
import com.dmtavt.fragpipe.messages.MissingAssetsException;
import com.dmtavt.fragpipe.messages.NoteConfigPython;
import com.dmtavt.fragpipe.messages.NoteConfigSpeclibgen;
import com.github.chhh.utils.Installed;
import com.github.chhh.utils.PythonModule;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;
import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.ThreadMode;
import org.jooq.lambda.Seq;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class SpecLibGen2 {

  public static final String DEFAULT_MESSAGE =
      "Python 3 with cython, msproteomicstools, matplotlib is "
          + "needed for Spectral Library Generation functionality.";
  public static final List<PythonModule> REQUIRED_MODULES = Arrays.asList(
      PythonModule.CYTHON,
      PythonModule.MATPLOTLIB);
  public static final List<PythonModule> REQUIRED_FOR_EASYPQP = Arrays.asList(
      new PythonModule("easypqp", "easypqp"));
  private static final List<PythonModule> REQUIRED_FOR_SPECTRAST = Arrays.asList(PythonModule.MSPROTEOMICSTOOLS);
  private static final Logger log = LoggerFactory.getLogger(SpecLibGen2.class);
  private static final String SCRIPT_SPEC_LIB_GEN = "speclib/gen_con_spec_lib.py";
  public static final String[] RESOURCE_LOCATIONS = {
      "speclib/common_funcs.py",
      "speclib/detect_decoy_prefix.py",
      SCRIPT_SPEC_LIB_GEN,
      "speclib/hela_irtkit.tsv",
      "speclib/linux/spectrast",
      "speclib/win/spectrast.exe",
      "speclib/spectrast_gen_pepidx.py",
      "speclib/unite_runs.py",
  };
  private static final String UNPACK_SUBDIR_IN_TEMP = "fragpipe";
  private static SpecLibGen2 INSTANCE = new SpecLibGen2();
  private final Object initLock = new Object();
  private PyInfo pi;
  private Path scriptSpecLibGenPath;
  private boolean isEasypqpOk;
  private boolean isSpectrastOk;
  private boolean isInitialized;

  private SpecLibGen2() {
    pi = null;
    isEasypqpOk = false;
    isSpectrastOk = false;
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

  public boolean isSpectrastOk() {
    return isSpectrastOk;
  }

  @Subscribe(sticky = true, threadMode = ThreadMode.ASYNC)
  public void on(NoteConfigPython m) {
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
      validateAssets();
      try {
        checkPythonEasypqp(python.pi);
        isEasypqpOk = true;
      } catch (ValidationException e) { // easypqp is optional, don't fail whole validation if it's missing
        log.debug("EasyPQP init error", e);
        isEasypqpOk = false;
      }

      try {
        checkPythonSpectrast(python.pi);
        isSpectrastOk = true;
      } catch (ValidationException e) { // easypqp is optional, don't fail whole validation if it's missing
        log.debug("Spectrast init error", e);
        isSpectrastOk = false;
      }

      isInitialized = true;
      log.debug("{} init complete", SpecLibGen2.class.getSimpleName());
    }
  }

  private void checkPythonSpectrast(PyInfo pi) throws ValidationException {
    Map<Installed, List<PythonModule>> modules = pi.modulesByStatus(REQUIRED_FOR_SPECTRAST);
    if (modules.keySet().stream().anyMatch(installed -> Installed.YES != installed)) {
      throw new ValidationException("Missing Python Spectrast modules");
    }
    isSpectrastOk = true;
  }

  private void checkPython(NoteConfigPython m) throws ValidationException {
    checkPythonVer(m);
    checkPythonSpeclibgen(m.pi);
    this.pi = m.pi;
  }

  private void checkPythonVer(NoteConfigPython m) throws ValidationException {
    if (m.pi == null || !m.isValid() || m.pi.getMajorVersion() != 3) {
      throw new ValidationException("Requires Python version 3.x");
    }
  }

  private void checkPythonSpeclibgen(PyInfo pi) throws ValidationException {
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

  private void validateAssets() throws ValidationException {
    try {
      List<Path> paths = FragpipeLocations.createToolsPaths(Seq.of(RESOURCE_LOCATIONS)
          .map(loc -> loc.startsWith("/") ? loc.substring(1) : loc)); // just in case, for old style paths used in this class
      final String scriptSpeclibgenFn = Paths.get(SCRIPT_SPEC_LIB_GEN).getFileName().toString();
      Optional<Path> mainScript = paths.stream()
          .filter(p -> p.getFileName().toString().equalsIgnoreCase(scriptSpeclibgenFn))
          .findFirst();
      if (!mainScript.isPresent()) {
        throw new ValidationException("Could not determine location of SpecLibGen python script " + SCRIPT_SPEC_LIB_GEN);
      }
      scriptSpecLibGenPath = mainScript.get();

    } catch (MissingAssetsException e) {
      log.error("DbSplit is missing assets in tools folder:\n{}", Seq.seq(e.getNotExisting()).toString("\n"));
      String missingRelativePaths = Seq.seq(e.getNotExisting())
          .map(p -> FragpipeLocations.get().getDirTools().relativize(p))
          .map(Path::toString).toString("; ");
      throw new ValidationException("Missing assets in tools/ folder:\n" + missingRelativePaths, e);
    }
  }
}
