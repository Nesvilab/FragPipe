package com.dmtavt.fragpipe.tools.speclibgen;

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
import java.util.List;
import java.util.Map;
import java.util.Optional;
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
  public static final List<PythonModule> REQUIRED_FOR_EASYPQP = Arrays.asList(PythonModule.EASYPQP);
  private static final Logger log = LoggerFactory.getLogger(SpecLibGen2.class);
  private static final String SCRIPT_SPEC_LIB_GEN = "speclib/gen_con_spec_lib.py";
  public static final String[] RESOURCE_LOCATIONS = {
      "speclib/common_funcs.py",
      "speclib/detect_decoy_prefix.py",
      SCRIPT_SPEC_LIB_GEN,
      "speclib/hela_irtkit.tsv",
      "speclib/Pierce_iRT.tsv",
      "speclib/unite_runs.py",
  };
  private static final String UNPACK_SUBDIR_IN_TEMP = "fragpipe";
  private static SpecLibGen2 INSTANCE = new SpecLibGen2();
  private final Object initLock = new Object();
  private PyInfo pi;
  private Path scriptSpecLibGenPath;
  public List<PythonModule> missingModulesEasyPqp;
  public List<PythonModule> missingModulesSpeclibgen;
  private boolean isEasypqpOk;
  private boolean isInitialized;

  private SpecLibGen2() {
    pi = null;
    isEasypqpOk = false;
    scriptSpecLibGenPath = null;
    isInitialized = false;
    missingModulesSpeclibgen = new ArrayList<>();
    missingModulesEasyPqp = new ArrayList<>();
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
    synchronized (initLock) {
      return isInitialized && isEasypqpOk;
    }
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

      missingModulesEasyPqp = checkPythonEasypqp(python.pi);
      isEasypqpOk = missingModulesEasyPqp.isEmpty();

      isInitialized = true;
      log.debug("{} init complete", SpecLibGen2.class.getSimpleName());
    }
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

  private List<PythonModule> checkForMissingModules(PyInfo pi, List<PythonModule> toCheck) {
    Map<Installed, List<PythonModule>> modules = pi.modulesByStatus(toCheck);
    return Seq.seq(modules).filter(kv -> Installed.YES != kv.v1)
        .flatMap(kv -> kv.v2.stream()).distinct().toList();
  }

  private void checkPythonSpeclibgen(PyInfo pi) throws ValidationException {
    missingModulesSpeclibgen = checkForMissingModules(pi, REQUIRED_MODULES);
    if (!missingModulesSpeclibgen.isEmpty()) {
      throw new ValidationException("Python modules missing:\n" + Seq.seq(missingModulesSpeclibgen).map(pm -> pm.installName).toString("\n"));
    }
  }

  /** @return List of missing modules. */
  private List<PythonModule> checkPythonEasypqp(PyInfo pi) {
    return checkForMissingModules(pi, REQUIRED_FOR_EASYPQP);
  }

  private void validateAssets() throws ValidationException {
    try {
      List<Path> paths = FragpipeLocations.tryLocateTools(Seq.of(RESOURCE_LOCATIONS)
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
      log.error("SpecLib is missing assets in tools folder:\n{}", Seq.seq(e.getNotExisting()).toString("\n"));
      String missingRelativePaths = Seq.seq(e.getNotExisting())
          .map(p -> FragpipeLocations.get().getDirTools().relativize(p))
          .map(Path::toString).toString("; ");
      throw new ValidationException("Missing assets in tools/ folder:\n" + missingRelativePaths, e);
    }
  }
}
