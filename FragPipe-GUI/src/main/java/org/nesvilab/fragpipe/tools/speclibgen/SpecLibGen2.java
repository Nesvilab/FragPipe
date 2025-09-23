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

package org.nesvilab.fragpipe.tools.speclibgen;

import static org.nesvilab.fragpipe.tabs.TabConfig.pythonMinVersion;

import org.nesvilab.fragpipe.FragpipeLocations;
import org.nesvilab.fragpipe.api.Bus;
import org.nesvilab.fragpipe.api.PyInfo;
import org.nesvilab.fragpipe.exceptions.ValidationException;
import org.nesvilab.fragpipe.messages.MissingAssetsException;
import org.nesvilab.fragpipe.messages.NoteConfigPython;
import org.nesvilab.fragpipe.messages.NoteConfigSpeclibgen;
import org.nesvilab.utils.Installed;
import org.nesvilab.utils.PythonModule;
import java.nio.file.Path;
import java.nio.file.Paths;
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

  public static final List<PythonModule> REQUIRED_FOR_EASYPQP = Arrays.asList(PythonModule.EASYPQP, PythonModule.LXML);
  private static final Logger log = LoggerFactory.getLogger(SpecLibGen2.class);
  private static final String SCRIPT_SPEC_LIB_GEN = "speclib/gen_con_spec_lib.py";
  public static final String[] RESOURCE_LOCATIONS = {
      "speclib/common_funcs.py",
      "speclib/detect_decoy_prefix.py",
      SCRIPT_SPEC_LIB_GEN,
      "speclib/hela_irtkit.tsv",
      "speclib/Pierce_iRT.tsv",
      "speclib/unite_runs.py"
  };
  private final Object initLock = new Object();
  private PyInfo pi;
  private Path scriptSpecLibGenPath;
  private boolean isInitialized;

  private SpecLibGen2() {
    pi = null;
    scriptSpecLibGenPath = null;
    isInitialized = false;
  }

  public static void initClass() {
    log.debug("Static initialization initiated");
    SpecLibGen2 o = new SpecLibGen2();
    Bus.register(o);
  }

  public boolean isEasypqpOk() {
    synchronized (initLock) {
      return isInitialized;
    }
  }

  @Subscribe(sticky = true, threadMode = ThreadMode.MAIN_ORDERED)
  public void on(NoteConfigPython m) {
    if (!m.isValid()) {
      Bus.postSticky(
          new NoteConfigSpeclibgen(null, new ValidationException("Python binary not valid")));
      return;
    }
    try {
      init(m);
      Bus.postSticky(new NoteConfigSpeclibgen(this, null));
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
    if (m.pi == null || !m.isValid() || m.pi.getFullVersion().compareTo(pythonMinVersion) < 0) {
      throw new ValidationException("Requires Python version " + pythonMinVersion + "+");
    }
  }

  private List<PythonModule> checkForMissingModules(PyInfo pi, List<PythonModule> toCheck) {
    Map<Installed, List<PythonModule>> modules = pi.modulesByStatus(toCheck);
    return Seq.seq(modules).filter(kv -> Installed.YES != kv.v1)
        .flatMap(kv -> kv.v2.stream()).distinct().toList();
  }

  private void checkPythonSpeclibgen(PyInfo pi) throws ValidationException {
    List<PythonModule> missingModulesSpeclibgen = checkForMissingModules(pi, REQUIRED_FOR_EASYPQP);
    if (!missingModulesSpeclibgen.isEmpty()) {
      throw new ValidationException("Click 'Finish Python install' and wait.");
    }
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
