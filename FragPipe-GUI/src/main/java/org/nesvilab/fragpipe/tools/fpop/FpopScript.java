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

package org.nesvilab.fragpipe.tools.fpop;

import static org.nesvilab.fragpipe.tabs.TabConfig.pythonMinVersion;

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
import org.nesvilab.fragpipe.FragpipeLocations;
import org.nesvilab.fragpipe.api.Bus;
import org.nesvilab.fragpipe.api.PyInfo;
import org.nesvilab.fragpipe.exceptions.ValidationException;
import org.nesvilab.fragpipe.messages.MissingAssetsException;
import org.nesvilab.fragpipe.messages.NoteConfigFpopQuant;
import org.nesvilab.fragpipe.messages.NoteConfigPython;
import org.nesvilab.utils.Installed;
import org.nesvilab.utils.PythonModule;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class FpopScript {
    private static final Logger log = LoggerFactory.getLogger(FpopScript.class);
    private static FpopScript INSTANCE = new FpopScript();
    private final Object initLock = new Object();
    public static FpopScript get() { return INSTANCE; }

    public static final String FPOP_SCRIPT_PATH = "fpop/FragPipe_FPOP_Analysis.py";
    public static final String[] RESOURCE_LOCATIONS = {FPOP_SCRIPT_PATH};
    public static final List<PythonModule> REQUIRED_MODULES = Arrays.asList(PythonModule.NUMPY, PythonModule.PANDAS);

    private PyInfo pi;
    private Path scriptFpopQuant;
    private boolean isInitialized;

    /** To be called by top level application in order to initialize
     * the singleton and subscribe it to the bus. */
    public static void initClass() {
        log.debug("Static initialization initiated");
        FpopScript o = new FpopScript();
        Bus.register(o);
        FpopScript.INSTANCE = o;
    }

    private FpopScript() {
        pi = null;
        scriptFpopQuant = null;
        isInitialized = false;
    }

    @Subscribe(sticky = true, threadMode = ThreadMode.MAIN_ORDERED)
    public void on(NoteConfigPython m) {
        onPythonChange(m);
    }

    private void onPythonChange(NoteConfigPython python) {
        try {
            log.debug("Started init of: {}, python null={}", FpopScript.class.getSimpleName(), python == null);
            init(python);
            Bus.postSticky(new NoteConfigFpopQuant(this, null));
        } catch (ValidationException e) {
            Bus.postSticky(new NoteConfigFpopQuant(null, e));
        }
    }

    public Path getScriptFpopPath() {
        return scriptFpopQuant;
    }

    public PyInfo getPythonInfo() {
        return pi;
    }

    public boolean isInitialized() {
        synchronized (initLock) {
            return isInitialized;
        }
    }

    private void init(NoteConfigPython python) throws ValidationException {
        synchronized (initLock) {
            if (python == null || python.pi == null)
                throw new ValidationException("Python needs to be configured first.");
            isInitialized = false;

            checkPython(python);
            validateAssets();

            isInitialized = true;
            log.debug("{} init complete",FpopScript.class.getSimpleName());
        }
    }

    private void checkPython(NoteConfigPython m) throws ValidationException {
        checkPythonVer(m);
        checkPythonModules(m.pi);
        this.pi = m.pi;
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
                    byStatus.add(bad.get(status) + " - " + list.stream().map(pm -> pm.installName).collect(Collectors.joining(", ")));
                }
            }
            throw new ValidationException("Click 'Finish Python Install' and wait.");
        }

        this.pi = pi;
    }

    private void checkPythonVer(NoteConfigPython m) throws ValidationException {
        if (m.pi == null || m.pi.getFullVersion().compareTo(pythonMinVersion) < 0) {
            throw new ValidationException("Python version " + pythonMinVersion + "+ is required");
        }
    }

    private void validateAssets() throws ValidationException {
        try {
            List<Path> paths = FragpipeLocations.tryLocateTools(Seq.of(RESOURCE_LOCATIONS)
                    .map(loc -> loc.startsWith("/") ? loc.substring(1) : loc));
            final String scriptFpopFn = Paths.get(FPOP_SCRIPT_PATH).getFileName().toString();
            Optional<Path> mainScript = paths.stream()
                    .filter(p -> p.getFileName().toString().equalsIgnoreCase(scriptFpopFn))
                    .findFirst();
            if (!mainScript.isPresent()) {
                throw new ValidationException("Could not determine location of Fpop quant python script " + FPOP_SCRIPT_PATH);
            }
            scriptFpopQuant = mainScript.get();
        } catch (MissingAssetsException e) {
            log.error("Fpop quant script is missing assets in tools folder:\n{}", Seq.seq(e.getNotExisting()).toString("\n"));
            String missingRelativePaths = Seq.seq(e.getNotExisting())
                    .map(p -> FragpipeLocations.get().getDirTools().relativize(p))
                    .map(Path::toString).toString("; ");
            throw new ValidationException("Missing assets in tools/ folder:\n" + missingRelativePaths, e);
        }
    }

}
