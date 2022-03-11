package com.dmtavt.fragpipe.cmd;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.api.Bus;
import com.dmtavt.fragpipe.api.InputLcmsFile;
import com.dmtavt.fragpipe.messages.NoteConfigCometParams;
import com.dmtavt.fragpipe.messages.NoteConfigDatabase;
import com.dmtavt.fragpipe.tools.comet.CometParams;
import com.dmtavt.fragpipe.tools.comet.CometPinUpdateDecoyLabel;
import com.dmtavt.fragpipe.tools.enums.MassTolUnits;
import com.dmtavt.fragpipe.tools.enums.PrecursorMassTolUnits;
import com.dmtavt.fragpipe.tools.fragger.MsfraggerParams;
import com.github.chhh.utils.IOUtils;
import com.github.chhh.utils.StringUtils;
import com.github.chhh.utils.UsageTrigger;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.*;
import javax.swing.filechooser.FileFilter;
import java.awt.*;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.StringReader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.TreeMap;
import java.util.function.Predicate;
import java.util.regex.Pattern;

import static com.github.chhh.utils.PathUtils.testFilePath;

public class CmdComet extends CmdBase {
    private static final Logger log = LoggerFactory.getLogger(CmdComet.class);
    public static final String NAME = "Comet";
    private static final String[] COMET_DEPS = {"CometWrapper.dll"};
    public static final String COMET_BIN_WIN = "comet.win64.exe";
    public static final String COMET_BIN_LINUX = "comet.linux.exe";
    public static final String COMET_BIN_MACOS = "comet.macos.exe";
    private static final List<String> SUPPORTED_FORMATS = Arrays.asList("mzml", "mzxml");


    private static volatile FileFilter ff = null;
    private static volatile Predicate<File> supportedFilePredicate = null;
    private static final Path PATH_NONE = Paths.get("");
    private MsfraggerParams paramsDda;
    private MsfraggerParams paramsDia;
    private MsfraggerParams paramsGpfDia;

    public CmdComet(boolean isRun, Path workDir) {
        super(isRun, workDir);
    }

    @Override
    public String getCmdName() {
        return NAME;
    }

    private String getPepxmlFn(InputLcmsFile f, String ext, int rank) {
        if (rank > 0) {
            return StringUtils.upToLastDot(f.getPath().getFileName().toString()) + "_rank" + rank + "." + ext;
        } else {
            return StringUtils.upToLastDot(f.getPath().getFileName().toString()) + "." + ext;
        }
    }

    public Map<InputLcmsFile, List<Path>> outputs(List<InputLcmsFile> inputs, String ext, Path workDir) {
        Map<InputLcmsFile, List<Path>> m = new HashMap<>();
        for (InputLcmsFile f : inputs) {
            if (!f.getDataType().contentEquals("DDA")) {
                throw new IllegalArgumentException("Only DDA inputs are supported by Comet");
            }
            if (!f.getDataType().contentEquals("DDA") && !ext.contentEquals("tsv") && !ext.contentEquals("pin")) {
                int maxRank = 5;
                if (f.getDataType().contentEquals("DIA") || f.getDataType().contentEquals("DIA-Lib")) {
                    if (paramsDia == null) {
                        maxRank = 5; // The report_topN_rank is 5 by default for DIA data.
                    } else {
                        maxRank = paramsDia.getOutputReportTopN();
                    }
                } else if (f.getDataType().contentEquals("GPF-DIA")) {
                    if (paramsGpfDia == null) {
                        maxRank = 3; // The report_topN_rank is 3 by default for GPF-DIA data.
                    } else {
                        maxRank = paramsGpfDia.getOutputReportTopN();
                    }
                }

                for (int rank = 1; rank <= maxRank; ++rank) {
                    String pepxmlFn = getPepxmlFn(f, ext, rank);
                    List<Path> t = m.get(f);
                    if (t == null) {
                        t = new ArrayList<>();
                        t.add(f.outputDir(workDir).resolve(pepxmlFn));
                        m.put(f, t);
                    } else {
                        t.add(f.outputDir(workDir).resolve(pepxmlFn));
                    }
                }
            } else {
                String pepxmlFn = getPepxmlFn(f, ext, 0);
                List<Path> tempList = new ArrayList<>(1);
                tempList.add(f.outputDir(workDir).resolve(pepxmlFn));
                m.put(f, tempList);
            }
        }
        return m;
    }

    private static void showError(boolean isHeadless, String msg, Component comp) {
        if (isHeadless) {
            log.error(msg);
        } else {
            JOptionPane.showMessageDialog(comp, msg + "\n", "Error", JOptionPane.ERROR_MESSAGE);
        }
    }

    private static void showError(String msg, Component comp) {
        if (Fragpipe.headless) {
            log.error(msg);
        } else {
            JOptionPane.showMessageDialog(comp, msg + "\n", "Error", JOptionPane.ERROR_MESSAGE);
        }
    }

    private boolean checkDependencies(Path bin, List<String> deps, Component comp) {
        List<String> missing = new ArrayList<>();
        for (String dep : deps) {
            Path pathDep = bin.getParent().resolve(dep);
            if (!Files.exists(pathDep)) {
                missing.add(dep);
            }
        }
        if (missing.isEmpty())
            return true;
        showError("Comet is missing dependencies: " + String.join(", ", missing), comp);
        return false;
    }

    public boolean configure(Component comp, boolean isDryRun, Path jarFragpipe, UsageTrigger binComet, MsfraggerParams params,
                             String pathFasta, String cometParamsPath, int ramGb, List<InputLcmsFile> lcmsFiles,
                             final String decoyTag, boolean hasDda, boolean hasDia, boolean hasGpfDia, boolean hasDiaLib, boolean isRunDiaU) {

        initPreConfig();
        if (StringUtils.isNullOrWhitespace(binComet.getBin())) {
            showError("Binary for running Comet can not be an empty string.", comp);
            return false;
        }
        final Path pathBin = Paths.get(binComet.getBin());
        checkDependencies(pathBin, Arrays.asList(COMET_DEPS), comp);
        if (testFilePath(binComet.getBin(), "") == null) {
            showError("Binary for running Comet not found or could not be run.\nNeither on PATH, nor in the working directory", comp);
            return false;
        }

        boolean isOpenFormats = lcmsFiles.stream().map(lcms -> lcms.getPath().toString().toLowerCase(Locale.ROOT))
                .allMatch(f -> f.endsWith(".mzml") || f.endsWith(".mzxml"));
        if (!isOpenFormats) {
            showError("Comet only supports mzml and mzxml", comp);
            return false;
        }

        // Fasta file
        if (pathFasta == null) {
            showError("Fasta file path (Fragger) can't be empty", comp);
            return false;
        }

        if ((hasDia || hasGpfDia || hasDiaLib) && !isRunDiaU) {
            showError("Comet can't process DIA data natively, enable DIA-Umpire", comp);
            return false;
        }

        // Search parameter file
        final Path pathParamsOrig = Paths.get(cometParamsPath).toAbsolutePath();
        if (!pathParamsOrig.toFile().exists()) {
            showError("Comet .params file doesn't exist: " + cometParamsPath, comp);
            return false;
        }


        // copy + update the comet param file to the output dir
        final Path pathParamsActual = wd.resolve(pathParamsOrig.getFileName());
        if (pathParamsActual.toString().contains(" ")) {
            showError("Comet .params file path contains spaces.\n" +
                    "Please change output directory to one without spaces.", comp);
            return false;
        }
        List<String> paramsUpdatedLines;
        try {
            List<String> paramsOrigLines = Files.readAllLines(pathParamsOrig);
            paramsUpdatedLines = updateCometParamsContent(paramsOrigLines, decoyTag, pathFasta);
            if (!validateCometParamsContent(paramsUpdatedLines, comp))
                return false;
            if (!isDryRun) {
                IOUtils.updateFileContent(pathParamsActual, paramsUpdatedLines);
            }
        } catch (IOException e) {
            showError("Error updating Comet .params file: \n" + e.getMessage(), comp);
            return false;
        }

        //params.setDatabaseName(pathFasta); // we will pass it as a parameter instead
        params.setDecoyPrefix(decoyTag);
        Path savedDdaParamsPath = (hasDia || hasGpfDia || hasDiaLib) ? wd.resolve("fragger_dda.params") : wd.resolve("fragger.params");
        Path savedDiaParamsPath = wd.resolve("fragger_dia.params");
        Path savedGpfDiaParamsPath = wd.resolve("fragger_gpfdia.params");

        paramsDda = new MsfraggerParams(params);
        paramsDia = new MsfraggerParams(params);
        paramsGpfDia = new MsfraggerParams(params);

        paramsDda.setDataType(0);
        adjustDiaParams(params, paramsDia, "DIA");
        adjustDiaParams(params, paramsGpfDia, "GPF-DIA");

        // 32k symbols splitting for regular command.
        // But for slicing it's all up to the python script.
        // final int commandLenLimit = isSlicing ? Integer.MAX_VALUE : 32000;
        final int commandLenLimit = 32000; // Make is a little bit smaller than 1 << 15 to make sure that it won't crash.

        StringBuilder sb = new StringBuilder();

        Map<InputLcmsFile, List<Path>> mapLcmsToPepxml = outputs(lcmsFiles, "pep.xml", wd);
//        Map<InputLcmsFile, List<Path>> mapLcmsToTsv = outputs(lcmsFiles, "tsv", wd);
        Map<InputLcmsFile, List<Path>> mapLcmsToPin = outputs(lcmsFiles, "pin", wd);


        Map<String, List<InputLcmsFile>> t = new TreeMap<>();

        for (InputLcmsFile inputLcmsFile : lcmsFiles) {
            String key = inputLcmsFile.getDataType();
            if ("DIA-Lib".equals(key)) {
                key = "DIA"; // merge DIA-Lib and DIA keys
            }
            t.computeIfAbsent(key, k -> new ArrayList<>()).add(inputLcmsFile);
        }

        // read comet params file
        CometParams cometParams = new CometParams();
        if (!isDryRun) {
            try {
                cometParams.load(Files.newInputStream(pathParamsActual), true);
            } catch (FileNotFoundException ex) {
                showError("Comet params file does not exist: " + pathParamsActual, comp);
                return false;
            } catch (IOException ex) {
                showError("Could not read Comet params file", comp);
                return false;
            }
        } else {
            String paramsContent = String.join("\n", paramsUpdatedLines);
            try {
                cometParams.load(new ByteArrayInputStream(paramsContent.getBytes(StandardCharsets.UTF_8)), true);
            } catch (IOException e) {
                showError("Could not read Comet params file content from string", comp);
                return false;
            }
        }
        final String cometPepxmlOut = cometParams.getProps().getProp("output_pepxmlfile", "1").value;
        final boolean isOutputPepXml = "1".equals(cometPepxmlOut);
        if (!isOutputPepXml) {
            showError("Set output_pepxmlfile=1 in comet.params file", comp);
            return false;
        }
        final String cometPinOut = cometParams.getProps().getProp("output_percolatorfile", "1").value;
        final boolean isOutputPin = "1".equals(cometPinOut);
        if (!isOutputPin) {
            showError("Set output_percolatorfile=1 in comet.params file", comp);
            return false;
        }

        for (Map.Entry<String, List<InputLcmsFile>> e : t.entrySet()) {
            int fileIndex = 0;
            while (fileIndex < e.getValue().size()) {
                List<String> cmd = new ArrayList<>();
                cmd.add(binComet.useBin());
                cmd.add("-P" + pathParamsActual);
                cmd.add("-D" + pathFasta); // override fasta file path specified in params file
                // check if the command length is ok so far
                sb.append(String.join(" ", cmd));
                if (sb.length() > commandLenLimit) {
                    showError("Comet command line length too large even for a single file.", comp);
                    return false;
                }

                List<InputLcmsFile> addedLcmsFiles = new ArrayList<>();
                while (fileIndex < e.getValue().size()) {
                    InputLcmsFile f = e.getValue().get(fileIndex);
                    // if adding this file to the command line will make the command length
                    // longer than the allowed maximum, stop adding files
                    if (sb.length() + f.getPath().toString().length() > commandLenLimit) {
                        break;
                    }
                    sb.append(f.getPath().toString()).append(" ");
                    cmd.add(f.getPath().toString());
                    addedLcmsFiles.add(f);
                    fileIndex++;
                }

                ProcessBuilder pb = new ProcessBuilder(cmd);

                pb.directory(wd.toFile());

                pbis.add(PbiBuilder.from(pb));
                sb.setLength(0);


                // move the pepxml files if the output directory is not the same as where
                // the lcms files were
                for (InputLcmsFile lcms : addedLcmsFiles) {
                    if (isOutputPepXml)
                        createMoveCommands(mapLcmsToPepxml, lcms, "pepxml", jarFragpipe, pbis);
                    if (isOutputPin) {
                        Map<Path, Path> pinFiles = createMoveCommands(mapLcmsToPin, lcms, "pin", jarFragpipe, pbis);
                        for (Path pin : pinFiles.values()) {
                            pbPinUpdateLabel(jarFragpipe, decoyTag, pin, pbis);
                        }
                    }
                }
            }
        }

        isConfigured = true;
        return true;
    }

    private boolean validateCometParamsContent(List<String> lines, Component comp) throws IOException {
        NoteConfigDatabase confDb = Fragpipe.getStickyStrict(NoteConfigDatabase.class);
        final Pattern reEq = Pattern.compile("=");
        int decoy_search = -1;
        for (String line : lines) {
            final int commentStart = line.indexOf('#');
            final String content = commentStart > 0 ? line.substring(0, commentStart) : line;
            String[] split = reEq.split(content);
            if (split.length != 2) {
                continue;
            }
            final String k = split[0].trim();
            if ("decoy_search".equals(k)) {
                final int v = Integer.parseInt(split[1].trim());
                switch (v) {
                    case 0: {
                        if (confDb.decoysCnt == 0) {
                            showError("Comet params file has `decoy_search=0`, but there are NO decoys in DB.\n" +
                                    "To get FDR estimates either add decoys to DB or set `decoy_search=1`" +
                                    "in comet.params to automatically add them.", comp);
                            return false;
                        }
                        break;
                    }
                    case 1: {
                        if (confDb.decoysCnt > 0) {
                            showError("Comet params file has `decoy_search=1`, but there ARE decoys in DB.\n" +
                                    "The result will be a mess, set decoy_search=0` in comet.params to rely on existing decoys", comp);
                            return false;
                        }
                        break;
                    }
                    default: {
                        showError("Comet param `decoy_search` can only have value 0 or 1.\n" +
                                "Other values are not supported.", comp);
                        return false;
                    }
                }
                decoy_search = v;
            }
        }
        Bus.postSticky(new NoteConfigCometParams(decoy_search, confDb.decoysCnt > 0));
        return true;
    }

    private List<String> updateCometParamsContent(List<String> origContent, String decoyTag, String dbPath) throws IOException {
        List<String> updated = new ArrayList<>();
        final Pattern reEq = Pattern.compile("=");
        final Map<String, String> mapUpdatedValues = new HashMap<>();
        mapUpdatedValues.put("decoy_prefix", decoyTag);
        //mapUpdatedValues.put("decoy_search", "1"); // force Decoy search
        mapUpdatedValues.put("database_name", dbPath);
        for (String line : origContent) {
            final int commentStart = line.indexOf('#');
            final String content = commentStart > 0 ? line.substring(0, commentStart) : line;
            final String comment = commentStart > 0 ? line.substring(commentStart) : "";
            String[] split = reEq.split(content);
            if (split.length != 2) {
                updated.add(line);
                continue;
            }
            final String k = split[0].trim();
            String vUpdate  = mapUpdatedValues.get(k);
            if (vUpdate == null) {
                updated.add(line);
                continue;
            }
            updated.add(String.format("%s = %s %s", k, vUpdate, comment));
        }
        return updated;
    }

    /**
     * @return Map from original file location to new destination.
     */
    private Map<Path, Path> createMoveCommands(Map<InputLcmsFile, List<Path>> mapLcmsToPepxml, InputLcmsFile lcms, String fileTypeDesc,
                                    Path jarFragpipe, List<ProcessBuilderInfo> builders) {
        List<Path> expectedTargetPaths = mapLcmsToPepxml.get(lcms);
        if (expectedTargetPaths == null || expectedTargetPaths.isEmpty())
            throw new IllegalStateException(String.format("LCMS file mapped to no %s file", fileTypeDesc));
        Map<Path, Path> copyFromTo = new LinkedHashMap<>();
        for (Path expectedTargetPath : expectedTargetPaths) {
            final String sourceFn = expectedTargetPath.getFileName().toString();
            final String targetFn = sourceFn.replaceFirst("\\.pep\\.xml$", ".pepXML");
            final Path targetDir = expectedTargetPath.getParent();
            final Path sourcePath = lcms.getPath().getParent().resolve(sourceFn);
            final Path targetPath = targetDir.resolve(targetFn);
            if (sourcePath.equals(targetPath))
                continue;
            copyFromTo.put(sourcePath, targetPath);
        }

        // Comet move pep.xml, Comet move pin
        List<ProcessBuilder> pbsMove = ToolingUtils
                .pbsCopyMoveFiles(jarFragpipe, ToolingUtils.Op.MOVE, true, copyFromTo);
        builders.addAll(PbiBuilder.from(pbsMove, String.format("%s move %s", NAME, fileTypeDesc)));

        return copyFromTo;
    }

    private static void pbPinUpdateLabel(Path jarFragpipe, String decoyRegex, Path pathPinToFix, List<ProcessBuilderInfo> pbis) {
        NoteConfigCometParams conf = Fragpipe.getStickyStrict(NoteConfigCometParams.class);
        if (conf.decoy_search != 0 || !conf.dbHasDecoys) {
            log.debug("No PIN file update necessary. conf.decoy_search={}, conf.dbHasDecoys={}", conf.decoy_search, conf.dbHasDecoys);
            return; // no PIN updates necessary
        }

        if (jarFragpipe == null) {
            throw new IllegalArgumentException("jar can't be null");
        }
        List<ProcessBuilder> pbs = new ArrayList<>();
        Path pathPinFixed = Paths.get(pathPinToFix.toString() + ".fixed");
        List<String> cmd = ToolingUtils.cmdStubForJar(jarFragpipe);
        cmd.add(CometPinUpdateDecoyLabel.class.getCanonicalName());
        cmd.add(decoyRegex);
        cmd.add(pathPinToFix.toString());
        cmd.add(pathPinFixed.toString());
        pbs.add(new ProcessBuilder(cmd));

        // copy back and delete original
        final HashMap<Path, Path> fromTo = new HashMap<>();
        fromTo.put(pathPinFixed, pathPinToFix);
        pbs.addAll(ToolingUtils.pbsCopyMoveFiles(jarFragpipe, ToolingUtils.Op.MOVE, false, fromTo));

        pbis.addAll(PbiBuilder.from(pbs, String.format("%s update PIN files", NAME)));
    }

    private void adjustDiaParams(MsfraggerParams params, MsfraggerParams paramsNew, String dataType) {
        paramsNew.setReportAlternativeProteins(true);
        paramsNew.setShiftedIons(false);
        paramsNew.setLabileSearchMode("off");
        paramsNew.setDeltamassAllowedResidues("all");
        paramsNew.setRemovePrecursorPeak(0);
        if (paramsNew.getCalibrateMass() > 1) {
            paramsNew.setCalibrateMass(1);
        }
        paramsNew.setMassDiffToVariableMod(0);
        paramsNew.setIsotopeError("0");
        paramsNew.setMassOffsets("0");
        paramsNew.setUseTopNPeaks(300);
        paramsNew.setMinimumRatio(0);
        paramsNew.setIntensityTransform(1);

        if (dataType.contentEquals("DIA")) {
            paramsNew.setDataType(1);
            paramsNew.setOutputReportTopN(Math.max(5, params.getOutputReportTopN()));
            paramsNew.setPrecursorTrueUnits(MassTolUnits.PPM);
            paramsNew.setPrecursorTrueTolerance(10);
            if (params.getPrecursorMassUnits() == PrecursorMassTolUnits.PPM) {
                paramsNew.setPrecursorMassLower(Math.max(-10, params.getPrecursorMassLower()));
                paramsNew.setPrecursorMassUpper(Math.min(10, params.getPrecursorMassUpper()));
            } else {
                paramsNew.setPrecursorMassUnits(PrecursorMassTolUnits.PPM);
                paramsNew.setPrecursorMassLower(-10.0);
                paramsNew.setPrecursorMassUpper(10.0);
            }
        } else if (dataType.contentEquals("GPF-DIA")) {
            paramsNew.setDataType(2);
            paramsNew.setOutputReportTopN(Math.max(3, params.getOutputReportTopN()));
        }
    }

    public String getOutputFileExt() {
        return "pep.xml";
    }


    private static class GetSupportedExts {

        private List<Path> searchPaths;
        private List<String> desc;
        private List<String> exts;

        public GetSupportedExts(List<Path> searchPaths) {
            this.searchPaths = searchPaths;
        }

        public List<String> getDesc() {
            return desc;
        }

        public List<String> getExts() {
            return exts;
        }

        public GetSupportedExts invoke() {
            desc = new ArrayList<>(Arrays.asList("mzML", "mzXML"));
            exts = new ArrayList<>(Arrays.asList(".mgf", ".mzml"));
            return this;
        }
    }
}