package com.dmtavt.fragpipe.tools.percolator;

import com.github.chhh.utils.JarUtils;
import com.github.chhh.utils.PathUtils;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@RunWith(JUnit4.class)
public class PercolatorOutputToPepXMLTest {
    private static List<Path> dirs = null;
    private static String fnPin = null;
    private static String fnBase = null;
    private static String fnPercolatorTargets = null;
    private static String fnPercolatorDecoys = null;
    private static String fnOutputBase = null;
    private static String dataType = null;
    private static double minProb = Double.NaN;

    @BeforeClass
    public static void setup() {
        Path testResourcesDir = Paths.get("test", "resources", "percolator-to-pepxml").toAbsolutePath();
        Path pFragger = testResourcesDir.resolve("fragpipe-search-16_PXD022287_msfragger");
        dirs = Stream.of(pFragger).filter(Files::exists).collect(Collectors.toList());
        fnPin = "Human-Protein-Training_Trypsin.pin";
        fnBase = PathUtils.removeExtension(fnPin, 2, 10);
        fnPercolatorTargets = fnBase + "_percolator_target_psms.tsv";
        fnPercolatorDecoys = fnBase + "_percolator_decoy_psms.tsv";
        fnOutputBase = "interact-" + fnBase;
        dataType = "DDA";
        minProb = 0.0;
    }

    @AfterClass
    public static void teardown() throws IOException {
        List<Path> found = new ArrayList<>();
        Predicate<File> test = file -> file.getName().startsWith("interact-");
        for (Path dir : dirs) {
            PathUtils.traverseDirectoriesAcceptingFiles(dir.toFile(), test, found, true);
        }
        for (Path path : found) {
            Files.deleteIfExists(path); // Don't delete the files if you want to take a look at the results
        }
    }

    @Test
    public void percolatorToPepXML() {
        for (Path dir : dirs) {
            System.out.printf("Testing %s in dir: %s\n", this.getClass().getSimpleName(), dir);
            PercolatorOutputToPepXML.percolatorToPepXML(
                    dir.resolve(fnPin),
                    dir.resolve(fnBase).toString(),
                    dir.resolve(fnPercolatorTargets),
                    dir.resolve(fnPercolatorDecoys),
                    dir.resolve(fnOutputBase),
                    dataType,
                    minProb
            );
            System.out.printf("\n========================\n\n", this.getClass().getSimpleName(), dir);
        }
    }
}