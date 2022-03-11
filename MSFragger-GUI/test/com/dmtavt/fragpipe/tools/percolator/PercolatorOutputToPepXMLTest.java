package com.dmtavt.fragpipe.tools.percolator;

import com.github.chhh.utils.PathUtils;
import org.junit.Test;

import java.nio.file.Path;
import java.nio.file.Paths;

import static org.junit.Assert.*;

public class PercolatorOutputToPepXMLTest {

    @Test
    public void percolatorToPepXML() {
//        final Path dir = Paths.get("C:\\Users\\DmitryAvtonomov\\ms-data\\bin-ma-2021_PXD022287\\fragpipe-search-14_PXD022287_comet");
        final Path dir = Paths.get("C:\\Users\\DmitryAvtonomov\\ms-data\\bin-ma-2021_PXD022287\\fragpipe-search-16_PXD022287_msfragger");
        final String fnPin = "Human-Protein-Training_Trypsin.pin";
        final String fnBase = PathUtils.removeExtension(fnPin, 2, 10);
        final String fnPercolatorTargets = fnBase + "_percolator_target_psms.tsv";
        final String fnPercolatorDecoys = fnBase + "_percolator_decoy_psms.tsv";
        final String fnOutputBase = "interact-" + fnBase;
        final String dataType = "DDA";
        final double minProb = 0.5;

        PercolatorOutputToPepXML.percolatorToPepXML(
                dir.resolve(fnPin),
                dir.resolve(fnBase).toString(),
                dir.resolve(fnPercolatorTargets),
                dir.resolve(fnPercolatorDecoys),
                dir.resolve(fnOutputBase),
                dataType,
                minProb
        );
    }
}