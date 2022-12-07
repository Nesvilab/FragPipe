package com.github.chhh.utils;

import org.junit.Assert;
import org.junit.Test;

public class PathUtilsTest {

    @Test
    public void removeExtensionTest() {
        final String fnBase = "file";
        final String[] dirBase = new String[] {"c:{}some{}path{}", "{}some{}path{}"};
        final String[] dirSeparators = new String[] {"/", "\\\\"};
        final int extLenMin = 1;
        final int extLenMax = 10;
        final int countExtsMax = 5;

        for (String dir : dirBase) {
            for (String sep : dirSeparators) {
                final String pathLessExt = dir.replaceAll("\\{}", sep) + fnBase;
                for (int extLen = extLenMin; extLen <= extLenMax; extLen++) {
                    for (int numExts = 1; numExts <= countExtsMax; numExts++) {
                        final String ext = "." + StringUtils.repeat("a", extLen);
                        final String extN = StringUtils.repeat(ext, numExts);
                        final String pathWithExt = pathLessExt + extN;
                        final String computedPathLessExt = PathUtils.removeExtension(pathWithExt, countExtsMax, extLenMax);
                        Assert.assertEquals(String.format("Wrong base path without extension, input: [%s]", pathWithExt),
                                pathLessExt, computedPathLessExt);
                    }
                }
            }
        }
    }
}
