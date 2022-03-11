package com.dmtavt.fragpipe.tools.comet;

import com.github.chhh.utils.IOUtils;
import com.github.chhh.utils.IOUtilsTest;
import org.junit.Assert;
import org.junit.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

public class CometPinUpdateDecoyLabelTest {
    public static final String location = "percolator";
    public static final String fileWrong = "comet-sample.pin";
    public static final String fileCorrect = "comet-sample_correct.pin";

    @Test
    public void main() throws IOException {
        Path pathIn = null;
        Path pathCorrect = null;
        Path pathOut = null;
        try {
            pathIn = IOUtils.getResource(IOUtilsTest.class, location, fileWrong);
            pathCorrect = IOUtils.getResource(IOUtilsTest.class, location, fileCorrect);
            pathOut = Paths.get(pathIn.toString() + ".fixed");
        } catch (Exception e) {
            Assert.fail("IOUtils.getResource failed: " + e.getMessage());
        }

        CometPinUpdateDecoyLabel.main(new String[]{"rev_", pathIn.toString(), pathOut.toString()});
        List<String> linesOut = Files.readAllLines(pathOut);
        List<String> linesCorrect = Files.readAllLines(pathCorrect);
        Assert.assertEquals("Line counts differ", linesCorrect.size(), linesOut.size());
        for (int i = 0; i < linesOut.size(); i++) {
            String lOut = linesOut.get(i);
            String lCorrect = linesCorrect.get(i);
            Assert.assertEquals("Lines not equal", lOut, lCorrect);
        }
    }
}