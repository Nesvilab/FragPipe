package com.dmtavt.fragpipe.tools.fpop;

import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class EditDiannReportFPOPTest {

    @Test
    public void testEditDiannFPOP() throws Exception {
        FPOP_DiannReport_Editor testEditor  = new FPOP_DiannReport_Editor();
        double[] fpopMasses = new double[]{15.9949, 31.9898, 47.9847, 13.9793, -43.0534, -22.0320, -23.0159, -10.0319, 4.9735, -30.0106, -27.9949, -43.9898, -25.0316, -9.0367};

        String output1 = testEditor.getFpopMods("AD(129.00624)QLTEEQIAEFKEAFSLFDK", fpopMasses);
        assertEquals(output1, "2D(13.9793)");

        String output2 = testEditor.getFpopMods("A(UniMod:35)D(129.00624)QLTEEQIAEFKEAFSLFDK", fpopMasses);
        assertEquals(output2, "1A(15.9949),2D(13.9793)");

        String output3 = testEditor.getFpopMods("A(UniMod:35)DQLTEEQIAEFKEAFSLFDK", fpopMasses);
        assertEquals(output3, "1A(15.9949)");

        String output4 = testEditor.getFpopMods("A(87.0320)DQLTEEQIAEFKEAFSLFDK", fpopMasses);
        assertEquals(output4, "1A(15.9949)");

        String output5 = testEditor.getFpopMods("A(UniMod:35)DQLTE(UniMod:35)E(101.047693)Q(UniMod:35)IAEFK", fpopMasses);
        assertEquals(output5, "1A(15.9949),6E(15.9949),7E(-27.9949),8Q(15.9949)");

        String output6 = testEditor.getFpopMods("A(100.00)D(129.00624)QLTEEQIAEFKEAFSLFDK", fpopMasses);
        assertEquals(output6, "2D(13.9793)");

        String output7 = testEditor.getFpopMods("A(100.00)D(129.00624)QLTEEQ(100.00)IAEFKEAFSLFDK", fpopMasses);
        assertEquals(output7, "2D(13.9793)");

        String output8 = testEditor.getFpopMods("A(100.00)D(129.00624)QLTEEQ(100.00)IAEFKEAFS(UniMod:403)LFDK", fpopMasses);
        assertEquals(output8, "2D(13.9793)");
    }
}
