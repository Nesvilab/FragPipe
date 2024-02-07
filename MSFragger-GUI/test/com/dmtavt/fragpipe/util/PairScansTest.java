package com.dmtavt.fragpipe.util;

import org.junit.Ignore;
import org.junit.Test;

public class PairScansTest {

    @Test
    public void testPairScans() {
        String[] args = new String[] {
                "E:\\_Software_Tests\\DIA\\glycoDIA_HCD-ETD\\181217_Fusion_(LC2)_NewObj_Serum_deSA_Jacalin_HRM_4h_ETD_HCD_DDA_mz(400_1200).mzML",
                "11",
                "HCD",
                "ETD",
                "true",
                "false"};
        PairScans.main(args);
    }

}
