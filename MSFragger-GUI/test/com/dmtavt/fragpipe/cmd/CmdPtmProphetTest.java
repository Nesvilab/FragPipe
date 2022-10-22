package com.dmtavt.fragpipe.cmd;

import static org.junit.Assert.*;

import org.junit.Test;

public class CmdPtmProphetTest {

  @Test
  public void testTranslateCmds() {
    assertEquals("KEEPOLD STATIC EM=1 NIONS=b STY:79.966331,M:15.9949 MINPROB=0.5", CmdPtmProphet.translateCmds("--keepold --static --em 1 --nions b --mods STY:79.966331,M:15.9949 --minprob 0.5"));
    assertEquals("KEEPOLD STATIC EM=1 NIONS=b C:463.2366,C:467.2529,C:57.02416,M:15.9949 MINPROB=0.5", CmdPtmProphet.translateCmds("--keepold --static --em 1 --nions b --mods C:463.2366,C:467.2529,C:57.02416,M:15.9949 --minprob 0.5"));
    assertEquals("KEEPOLD STATIC FRAGPPMTOL=20 EM=1 SKREDTY:541.06111 MINPROB=0.5", CmdPtmProphet.translateCmds("--keepold --static --fragppmtol 20 --em 1 --mods SKREDTY:541.06111 --minprob 0.5"));
    assertEquals("KEEPOLD STATIC EM=1 NIONS=b K:114.04293,M:15.9949 MINPROB=0.5", CmdPtmProphet.translateCmds("--keepold --static --em 1 --nions b --mods K:114.04293,M:15.9949 --minprob 0.5"));
    assertEquals("KEEPOLD STATIC EM=1 NIONS=b STY:79.966331,n:42.0106,M:15.9949,K:4.025107,K:8.014199,R:6.020129,R:10.008269 MINPROB=0.5", CmdPtmProphet.translateCmds("--keepold --static --em 1 --nions b --mods STY:79.966331,n:42.0106,M:15.9949,K:4.025107,K:8.014199,R:6.020129,R:10.008269 --minprob 0.5"));
    assertEquals("KEEPOLD STATIC FRAGPPMTOL=200 EM=1 NIONS=b STY:79.966331,M:15.9949 MINPROB=0.5", CmdPtmProphet.translateCmds("--keepold --static --fragppmtol 200 --em 1 --nions b --mods STY:79.966331,M:15.9949 --minprob 0.5"));
    assertEquals("KEEPOLD STATIC EM=1 NIONS=b K:114.04293,M:15.9949 MINPROB=0.5", CmdPtmProphet.translateCmds("--keepold --static --em 1 --nions b --mods K:114.04293,M:15.9949 --minprob 0.5"));
    assertEquals("KEEPOLD STATIC EM=1 NIONS=b K:-114.04293,M:15.9949 MINPROB=0.5", CmdPtmProphet.translateCmds("--keepold --static --em 1 --nions b --mods K:-114.04293,M:15.9949 --minprob 0.5"));
  }

}