package com.dmtavt.fragpipe.tools.mbg;

public class MBGParams {
    private String residuesToAdd;
    private String residueDB;
    private double maxGlycanQ;
    private int minPSMs;
    private float fdr;

    public float getFdr() {
        return fdr;
    }

    public void setFdr(float fdr) {
        this.fdr = fdr;
    }

    public int getMinGlycans() {
        return intGlycans;
    }

    public void setIntGlycans(int intGlycans) {
        this.intGlycans = intGlycans;
    }

    private int intGlycans;

    public String getResiduesToAdd() {
        return residuesToAdd;
    }

    public void setResiduesToAdd(String glycanDB) {
        this.residuesToAdd = glycanDB;
    }

    public String getResidueDB() {
        return residueDB;
    }

    public void setResidueDB(String residueDB) {
        this.residueDB = residueDB;
    }

    public double getMaxGlycanQ() {
        return maxGlycanQ;
    }

    public void setMaxGlycanQ(double maxGlycanQ) {
        this.maxGlycanQ = maxGlycanQ;
    }

    public int getMinPSMs() {
        return minPSMs;
    }

    public void setMinPSMs(int minPSMs) {
        this.minPSMs = minPSMs;
    }

    public MBGParams() {

    }

}
