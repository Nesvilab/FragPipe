package com.dmtavt.fragpipe.tools.mbg;

public class MBGParams {
    private String glycanDB;
    private String residueDB;
    private double maxGlycanQ;
    private int minPSMs;

    public int getMinGlycans() {
        return intGlycans;
    }

    public void setIntGlycans(int intGlycans) {
        this.intGlycans = intGlycans;
    }

    private int intGlycans;

    public String getGlycanDB() {
        return glycanDB;
    }

    public void setGlycanDB(String glycanDB) {
        this.glycanDB = glycanDB;
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
