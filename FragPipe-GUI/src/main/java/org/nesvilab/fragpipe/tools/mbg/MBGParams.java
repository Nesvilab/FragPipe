package org.nesvilab.fragpipe.tools.mbg;

public class MBGParams {
    private String residuesToAdd;
    private double maxGlycanQ;
    private int minPSMs;
    private double fdr;
    private int intGlycans;
    private int expandDB;
    private int maxSkips;

    public int getMaxSkips() { return maxSkips;}

    public void setMaxSkips(int maxSkips) { this.maxSkips = maxSkips;}

    public int getExpandDB() {
        return expandDB;
    }

    public void setExpandDB(int expandDB) {
        this.expandDB = expandDB;
    }

    public double getFdr() {
        return fdr;
    }

    public void setFdr(double fdr) {
        this.fdr = fdr;
    }

    public int getMinGlycans() {
        return intGlycans;
    }

    public void setIntGlycans(int intGlycans) {
        this.intGlycans = intGlycans;
    }

    public String getResiduesToAdd() {
        return residuesToAdd;
    }

    public void setResiduesToAdd(String glycanDB) {
        this.residuesToAdd = glycanDB;
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
