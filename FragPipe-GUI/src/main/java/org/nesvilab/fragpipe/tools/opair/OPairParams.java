/*
 * This file is part of FragPipe.
 *
 * FragPipe is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * FragPipe is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with FragPipe. If not, see <https://www.gnu.org/licenses/>.
 */

package org.nesvilab.fragpipe.tools.opair;

public class OPairParams {
    private double productPPMtol;
    private double precursorPPMtol;
    private String OglycanDB;
    private int maxNumGlycans;
    private int minIsotope;
    private int maxIsotope;
    private boolean reverseScanOrder;
    private boolean singleScanType;
    private String activation1;
    private String activation2;
    private boolean filterOxonium;
    private String oxoRulesFilePath;
    private double oxoMinInt;
    private String allowedSites;

    public double getProductPPMtol() {
        return productPPMtol;
    }

    public void setProductPPMtol(double productPPMtol) {
        this.productPPMtol = productPPMtol;
    }

    public double getPrecursorPPMtol() {
        return precursorPPMtol;
    }

    public void setPrecursorPPMtol(double precursorPPMtol) {
        this.precursorPPMtol = precursorPPMtol;
    }

    public String getOglycanDB() {
        return OglycanDB;
    }

    public void setOglycanDB(String oglycanDB) {
        OglycanDB = oglycanDB;
    }

    public int getMaxNumGlycans() {
        return maxNumGlycans;
    }

    public void setMaxNumGlycans(int maxNumGlycans) {
        this.maxNumGlycans = maxNumGlycans;
    }

    public int getMinIsotope() { return minIsotope; }

    public void setMinIsotope(int minIsotope) { this.minIsotope = minIsotope; }

    public int getMaxIsotope() { return maxIsotope; }

    public void setMaxIsotope(int maxIsotope) { this.maxIsotope = maxIsotope; }

    public boolean isSingleScanType() {
        return singleScanType;
    }

    public void setSingleScanType(boolean singleScanType) {
        this.singleScanType = singleScanType;
    }

    public String getActivation1() {
        return activation1;
    }

    public void setActivation1(String activation1) {
        this.activation1 = activation1;
    }

    public String getActivation2() {
        return activation2;
    }

    public void setActivation2(String activation2) {
        this.activation2 = activation2;
    }

    public boolean isReverseScanOrder() {
        return reverseScanOrder;
    }

    public void setReverseScanOrder(boolean reverseScanOrder) {
        this.reverseScanOrder = reverseScanOrder;
    }

    public boolean isFilterOxonium() { return filterOxonium; }

    public void setFilterOxonium(boolean filterOxonium) { this.filterOxonium = filterOxonium; }

    public String getOxoRulesFilePath() {
        return oxoRulesFilePath;
    }

    public void setOxoRulesFilePath(String newPath) {
        oxoRulesFilePath = newPath;
    }

    public double getOxoMinInt() {
        return oxoMinInt;
    }

    public void setOxoMinInt(double minInt) {
        this.oxoMinInt = minInt;
    }

    public String getAllowedSites() {
        return allowedSites;
    }
    public void setAllowedSites(String sites) {
        allowedSites = sites;
    }

}
