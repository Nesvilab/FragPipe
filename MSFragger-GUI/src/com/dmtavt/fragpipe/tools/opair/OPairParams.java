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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with FragPipe.  If not, see <https://www.gnu.org/licenses/>.
 */

package com.dmtavt.fragpipe.tools.opair;

public class OPairParams {
    private double productPPMtol;
    private double precursorPPMtol;
    private String OglycanDB;
    private int maxNumGlycans;
    private int minIsotope;
    private int maxIsotope;


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
}
