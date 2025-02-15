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
package org.nesvilab.fragpipe.tools.crystalc;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.nesvilab.fragpipe.params.AbstractParams;
import org.nesvilab.fragpipe.params.Props;

/**
 * @author Dmitry Avtonomov
 */
public class CrystalcParams extends AbstractParams {

  public static final String PROP_thread = "thread";
  public static final String PROP_fasta = "fasta";
  public static final String PROP_raw_file_location = "raw_file_location";
  public static final String PROP_output_location = "output_location";
  public static final String PROP_raw_file_extension = "raw_file_extension";
  // precursor charge range for detecting chimeric spectra
  public static final String PROP_precursor_charge = "precursor_charge";
  public static final String PROP_isotope_number = "isotope_number";
  // precursor mass tolerance (unit: ppm)
  public static final String PROP_precursor_mass = "precursor_mass";
  public static final String PROP_precursor_isolation_window = "precursor_isolation_window";
  public static final String PROP_correct_isotope_error = "correct_isotope_error";

  public static final String CACHE_FILE = "crystalc.params";

  public CrystalcParams() {
    props = new Props();
  }

  @Override
  public Path tempFileName() {
    return Paths.get(CACHE_FILE);
  }

  @Override
  public void loadDefault() {
    try {
      InputStream is = CrystalcParams.class.getResourceAsStream(CACHE_FILE);
      props.load(is);
    } catch (IOException ex) {
      throw new IllegalStateException("Could not load CrystalC deafult properties from jar");
    }
  }

  public int getThread() {
    return getInt(PROP_thread, "-1");
  }

  public void setThread(int threads) {
    setInt(PROP_thread, threads);
  }

  public String getFasta() {
    return getString(PROP_fasta, "");
  }

  public void setFasta(String fastaPath) {
    setString(PROP_fasta, fastaPath);
  }

  public String getRawFileLocation() {
    return getString(PROP_raw_file_location, "");
  }

  public void setRawFileLocation(String rawDir) {
    setString(PROP_raw_file_location, rawDir);
  }

  public String getOutputLocation() {
    return getString(PROP_output_location, "");
  }

  public void setOutputLocation(String out) {
    setString(PROP_output_location, out);
  }

  public String getRawFileExt() {
    return getString(PROP_raw_file_extension, "mzML");
  }

  public void setRawFileExt(String rawFileExt) {
    setString(PROP_raw_file_extension, rawFileExt);
  }

  public int getMaxZ() {
    String val = getString(PROP_precursor_charge, "1 6");
    Pattern re = Pattern.compile("^\\d+$"); // old format, single number
    if (re.matcher(val).matches()) {
      val = "1 " + val;
    }

    Pattern re2 = Pattern.compile("^(\\d+)\\s+(\\d+)$");
    Matcher m = re2.matcher(val);
    if (!m.matches()) {
      throw new IllegalStateException(
          "CrystalC format for charge should be two numbers separated by whitespace.");
    }
    return Integer.parseInt(m.group(2));
  }

  public void setMaxZ(int z) {
    setString(PROP_precursor_charge, String.format("%d %d", 1, z));
  }

  public int getIsoNum() {
    return getInt(PROP_isotope_number, "3");
  }

  public void setIsoNum(int iso) {
    setInt(PROP_isotope_number, iso);
  }

  public double getPrecursorMassTol() {
    return getDouble(PROP_precursor_mass, "20");
  }

  public void setPrecursorMassTol(double tolPpm) {
    setDouble(PROP_precursor_mass, tolPpm);
  }

  public double getPrecursorIsolationWindow() {
    return getDouble(PROP_precursor_isolation_window, "0.7");
  }

  public void setPrecursorIsolationWindow(double window) {
    setDouble(PROP_precursor_isolation_window, window);
  }

  public void setCorrectIsotopeError(boolean correctIsotopeError) {
    setBool(PROP_correct_isotope_error, correctIsotopeError);
  }

  public void getCorrectIsotopeError(boolean correctIsotopeError) {
    getBoolean(PROP_correct_isotope_error, "false");
  }
}
