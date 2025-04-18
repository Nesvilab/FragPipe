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

package org.nesvilab.fragpipe.messages;

import org.nesvilab.fragpipe.exceptions.ValidationException;
import java.util.StringJoiner;

public class NoteConfigIonQuant implements INoteConfig {

  public static String path;
  public static String version;
  public static String license;
  public static String customer;
  public static String mode;
  public static String expiryDate;
  public static boolean isValid;
  public static boolean isTooOld;
  public static Throwable ex;

  public NoteConfigIonQuant(String path, String version, String license, String customer, String mode, String expiryDate, boolean isValid, boolean isTooOld, Throwable ex) {
    NoteConfigIonQuant.path = path;
    NoteConfigIonQuant.version = version;
    NoteConfigIonQuant.license = license;
    NoteConfigIonQuant.customer = customer;
    NoteConfigIonQuant.mode = mode;
    NoteConfigIonQuant.expiryDate = expiryDate;
    NoteConfigIonQuant.isValid = isValid;
    NoteConfigIonQuant.isTooOld = isTooOld;
    NoteConfigIonQuant.ex = (path == null || path.trim().isEmpty() || version.trim().equalsIgnoreCase("n/a")) ? new ValidationException("IonQuant path or version does not exist.") : null;
  }

  @Override
  public boolean isValid() {
    return ex == null && !isTooOld && isValid && !path.contentEquals("N/A") && !version.contentEquals("N/A");
  }

  @Override
  public String toString() {
    return new StringJoiner(", ", NoteConfigIonQuant.class.getSimpleName() + "[", "]")
        .add("path='" + path + "'")
        .add("version='" + version + "'")
        .add("license='" + license + "'")
        .add("customer='" + customer + "'")
        .add("mode='" + mode + "'")
        .add("expiryDate='" + expiryDate + "'")
        .add("isTooOld=" + isTooOld)
        .add("validation=" + (ex == null ? "null" : ex.getMessage()))
        .toString();
  }
}
