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

public class NoteConfigDiaTracer implements INoteConfig {

  public static String path;
  public static String version = "N/A";
  public static boolean isTooOld;
  public static boolean enabled;
  public static Throwable ex;

  public NoteConfigDiaTracer(String path, String version, boolean isTooOld, boolean enabled, Throwable ex) {
    NoteConfigDiaTracer.path = path;
    NoteConfigDiaTracer.version = version;
    NoteConfigDiaTracer.isTooOld = isTooOld;
    NoteConfigDiaTracer.enabled = enabled;
   if (ex == null) {
     if (path == null || path.trim().isEmpty() || version.trim().equalsIgnoreCase("n/a")) {
       NoteConfigDiaTracer.ex = new ValidationException("IonQuant path or version does not exist.");
     } else {
       NoteConfigDiaTracer.ex = null;
     }
   } else {
     NoteConfigDiaTracer.ex = ex;
   }
  }

  @Override
  public boolean isValid() {
    return !isTooOld && enabled && ex == null && path != null && !path.equalsIgnoreCase("N/A") && version != null && !version.equalsIgnoreCase("N/A") && !path.isEmpty() && !version.isEmpty();
  }

  @Override
  public String toString() {
    return new StringJoiner(", ", NoteConfigDiaTracer.class.getSimpleName() + "[", "]")
        .add("path='" + path + "'")
        .add("version='" + version + "'")
        .add("isTooOld=" + isTooOld)
        .add("enabled=" + enabled)
        .add("validation=" + (ex == null ? "null" : ex.getMessage()))
        .toString();
  }
}
