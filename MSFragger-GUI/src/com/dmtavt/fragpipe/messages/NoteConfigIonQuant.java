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

package com.dmtavt.fragpipe.messages;

import java.util.StringJoiner;

public class NoteConfigIonQuant implements INoteConfig {

  public static String path;
  public static String version;
  public static boolean isTooOld;
  public static boolean enabled;
  public static Throwable ex;

  public NoteConfigIonQuant(String path, String version, boolean isTooOld, boolean enabled, Throwable ex) {
    NoteConfigIonQuant.path = path;
    NoteConfigIonQuant.version = version;
    NoteConfigIonQuant.isTooOld = isTooOld;
    NoteConfigIonQuant.enabled = enabled;
    NoteConfigIonQuant.ex = ex;
  }

  @Override
  public boolean isValid() {
    return !isTooOld && enabled && ex == null && path != null && !path.contentEquals("N/A") && version != null && !version.contentEquals("N/A");
  }

  @Override
  public String toString() {
    return new StringJoiner(", ", NoteConfigIonQuant.class.getSimpleName() + "[", "]")
        .add("path='" + path + "'")
        .add("version='" + version + "'")
        .add("isTooOld=" + isTooOld)
        .add("enabled=" + enabled)
        .add("validation=" + (ex == null ? "null" : ex.getMessage()))
        .toString();
  }
}
