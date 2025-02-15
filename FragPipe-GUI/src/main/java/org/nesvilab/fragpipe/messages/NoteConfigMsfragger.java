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

public class NoteConfigMsfragger implements INoteConfig {

  public final String path;
  public final String version;
  public final boolean isTooOld;
  public final Throwable ex;

  public NoteConfigMsfragger(String path, String version) {
    this(path, version, (path == null || path.trim().isEmpty() || version.trim().equalsIgnoreCase("n/a")) ? new ValidationException("MSFragger path or version does not exist.") : null);
  }

  public NoteConfigMsfragger(String path, String version, Throwable ex) {
    this(path, version, false, ex);
  }

  public NoteConfigMsfragger(String path, String version, boolean isTooOld, Throwable ex) {
    this.path = path;
    this.version = version;
    this.isTooOld = isTooOld;
    this.ex = ex;
  }

  @Override
  public boolean isValid() {
    return ex == null && path != null && version != null && !isTooOld && !path.isEmpty() && !version.isEmpty() && !path.trim().equalsIgnoreCase("n/a") && !version.trim().equalsIgnoreCase("n/a");
  }

  @Override
  public String toString() {
    return new StringJoiner(", ", NoteConfigMsfragger.class.getSimpleName() + "[", "]")
        .add("path='" + path + "'")
        .add("version='" + version + "'")
        .add("isTooOld=" + isTooOld)
        .add("validation=" + (ex == null ? "null" : ex.getMessage()))
        .toString();
  }
}
