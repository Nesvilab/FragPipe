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

package com.dmtavt.fragpipe.messages;

import java.util.StringJoiner;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;

public class NoteConfigSkyline implements INoteConfig {

  public final String path;
  public final String version;
  public final Throwable ex;
  private final boolean isValid;
  private final DefaultArtifactVersion versionObj;

  public NoteConfigSkyline(String path, String version, Throwable ex, boolean isValid) {
    this.path = path;
    this.version = version;
    this.ex = ex;
    this.isValid = isValid;
    if (version != null) {
      this.versionObj = new DefaultArtifactVersion(version);
    } else {
      this.versionObj = null;
    }
  }

  public NoteConfigSkyline(Throwable ex) {
    this.path = null;
    this.version = null;
    this.ex = ex;
    this.isValid = false;
    this.versionObj = null;
  }

  @Override
  public boolean isValid() {
    return isValid && ex == null && version != null && path != null && !version.isEmpty() && !path.isEmpty() && !version.trim().equalsIgnoreCase("n/a") && !path.trim().equalsIgnoreCase("n/a");
  }

  @Override
  public String toString() {
    return new StringJoiner(", ", NoteConfigSkyline.class.getSimpleName() + "[", "]")
        .add("path='" + path + "'")
        .add("version='" + version + "'")
        .add("validation=" + (ex == null ? "null" : ex.getMessage()))
        .toString();
  }

  public int compareVersion(String other) {
    if (versionObj == null && other == null) {
      return 0;
    } else if (versionObj == null) {
      return -1;
    } else if (other == null) {
      return 1;
    } else {
      return versionObj.compareTo(new DefaultArtifactVersion(other));
    }
  }
}
