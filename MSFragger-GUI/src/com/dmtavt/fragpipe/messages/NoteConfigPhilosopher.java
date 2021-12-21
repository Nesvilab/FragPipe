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

public class NoteConfigPhilosopher implements INoteConfig {
  public final String path;
  public final String version;
  public final Throwable ex;

  public NoteConfigPhilosopher(String path, String version) {
    this(path, version, null);
  }

  public NoteConfigPhilosopher(String path, String version, Throwable ex) {
    this.path = path;
    this.version = version;
    this.ex = ex;
  }

  @Override
  public boolean isValid() {
    return ex == null && version != null && path != null;
  }

  @Override
  public String toString() {
    return new StringJoiner(", ", NoteConfigPhilosopher.class.getSimpleName() + "[", "]")
        .add("path='" + path + "'")
        .add("version='" + version + "'")
        .add("validation=" + (ex == null ? "null" : ex.getMessage()))
        .toString();
  }
}
