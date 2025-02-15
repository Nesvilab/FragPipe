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

import java.util.StringJoiner;

public class NoteFragpipeUpdate {

  public final String releaseVer;
  public final String downloadUrl;
  public final String announcement;

  public NoteFragpipeUpdate(String releaseVer, String downloadUrl, String announcement) {
    this.releaseVer = releaseVer;
    this.downloadUrl = downloadUrl;
    this.announcement = announcement;
  }

  @Override
  public String toString() {
    return new StringJoiner(", ", NoteFragpipeUpdate.class.getSimpleName() + "[", "]")
        .add("releaseVer='" + releaseVer + "'")
        .add("downloadUrl='" + downloadUrl + "'")
        .add("announcement='" + announcement + "'")
        .toString();
  }
}
