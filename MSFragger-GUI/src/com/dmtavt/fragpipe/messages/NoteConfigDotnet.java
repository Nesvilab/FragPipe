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

import com.dmtavt.fragpipe.api.DotnetInfo;

public class NoteConfigDotnet  implements INoteConfig {
    public final DotnetInfo dotnetInfo;
    public final Throwable ex;
    public final String command;
    public final String version;


    public NoteConfigDotnet(DotnetInfo di) {
        this(di, null, di.getCommand(), di.getVersion());
    }

    public NoteConfigDotnet(DotnetInfo di, Throwable ex, String command, String version) {
        this.dotnetInfo = di;
        this.ex = ex;
        this.command = command;
        this.version = version;
    }


    @Override
    public boolean isValid() {
        return dotnetInfo != null && ex == null;
    }
}
