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

import com.dmtavt.fragpipe.tools.tmtintegrator.QuantLabel;
import com.dmtavt.fragpipe.util.SDRFtable;

import java.nio.file.Path;

public class MessageSDRFsave {
    public final Path path;
    public final boolean quiet;
    public final QuantLabel label;

    public MessageSDRFsave() {
        path = null;
        quiet = false;
        label = null;
    }

    public MessageSDRFsave(Path path, boolean quiet, QuantLabel label) {
        this.path = path;
        this.quiet = quiet;
        this.label = label;
    }
}
