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

import org.nesvilab.fragpipe.tools.tmtintegrator.QuantLabel;
import org.nesvilab.fragpipe.util.SDRFtable;

import java.nio.file.Path;
import java.util.List;

public class MessageSDRFsave {
    public final Path path;
    public final boolean quiet;
    public final SDRFtable.SDRFtypes type;
    public final QuantLabel label;
    public String logText;
    public List<String> proteinHeaders;

    public MessageSDRFsave() {
        path = null;
        quiet = false;
        type = null;
        label = null;
    }

    public MessageSDRFsave(Path path, boolean quiet, SDRFtable.SDRFtypes type, QuantLabel label, String logText, List<String> proteinHeaders) {
        this.path = path;
        this.quiet = quiet;
        this.type = type;
        this.label = label;
        this.logText = logText;
        this.proteinHeaders = proteinHeaders;
    }
}
