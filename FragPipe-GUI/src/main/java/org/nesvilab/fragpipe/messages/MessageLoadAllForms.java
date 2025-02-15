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

import static org.nesvilab.fragpipe.Fragpipe.UI_STATE_CACHE_FN;

import java.nio.file.Path;
import org.nesvilab.utils.CacheUtils;

public class MessageLoadAllForms {

  public final Path path;

  public MessageLoadAllForms(Path path) {
    this.path = path;
  }

  public static MessageLoadAllForms newForCaching() {
    Path formCachePath = CacheUtils.getTempFile(UI_STATE_CACHE_FN);
    return new MessageLoadAllForms(formCachePath);
  }
}
