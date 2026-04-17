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

package org.nesvilab.fragpipe;

public final class NetworkFlags {

  // Master switch for outbound Internet calls made by FragPipe itself.
  // Set to false (and recompile) to disable:
  //   - Startup remote properties fetch (fragpipe.info)
  //   - Update checks for MSFragger, IonQuant, diaTracer
  // Does not affect user-initiated downloads, Transfer Learning / FragNovo (user-supplied URL),
  // browser-launch links, or network calls made by bundled external tools.
  //
  // Not declared final so unit tests can toggle it at runtime. Production code should
  // treat it as read-only; non-test code must never assign to it.
  public static boolean ENABLE_NETWORK = true;

  private NetworkFlags() {}
}
