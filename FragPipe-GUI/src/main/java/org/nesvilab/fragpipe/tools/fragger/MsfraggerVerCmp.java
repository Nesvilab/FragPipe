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

package org.nesvilab.fragpipe.tools.fragger;

import java.util.Comparator;
import java.util.regex.Pattern;
import org.nesvilab.utils.VersionComparator;

public class MsfraggerVerCmp implements Comparator<String> {

  public static Pattern regex = Pattern.compile("MSFragger-(Commercial-)?(\\d+\\.\\d+[a-z0-9.-]*).jar", Pattern.CASE_INSENSITIVE);
  public static Pattern regex2 = Pattern.compile("MSFragger-(Commercial-)?(\\d+\\.\\d+[a-z0-9.-]*)", Pattern.CASE_INSENSITIVE);

  static final VersionComparator VER_CMP = new VersionComparator();
  private static final MsfraggerVerCmp INSTANCE = new MsfraggerVerCmp();

  public static MsfraggerVerCmp get() {
    return INSTANCE;
  }

  @Override
  public int compare(String v1, String v2) {
    return VER_CMP.compare(v1, v2);
  }
}
