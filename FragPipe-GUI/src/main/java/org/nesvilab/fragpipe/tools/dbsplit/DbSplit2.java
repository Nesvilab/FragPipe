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

package org.nesvilab.fragpipe.tools.dbsplit;

import org.nesvilab.fragpipe.FragpipeLocations;
import org.nesvilab.fragpipe.messages.MissingAssetsException;
import org.jooq.lambda.Seq;
import java.nio.file.Path;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Locator for the FragDbSplitter jar bundled under tools/. Since the jar is
 * always shipped with FragPipe, no availability checking or event-bus wiring
 * is required — just resolve the path on demand.
 */
public final class DbSplit2 {
  private static final Logger log = LoggerFactory.getLogger(DbSplit2.class);

  public static final String DBSPLIT_JAR_NAME = "FragDbSplitter-1.0.7.jar";

  private DbSplit2() {}

  /**
   * Locate the FragDbSplitter jar inside the bundled tools/ folder. Uses
   * {@link FragpipeLocations#tryLocateTools(java.util.stream.Stream)} so that
   * newer versioned jars (e.g. fragdbsplitter-1.1.0.jar) are automatically
   * picked up.
   *
   * @throws IllegalStateException if the jar cannot be located — this should
   *         never happen in a normal installation.
   */
  public static Path getScriptDbslicingPath() {
    try {
      List<Path> paths = FragpipeLocations.tryLocateTools(Seq.of(DBSPLIT_JAR_NAME));
      if (paths.isEmpty()) {
        throw new IllegalStateException("Could not locate FragDbSplitter jar " + DBSPLIT_JAR_NAME + " in tools/ folder");
      }
      return paths.get(0);
    } catch (MissingAssetsException e) {
      log.error("FragDbSplitter jar is missing from tools/ folder", e);
      throw new IllegalStateException("FragDbSplitter jar is missing from tools/ folder", e);
    }
  }
}
