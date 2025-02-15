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
package org.nesvilab.fragpipe.api;

import java.nio.file.Path;

/**
 *
 * @author Dmitry Avtonomov
 */
public interface VersionFetcher {
    /**
     * The current version of the tool.
     * @return Arbitrary string representation of the version, e.g. "v5.4.1" or "Release 6.1" etc.
     * @throws java.lang.Exception For any situation when a version can't be fetched.
     */
    String fetchVersion() throws Exception;
    /**
     * This method should return non-null only in case {@link #fetchVersion()}
     * was called successfully.
     */
    String getDownloadUrl();
    /**
     * Name of the tool for which the version if fetched.
     */
    String getToolName();
    
    boolean canAutoUpdate();
    
    /**
     * Automatically update file, deleting the older version.
     * @param p the file to be updated and deleted in case of successful update.
     * @return Path of the updated file or null in case update didn't happen.
     * @throws java.lang.Exception If can't download new version or whatever else.
     */
    Path autoUpdate(Path p) throws Exception;
}
