/*
 * Copyright 2018 Dmitry Avtonomov.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package umich.msfragger.gui.api;

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
