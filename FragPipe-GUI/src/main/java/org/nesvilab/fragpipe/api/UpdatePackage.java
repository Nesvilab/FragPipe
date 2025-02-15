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

import org.nesvilab.utils.StringUtils;
import java.util.Objects;
import java.util.StringJoiner;

public class UpdatePackage {
  public final String downloadUrl;
  public final String propertyName;
  public final String description;
  public final String minVersion;
  public final String maxVersion;

  public UpdatePackage(String downloadUrl, String propertyName, String description,
      String minVersion, String maxVersion) {
    Objects.requireNonNull(propertyName, "prop name can't be null");
    Objects.requireNonNull(downloadUrl, "download url can't be null");
    this.downloadUrl = downloadUrl;
    this.propertyName = propertyName;
    this.description = description;
    this.minVersion = minVersion;
    this.maxVersion = maxVersion;
  }

  public String getDescriptionOrName() {
    return StringUtils.isBlank(description) ? propertyName : description;
  }

  @Override
  public String toString() {
    return new StringJoiner(", ", UpdatePackage.class.getSimpleName() + "[", "]")
        .add("propertyName='" + propertyName + "'")
        .add("downloadUrl='" + downloadUrl + "'")
        .add("description='" + description + "'")
        .add("minVersion='" + minVersion + "'")
        .add("maxVersion='" + maxVersion + "'")
        .toString();
  }
}
