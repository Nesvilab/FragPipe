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

package org.nesvilab.fragpipe.util;

import java.util.Map;

public class GitHubJson implements Comparable<GitHubJson> {

  private String tagName;
  private String name;
  private String zipballUrl;
  private String tarballUrl;
  private Map<String, String> commit;
  private String nodeId;

  public GitHubJson() { }

  public int compareTo(GitHubJson other) {
    return name.compareTo(other.name);
  }

  public boolean equals(Object other) {
    if (other instanceof GitHubJson) {
      return name.contentEquals(((GitHubJson) other).name);
    } else {
      return false;
    }
  }

  public void setTagName(String tagName) {
    this.tagName = tagName;
  }

  public void setName(String name) {
    this.name = name;
  }

  public void setZipballUrl(String zipballUrl) {
    this.zipballUrl = zipballUrl;
  }

  public void setTarballUrl(String tarballUrl) {
    this.tarballUrl = tarballUrl;
  }

  public void setCommit(Map<String, String> commit) {
    this.commit = commit;
  }

  public void setNodeId(String nodeId) {
    this.nodeId = nodeId;
  }

  public String getTagName() {
    return tagName;
  }

  public String getName() {
    return name;
  }

  public String getZipballUrl() {
    return zipballUrl;
  }

  public String getTarballUrl() {
    return tarballUrl;
  }

  public Map<String, String> getCommit() {
    return commit;
  }

  public String getNodeId() {
    return nodeId;
  }
}
