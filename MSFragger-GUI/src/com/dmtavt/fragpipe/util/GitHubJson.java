package com.dmtavt.fragpipe.util;

import java.util.Map;

public class GitHubJson implements Comparable<GitHubJson> {

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
