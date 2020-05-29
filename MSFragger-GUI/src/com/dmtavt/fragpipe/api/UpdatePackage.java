package com.dmtavt.fragpipe.api;

public class UpdatePackage {
  public final String downloadUrl;
  public final String propertyName;
  public final String description;
  public final String minVersion;
  public final String maxVersion;

  public UpdatePackage(String downloadUrl, String propertyName, String description,
      String minVersion, String maxVersion) {
    this.downloadUrl = downloadUrl;
    this.propertyName = propertyName;
    this.description = description;
    this.minVersion = minVersion;
    this.maxVersion = maxVersion;
  }
}
