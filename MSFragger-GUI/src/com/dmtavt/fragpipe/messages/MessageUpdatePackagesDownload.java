package com.dmtavt.fragpipe.messages;

import com.dmtavt.fragpipe.api.UpdatePackage;
import java.util.List;

public class MessageUpdatePackagesDownload {

  public final List<UpdatePackage> updates;

  public MessageUpdatePackagesDownload(List<UpdatePackage> updates) {
    this.updates = updates;
  }
}
