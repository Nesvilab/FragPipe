package com.dmtavt.fragpipe.messages;

import com.dmtavt.fragpipe.api.UpdatePackage;
import java.util.List;

public class MessageUpdatePackagesAvailable {

  final List<UpdatePackage> updates;

  public MessageUpdatePackagesAvailable(List<UpdatePackage> updates) {
    this.updates = updates;
  }
}
