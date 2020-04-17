package com.dmtavt.fragpipe.messages;

import com.dmtavt.fragpipe.params.fragger.MsfraggerParams;

public class MessageMsfraggerParamsUpdate {
  public final MsfraggerParams params;

  public MessageMsfraggerParamsUpdate(MsfraggerParams params) {
    this.params = params;
  }
}
