package com.dmtavt.fragpipe.messages;

import com.dmtavt.fragpipe.tools.comet.CometParams;

public class MessageCometParamsUpdate {
    public final CometParams params;

    public MessageCometParamsUpdate(CometParams params) {
        this.params = params;
    }
}
