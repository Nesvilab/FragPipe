package com.dmtavt.fragpipe;

import com.dmtavt.fragpipe.api.Bus;
import com.dmtavt.fragpipe.messages.MessageDoneCreatingUi;
import com.dmtavt.fragpipe.messages.MessageLcmsFilesAdded;
import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.ThreadMode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.nio.file.Paths;
import java.util.Arrays;

public class FragpipeDebug {
    private static final Logger log = LoggerFactory.getLogger(FragpipeDebug.class);

    public FragpipeDebug() {
        Bus.registerQuietly(this);
    }

    @Subscribe(threadMode = ThreadMode.ASYNC)
    public void on(MessageDoneCreatingUi m) {
        if (log.isDebugEnabled()) {
            Bus.post(new MessageLcmsFilesAdded(Arrays.asList(Paths.get("path-to-file-to-automatically-add"))));
        }
    }

}
