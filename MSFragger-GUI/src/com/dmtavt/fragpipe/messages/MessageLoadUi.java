package com.dmtavt.fragpipe.messages;

import com.dmtavt.fragpipe.api.PropsFile;
import java.util.Objects;
import java.util.Properties;

public class MessageLoadUi {
  public final Properties props;

  public MessageLoadUi(Properties props) {
    Objects.requireNonNull(props);
    this.props = props;
  }
}
