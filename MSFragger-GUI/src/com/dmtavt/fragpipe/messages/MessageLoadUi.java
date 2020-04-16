package com.dmtavt.fragpipe.messages;

import com.dmtavt.fragpipe.api.PropsFile;
import java.util.Objects;

public class MessageLoadUi {
  public final PropsFile props;

  public MessageLoadUi(PropsFile props) {
    Objects.requireNonNull(props);
    this.props = props;
  }
}
