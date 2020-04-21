package com.dmtavt.fragpipe.messages;

import com.dmtavt.fragpipe.api.SearchTypeProp;
import java.util.StringJoiner;

public class MessageLoadShepherdDefaults {
  public final boolean doAskUser;
  public final SearchTypeProp type;

  public MessageLoadShepherdDefaults(boolean doAskUser, SearchTypeProp type) {
    this.doAskUser = doAskUser;
    this.type = type;
  }

  @Override
  public String toString() {
    return new StringJoiner(", ", MessageLoadShepherdDefaults.class.getSimpleName() + "[",
        "]")
        .add("doAskUser=" + doAskUser)
        .add("type=" + type)
        .toString();
  }
}
