package com.dmtavt.fragpipe.internal;

import com.dmtavt.fragpipe.cmd.CmdBase;
import java.util.StringJoiner;

public class CmdBaseNode {
  public final CmdBase cmdBase;

  public CmdBaseNode(CmdBase cmdBase) {
    this.cmdBase = cmdBase;
  }

  @Override
  public String toString() {
//    return new StringJoiner(", ", CmdBaseNode.class.getSimpleName() + "[", "]")
//        .add("cmdBase=" + cmdBase)
//        .toString();
    return cmdBase == null ? "n/a" : cmdBase.getCmdName();

  }
}
