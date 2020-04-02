package com.dmtavt.fragpipe.api;

import com.github.chhh.utils.CheckResult;

@FunctionalInterface
public
interface ICheck {
  CheckResult perform() throws Exception;
}
