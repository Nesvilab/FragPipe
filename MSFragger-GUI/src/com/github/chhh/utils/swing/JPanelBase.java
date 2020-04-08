package com.github.chhh.utils.swing;

public abstract class JPanelBase extends JPanelWithEnablement {
  protected final MigUtils mu = MigUtils.get();

  public JPanelBase() {
    init();
    initMore();
  }

  public abstract void init();
  public abstract void initMore();
}
