package umich.msfragger.gui;

public class RunnableDescription {

  public final ProcessDescription description;
  public final Runnable runnable;

  public RunnableDescription(ProcessDescription description,
      Runnable runnable) {
    this.description = description;
    this.runnable = runnable;
  }
}
