package umich.msfragger.gui;

public class RunnableDescription {

  public final ProcessDescription description;
  public final Runnable runnable;
  public final String parallelGroup;

  public RunnableDescription(ProcessDescription description,
      Runnable runnable) {
    this(description, runnable, null);
  }

  public RunnableDescription(
      ProcessDescription description,
      Runnable runnable, String parallelGroup) {
    this.description = description;
    this.runnable = runnable;
    this.parallelGroup = parallelGroup;
  }
}
