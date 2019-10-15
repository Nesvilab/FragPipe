package umich.msfragger.gui;

public class Tooltips {
  private Tooltips() {}

  public static String tipBtnAssignToSelected() {
    return "<html>Assign each of selected rows to an Experiment and/or Replicate.<br/>\n"
        + "<b>For convenience</b>:<br/>\n"
        + "If selected rows are all assigned to same Experiment or Replicate already<br/>\n"
        + "the corresponding form fields will be pre-populated.";
  }
}
