package umich.msfragger.params.tmtintegrator;

public class ComboValue {
  public final String valInFile;
  public final String valInUi;
  public final String description;

  public ComboValue(String valInFile, String valInUi, String description) {
    this.valInFile = valInFile;
    this.valInUi = valInUi;
    this.description = description;
  }

  public ComboValue(String valueInFile, String valInUi) {
    this(valueInFile, valInUi, "");
  }

  public String getValInFile() {
    return valInFile;
  }

  public String getValInUi() {
    return valInUi;
  }

  public String getDescription() {
    return description;
  }
}
