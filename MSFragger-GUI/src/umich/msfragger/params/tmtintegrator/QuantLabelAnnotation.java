package umich.msfragger.params.tmtintegrator;

public class QuantLabelAnnotation {

  private String label;
  private String sample;

  public QuantLabelAnnotation() {
  }

  public QuantLabelAnnotation(String label, String sample) {
    this.label = label;
    this.sample = sample;
  }

  public String getLabel() {
    return label;
  }

  public void setLabel(String label) {
    this.label = label;
  }

  public String getSample() {
    return sample;
  }

  public void setSample(String sample) {
    this.sample = sample;
  }
}
