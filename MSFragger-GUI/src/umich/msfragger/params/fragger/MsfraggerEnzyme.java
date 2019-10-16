package umich.msfragger.params.fragger;

import java.util.StringJoiner;

public class MsfraggerEnzyme {
  public final String name;
  public final String cut;
  public final String nocuts;
  public final String sense = "C";

  public MsfraggerEnzyme(String name, String cut, String nocuts) {
    this.name = name;
    this.cut = cut;
    this.nocuts = nocuts;
  }

  @Override
  public String toString() {
    return new StringJoiner(", ", MsfraggerEnzyme.class.getSimpleName() + "[", "]")
        .add("name='" + name + "'")
        .add("cut='" + cut + "'")
        .add("nocuts='" + nocuts + "'")
        .add("sense='" + sense + "'")
        .toString();
  }
}
