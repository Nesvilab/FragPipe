package umich.msfragger.messages;

import umich.msfragger.params.fragger.MsfraggerParams;

public class MsfraggerParamsUpdate {
  public final MsfraggerParams params;

  public MsfraggerParamsUpdate(MsfraggerParams params) {
    this.params = params;
  }
}
