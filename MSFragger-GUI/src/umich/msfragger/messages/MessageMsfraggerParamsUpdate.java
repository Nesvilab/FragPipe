package umich.msfragger.messages;

import umich.msfragger.params.fragger.MsfraggerParams;

public class MessageMsfraggerParamsUpdate {
  public final MsfraggerParams params;

  public MessageMsfraggerParamsUpdate(MsfraggerParams params) {
    this.params = params;
  }
}
