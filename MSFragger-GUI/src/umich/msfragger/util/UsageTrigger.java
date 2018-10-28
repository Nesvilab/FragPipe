package umich.msfragger.util;

/**
 * Describes the "used" state of some entity. Once {@link #setUsed(boolean)} method was
 * called with 'true' parameter, it can't be reset back.
 */
public class UsageTrigger {
  private boolean isUsed = false;
  private final String desc;

  public UsageTrigger(String desc) {
    this.desc = desc;
  }

  public boolean isUsed() {
    return isUsed;
  }

  public void setUsed(boolean used) {
    if (isUsed)
      return;
    isUsed = used;
  }

  public String getDesc() {
    return desc;
  }
}
