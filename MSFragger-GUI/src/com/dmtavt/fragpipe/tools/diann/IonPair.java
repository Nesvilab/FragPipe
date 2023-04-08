package com.dmtavt.fragpipe.tools.diann;

class IonPair {

  final IonEntry lightIonEntry;
  final IonEntry mediumIonEntry;
  final IonEntry heavyIonEntry;

  IonPair(IonEntry lightIonEntry, IonEntry mediumIonEntry, IonEntry heavyIonEntry) {
    this.lightIonEntry = lightIonEntry;
    this.mediumIonEntry = mediumIonEntry;
    this.heavyIonEntry = heavyIonEntry;
  }

  float getTotalIntensity() {
    float totalIntensity = 0;
    if (lightIonEntry != null) {
      totalIntensity += lightIonEntry.intensity;
    }
    if (mediumIonEntry != null) {
      totalIntensity += mediumIonEntry.intensity;
    }
    if (heavyIonEntry != null) {
      totalIntensity += heavyIonEntry.intensity;
    }
    return totalIntensity;
  }
}
