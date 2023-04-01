package com.dmtavt.fragpipe.tools.diann;

import com.google.common.collect.ComparisonChain;

class Fragment implements Comparable<Fragment> {

  final float mz;
  final float intensity;
  final char type;
  final byte charge;
  final int ordinal;
  final String lossType;

  Fragment(float mz, float intensity, char type, byte charge, int ordinal, String lossType) {
    this.mz = mz;
    this.intensity = intensity;
    this.type = type;
    this.charge = charge;
    this.ordinal = ordinal;
    this.lossType = lossType;
  }

  @Override
  public int compareTo(Fragment o) {
    return ComparisonChain.start()
        .compare(mz, o.mz)
        .compare(type, o.type)
        .compare(ordinal, o.ordinal)
        .compare(lossType, o.lossType)
        .compare(charge, o.charge)
        .result();
  }

  @Override
  public boolean equals(Object o) {
    if (o instanceof Fragment) {
      return compareTo((Fragment) o) == 0;
    } else {
      return false;
    }
  }

  @Override
  public String toString() {
    if (lossType.isEmpty()) {
      return String.valueOf(type) + ordinal + "^" + charge;
    } else {
      return String.valueOf(type) + ordinal + "-" + lossType + "^" + charge;
    }
  }

  @Override
  public int hashCode() {
    return toString().hashCode();
  }
}
