package com.github.chhh.utils;

import java.text.SimpleDateFormat;
import java.util.Date;

public class TimeUtils {
  private TimeUtils() {}

  public static String dateTimeNoSpaces() {
    SimpleDateFormat df = new SimpleDateFormat("yyyy-MM-dd_HH-mm-ss");
    Date now = new Date();
    return String.format("%s", df.format(now));
  }

}
