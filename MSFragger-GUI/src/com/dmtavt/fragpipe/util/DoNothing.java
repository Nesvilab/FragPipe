package com.dmtavt.fragpipe.util;

public class DoNothing {

  public static void main(String[] args) throws InterruptedException {
    long millis = 1000;
    if (args.length > 0) {
      try {
        millis = Long.parseLong(args[0]);
      } catch (Exception ignore) {
        System.err.println(
            "This program accepts only 1 parameter, an integer number of milliseconds to sleep");
      }
    }
    System.out.printf("Sleeping for %dms\n", millis);
    Thread.sleep(millis);
  }

}