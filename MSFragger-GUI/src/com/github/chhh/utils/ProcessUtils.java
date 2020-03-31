package com.github.chhh.utils;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ProcessUtils {
  private static final Logger log = LoggerFactory.getLogger(ProcessUtils.class);

  private ProcessUtils() {}

  public static String captureOutput(ProcessBuilder pb) throws IOException, InterruptedException {
    pb.redirectErrorStream(true);
    StringBuilder sb = new StringBuilder();
    log.debug("Starting process to capture output: {}", String.join(" ", pb.command()));
    Process p = pb.start();
    try (BufferedReader br = new BufferedReader(new InputStreamReader(p.getInputStream()))) {
      String line;
      while ((line = br.readLine()) != null) {
        if (sb.length() > 0)
          sb.append("\n");
        sb.append(line);
      }
    }
    int code = p.waitFor();
    log.debug("Got return code {} from process: {}", code, String.join(" ", pb.command()));
    return sb.toString();
  }
}
