/*
 * This file is part of FragPipe.
 *
 * FragPipe is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * FragPipe is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with FragPipe. If not, see <https://www.gnu.org/licenses/>.
 */

package com.dmtavt.fragpipe;

import java.nio.file.Paths;

public class FragPipeMain {
  public static void main(String[] args) {
    if (args.length == 1 && (args[0].equalsIgnoreCase("--help") || args[0].equalsIgnoreCase("-h"))) {
      System.out.print(Fragpipe.help());
      System.exit(1);
    } else if (args.length > 0) {
      for (int i = 0; i < args.length; ++i) {
        if (args[i].equalsIgnoreCase("--headless")) {
          System.setProperty("java.awt.headless", "true"); // In some rare case, the server does not have X11 but DISPLAY env var is set, which crashes the headless mode. Setting the headless env to true to prevent the crash.
          Fragpipe.headless = true;
        } else if (args[i].equalsIgnoreCase("--dry-run")) {
          Fragpipe.dryRun = true;
        } else if (args[i].equalsIgnoreCase("--workflow")) {
          Fragpipe.workflowFile = Paths.get(args[++i]);
        } else if (args[i].equalsIgnoreCase("--manifest")) {
          Fragpipe.manifestFile = Paths.get(args[++i]);
        } else if (args[i].equalsIgnoreCase("--ram")) {
          Fragpipe.ram = Integer.parseInt(args[++i]);
        } else if (args[i].equalsIgnoreCase("--threads")) {
          Fragpipe.nThreadsHeadlessOnly = Integer.parseInt(args[++i]);
        } else if (args[i].equalsIgnoreCase("--workdir")) {
          Fragpipe.workdir = args[++i].trim();
        } else if (args[i].equalsIgnoreCase("--config-msfragger")) {
          Fragpipe.msfraggerBinPath = args[++i].trim();
        } else if (args[i].equalsIgnoreCase("--config-philosopher")) {
          Fragpipe.philosopherBinPath = args[++i].trim();
        } else if (args[i].equalsIgnoreCase("--config-diann")) {
          Fragpipe.diannBinPath = args[++i].trim();
        } else if (args[i].equalsIgnoreCase("--config-python")) {
          Fragpipe.pythonBinPath = args[++i].trim();
        } else if (args[i].equalsIgnoreCase("--config-ionquant")){
          Fragpipe.ionquantBinPath = args[++i].trim();
        } else {
          System.err.println("Cannot recognize the argument " + args[i]);
          System.exit(1);
        }
      }
    }

    Fragpipe.main0();
  }
}
