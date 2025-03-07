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

package org.nesvilab.fragpipe.cmd;

import org.nesvilab.fragpipe.Fragpipe;
import org.nesvilab.fragpipe.FragpipeLocations;
import org.nesvilab.fragpipe.tabs.TabDownstream.Mode;
import org.nesvilab.utils.OsUtils;
import java.awt.Component;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import javax.swing.JOptionPane;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CmdSaintExpress extends CmdBase {

  private static final Logger log = LoggerFactory.getLogger(CmdSaintExpress.class);
  public static final String NAME = "SaintExpress";

  public static final String SAINT_EXPRESS_SPECTRAL_COUNTS = "SAINTexpress/SAINTexpress-spc" + (OsUtils.isUnix() ? "" : ".exe");
  public static final String SAINT_EXPRESS_INTENSITY = "SAINTexpress/SAINTexpress-int" + (OsUtils.isUnix() ? "" : ".exe");

  public CmdSaintExpress(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  public boolean configure(Component comp, Mode mode, int maxReplicates, int virtualControls, String cmdOpts) {
    initPreConfig();

    final Path inputPath = wd.resolve("reprint." + mode + ".tsv");
    final Path outputPath = wd.resolve("saintexpress-" + mode);
    final Path saintExpressBin = FragpipeLocations.get().getDirTools().resolve(Map.of(Mode.SPECTRAL_COUNT, SAINT_EXPRESS_SPECTRAL_COUNTS, Mode.INTENSITY, SAINT_EXPRESS_INTENSITY).get(mode));

    if (!Files.exists(wd) || !Files.isReadable(wd) || !Files.isDirectory(wd)) {
      if (Fragpipe.headless) {
        log.error("Directory " + wd.toAbsolutePath() + " is not accessible.");
      } else {
        JOptionPane.showMessageDialog(comp, "Directory " + wd.toAbsolutePath() + " is not accessible.", NAME + " error", JOptionPane.ERROR_MESSAGE);
      }
      return false;
    }

    if (!Files.exists(inputPath) || !Files.isReadable(inputPath) || !Files.isRegularFile(inputPath)) {
      if (Fragpipe.headless) {
        log.error("Input file " + inputPath.toAbsolutePath() + " is not accessible.");
      } else {
        JOptionPane.showMessageDialog(comp, "Input file " + inputPath.toAbsolutePath() + " is not accessible.", NAME + " error", JOptionPane.ERROR_MESSAGE);
      }
      return false;
    }

    reprintTsvToSaintInput(inputPath, outputPath);

    List<String> cmd = new ArrayList<>();
    cmd.add(saintExpressBin.toAbsolutePath().normalize().toString());
    cmd.add("-R" + maxReplicates);
    cmd.add("-L" + virtualControls);
    if (!cmdOpts.isEmpty()) {
      cmd.add(cmdOpts);
    }
    ProcessBuilder pb = new ProcessBuilder(cmd);
    pb.directory(outputPath.toFile());
    pbis.add(PbiBuilder.from(pb));

    isConfigured = true;
    return true;
  }

  public static void reprintTsvToSaintInput(final Path inputPath, final Path outPath) {
    final ArrayList<String> inter_dat = new ArrayList<>();
    final ArrayList<String> bait_dat = new ArrayList<>();
    final ArrayList<String> prey_dat = new ArrayList<>();
    try (final BufferedReader br = Files.newBufferedReader(inputPath)) {
      final String[] first_row = br.readLine().split("\t");
      final boolean has_protlen = first_row[2].equalsIgnoreCase("PROTLEN");
      final boolean has_geneid = first_row[1].equalsIgnoreCase("GENEID");
      final int column_offset = has_protlen ? 3 : has_geneid ? 2 : 1;
      final String[] IP = new String[first_row.length - column_offset];
      for (int i = 0; i < IP.length; i++) {
        final String s = first_row[i + column_offset];
        if (!(s.endsWith("_INT") || s.endsWith("_SPC"))) {
          throw new AssertionError(s);
        }
        IP[i] = s.substring(0, s.length() - 4);
      }
      final String[] bait_names = new String[IP.length];
      final String[] second_row = br.readLine().split("\t");
      for (int i = 0; i < IP.length; i++) {
        final String s = second_row[i + column_offset];
        bait_names[i] = s.equalsIgnoreCase("CONTROL") ? IP[i] : s;
      }
      for (int i = 0; i < IP.length; ++i) {
        bait_dat.add(IP[i] + "\t" + bait_names[i] + "\t" + (bait_names[i].toUpperCase().startsWith("CONTROL") ? "C" : "T"));
      }
      String line;
      while ((line = br.readLine()) != null) {
        final String[] ls = line.split("\t");
        final String prey = ls[0];
        prey_dat.add(ls[0] + "\t" + (has_protlen ? ls[2] : -1) + "\t" + (has_geneid ? ls[1] : ls[0]));
        for (int i = 0; i < IP.length; ++i) {
          final String e = ls[i + column_offset];
          if (Double.parseDouble(e) != 0) {
            inter_dat.add(IP[i] + "\t" + bait_names[i] + "\t" + prey + "\t" + e);
          }
        }
      }
    } catch (IOException e) {
      throw new RuntimeException(e);
    }

    try {
      Files.createDirectories(outPath);
    } catch (FileAlreadyExistsException ignore) {
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    }
    try (final BufferedWriter bw = Files.newBufferedWriter(outPath.resolve("inter.dat"))) {
      for (String e : inter_dat) {
        bw.write(e + "\n");
      }
      bw.write('\n');
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    }
    try (final BufferedWriter bw = Files.newBufferedWriter(outPath.resolve("bait.dat"))) {
      for (String e : bait_dat) {
        bw.write(e + "\n");
      }
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    }
    try (final BufferedWriter bw = Files.newBufferedWriter(outPath.resolve("prey.dat"))) {
      for (String e : prey_dat) {
        bw.write(e + "\n");
      }
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    }
  }
}
