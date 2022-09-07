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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with FragPipe.  If not, see <https://www.gnu.org/licenses/>.
 */

package com.dmtavt.fragpipe.tabs;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.FragpipeLocations;
import com.dmtavt.fragpipe.api.Bus;
import com.dmtavt.fragpipe.tools.downstream.SaintexpressPanel;
import com.github.chhh.utils.OsUtils;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.swing.JPanelWithEnablement;
import com.github.chhh.utils.swing.LogbackJTextPaneAppender;
import com.github.chhh.utils.swing.MigUtils;
import com.github.chhh.utils.swing.TextConsole;
import com.github.chhh.utils.swing.UiCheck;
import com.github.chhh.utils.swing.UiUtils;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.UIManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class TabDownstream extends JPanelWithEnablement {

  private static final Logger log = LoggerFactory.getLogger(TabDownstream.class);
  public static final String SAINT_EXPRESS_SPECTRAL_COUNTS = "SAINTexpress/SAINTexpress-spc" + (OsUtils.isUnix() ? "" : ".exe");
  public static final String SAINT_EXPRESS_INTENSITY = "SAINTexpress/SAINTexpress-int" + (OsUtils.isUnix() ? "" : ".exe");

  private enum Mode {
    SPECTRAL_COUNT("spc"),
    INTENSITY("int");
    private final String name;

    Mode(String name) {
      this.name = name;
    }

    @Override
    public String toString() {
      return name;
    }
  }

  public static final MigUtils mu = MigUtils.get();
  public static final String TAB_PREFIX = "tab-downstream.";
  final TextConsole console;
  Color defTextColor;
  private UiCheck uiCheckDryRun;
  public JButton btnRun;
  public JButton btnStop;
  private SaintexpressPanel pSaintExpress;
  private JPanel pBottom;
  private JPanel pConsole;
  private UiCheck uiCheckWordWrap;
  private Process saintExpressProcess;

  public TabDownstream() {
    this.console = createConsole();
    init();
    initMore();
  }

  private void initMore() {
    Bus.registerQuietly(this);
    Bus.postSticky(this);
  }

  private void clearConsole() {
    console.setText("");
  }

  private void toConsole(Color color, String text, boolean addNewline) {
    if (Fragpipe.headless) {
      System.out.print(text);
      if (addNewline) {
        System.out.println();
      }
    }
    console.append(color, text);
    if (addNewline) {
      console.append("\n");
    }
    console.getParent().getParent().revalidate();
  }

  private int runSaintExpress() {
    if (!pSaintExpress.isRunSaintexpress()) {
      return 0;
    }

    final TabRun tabRun = Bus.getStickyEvent(TabRun.class);
    if (tabRun.getWorkdirText().isEmpty()) {
      toConsole(Fragpipe.COLOR_RED_DARKEST, "Directory " + tabRun.getWorkdirText() + " is empty.", true);
      return 1;
    }

    Path wd = Paths.get(tabRun.getWorkdirText());
    if (!Files.exists(wd) || !Files.isReadable(wd) || !Files.isDirectory(wd)) {
      toConsole(Fragpipe.COLOR_RED_DARKEST, "Directory " + wd.toAbsolutePath() + " is not accessible.", true);
      return 1;
    }

    for (Mode mode : Mode.values()) {
      final Path inputPath = wd.resolve("reprint." + mode + ".tsv");

      if (!Files.exists(inputPath) || !Files.isReadable(inputPath) || !Files.isRegularFile(inputPath)) {
        toConsole(Fragpipe.COLOR_RED_DARKEST, "Input file " + inputPath.toAbsolutePath() + " is not accessible.", true);
        return 1;
      }

      final Path outPath = wd.resolve("saintexpress-" + mode);
      reprintTsvToSaintInput(inputPath, outPath);
      final Path bin = FragpipeLocations.get().getDirTools().resolve(Map.of(Mode.SPECTRAL_COUNT, SAINT_EXPRESS_SPECTRAL_COUNTS, Mode.INTENSITY, SAINT_EXPRESS_INTENSITY).get(mode));
      try {
        List<String> command = new ArrayList<>(List.of(bin.toString(), "-L" + pSaintExpress.getMaxReplicates(), "-R" + pSaintExpress.getVirtualControls()));
        if (!pSaintExpress.getCmdOpts().isEmpty()) {
          command.add(pSaintExpress.getCmdOpts());
        }
        final ProcessBuilder pb = new ProcessBuilder().command(command).redirectErrorStream(true).directory(outPath.toFile());

        toConsole(Fragpipe.COLOR_TOOL, bin.getFileName().toString(), false);
        toConsole(Fragpipe.COLOR_WORKDIR, " [Work dir: " + pb.directory() + "]", true);
        toConsole(Fragpipe.COLOR_CMDLINE, String.join(" ", pb.command()), true);

        if (!isDryRun()) {
          saintExpressProcess = pb.start();
          toConsole(null, new String(saintExpressProcess.getInputStream().readAllBytes()), true);
          saintExpressProcess.waitFor();
          Thread.sleep(5);
        }
      } catch (Exception e) {
        throw new RuntimeException(e);
      }
    }

    toConsole(Fragpipe.COLOR_RED_DARKEST, "\n=============================================================ALL JOBS DONE=============================================================", true);

    if (isDryRun()) {
      toConsole(Fragpipe.COLOR_RED_DARKEST, "\nIt's a dry-run, not running the commands.\n", true);
    }

    toConsole(Fragpipe.COLOR_RED_DARKEST, "\nPlease cite:", true);
    toConsole(Fragpipe.COLOR_BLACK, "Teo, G., et al. SAINTexpress: improvements and additional features in Significance Analysis of INTeractome software. J Proteomics, 100:37 (2014)", true);

    return 0;
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
        bait_names[i] = s.equals("CONTROL") ? IP[i] : s;
      }
      for (int i = 0; i < IP.length; ++i) {
        bait_dat.add(IP[i] + "\t" + bait_names[i] + "\t" + (bait_names[i].startsWith("CONTROL") ? "C" : "T"));
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

  private void killSaintExpress() {
    if (saintExpressProcess != null) {
      saintExpressProcess.destroyForcibly();
    }
    btnRun.setEnabled(true);
    btnStop.setEnabled(false);
  }

  private JPanel createPanelBottom(TextConsole console) {
    uiCheckDryRun = UiUtils.createUiCheck("Dry Run", false);
    btnRun = UiUtils.createButton("Run", e -> {
      btnRun.setEnabled(false);
      btnStop.setEnabled(true);
      runSaintExpress();
      btnRun.setEnabled(true);
      btnStop.setEnabled(false);
    });
    btnStop = UiUtils.createButton("Stop", e -> killSaintExpress());

    JButton btnClearConsole = UiUtils.createButton("Clear Console", e -> clearConsole());
    uiCheckWordWrap = UiUtils.createUiCheck("Word wrap", console.getScrollableTracksViewportWidth(), e -> {
      console.setScrollableTracksViewportWidth(uiCheckWordWrap.isSelected());
      console.setVisible(false);
      console.setVisible(true);
    });

    JPanel p = mu.newPanel(null, true);
    mu.add(p, btnRun).split(5);
    mu.add(p, btnStop);
    mu.add(p, uiCheckDryRun);
    mu.add(p, btnClearConsole);
    mu.add(p, uiCheckWordWrap).wrap();

    return p;
  }

  public boolean isDryRun() {
    return SwingUtils.isEnabledAndChecked(uiCheckDryRun);
  }

  protected void init() {
    defTextColor = UIManager.getColor("TextField.foreground");
    if (defTextColor == null) {
      defTextColor = Color.BLACK;
    }

    pSaintExpress = new SaintexpressPanel();
    pBottom = createPanelBottom(console);
    pBottom.setPreferredSize(new Dimension(400, 50));
    initConsole(console);
    pConsole = createPanelConsole(console);

    mu.layout(this).fillX();
    mu.add(this, pSaintExpress).growX().alignY("top").wrap();
    mu.add(this, pBottom).growX().alignY("top").wrap();
    mu.add(this, pConsole).grow().push().alignY("top").wrap();
  }

  private TextConsole createConsole() {
    TextConsole c = new TextConsole(false);
    final Font currentFont = c.getFont();
    c.setFont(new Font(Font.MONOSPACED, currentFont.getStyle(), currentFont.getSize()));
    c.setContentType("text/plain; charset=UTF-8");

    LogbackJTextPaneAppender appender = new LogbackJTextPaneAppender();
    appender.start();
    log.debug("Started LogbackJTextPaneAppender logger");
    appender.setTextPane(c);

    return c;
  }


  private JPanel createPanelConsole(TextConsole tc) {
    JPanel p = mu.newPanel("Console", mu.lcNoInsetsTopBottom());

    JScrollPane scroll = SwingUtils.wrapInScroll(tc);
    scroll.setMinimumSize(new Dimension(400, 50));
    // the editor does not originally occupy the whole width of the viewport
    // so we mask it off with the same color as the console
    scroll.getViewport().setBackground(tc.getBackground());

    mu.add(p, scroll).grow().push().wrap();
    return p;
  }

  private void initConsole(TextConsole console) {
    final Font currentFont = console.getFont();

    console.setFont(new Font(Font.MONOSPACED, currentFont.getStyle(), currentFont.getSize()));
    console.setContentType("text/plain; charset=UTF-8");
  }
}
