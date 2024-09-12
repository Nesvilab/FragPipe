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

package com.dmtavt.fragpipe.tabs;

import static com.dmtavt.fragpipe.FragpipeRun.createVersionsString;
import static com.dmtavt.fragpipe.FragpipeRun.printProcessDescription;
import static com.dmtavt.fragpipe.messages.MessagePrintToConsole.toConsole;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.FragpipeRun;
import com.dmtavt.fragpipe.api.Bus;
import com.dmtavt.fragpipe.cmd.CmdSaintExpress;
import com.dmtavt.fragpipe.cmd.PbiBuilder;
import com.dmtavt.fragpipe.cmd.ProcessBuilderInfo;
import com.dmtavt.fragpipe.cmd.ProcessBuildersDescriptor;
import com.dmtavt.fragpipe.messages.MessageRunButtonEnabled;
import com.dmtavt.fragpipe.messages.MessageRunDownstream;
import com.dmtavt.fragpipe.messages.MessageStartProcesses;
import com.dmtavt.fragpipe.process.ProcessDescription;
import com.dmtavt.fragpipe.process.ProcessDescription.Builder;
import com.dmtavt.fragpipe.process.RunnableDescription;
import com.dmtavt.fragpipe.tools.downstream.SaintexpressPanel;
import com.dmtavt.fragpipe.tools.fpop.FpopCoadaptrPanel;
import com.dmtavt.fragpipe.tools.fpop.FpopQuantPanel;
import com.github.chhh.utils.OsUtils;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.swing.JPanelWithEnablement;
import com.github.chhh.utils.swing.MigUtils;
import com.github.chhh.utils.swing.TextConsole;
import com.github.chhh.utils.swing.UiCheck;
import com.github.chhh.utils.swing.UiUtils;
import com.github.chhh.utils.swing.FormEntry;
import com.github.chhh.utils.swing.UiText;
import com.github.chhh.utils.swing.UiSpinnerInt;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.stream.Collectors;
import javax.swing.JButton;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.UIManager;
import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.ThreadMode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class TabDownstream extends JPanelWithEnablement {

  private static final Logger log = LoggerFactory.getLogger(TabDownstream.class);

  public enum Mode {
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
  private SaintexpressPanel pSaintExpress;
  public FpopQuantPanel pFpop;
  public FpopCoadaptrPanel pFpopCoadaptr;
  private JPanel pBottom;
  private JPanel pConsole;
  private UiCheck uiCheckWordWrap;


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

  private int runSaintExpress(MessageRunDownstream m) {
    boolean runConfigurationDone = false;
    try {
      Bus.post(new MessageRunButtonEnabled(false));

      if (!m.runSaintExpress) {
        return 0;
      }

      final TabRun tabRun = Bus.getStickyEvent(TabRun.class);
      if (tabRun.getWorkdirText().isEmpty()) {
        if (Fragpipe.headless) {
          log.error("Directory " + tabRun.getWorkdirText() + " is empty.");
        } else {
          JOptionPane.showMessageDialog(this, "Directory " + tabRun.getWorkdirText() + " is empty.", TAB_PREFIX + " error", JOptionPane.ERROR_MESSAGE);
        }
        return 1;
      }

      Path wd = Paths.get(tabRun.getWorkdirText());

      // prepare the processes
      List<ProcessBuildersDescriptor> pbDescsBuilderDescs = new ArrayList<>(1);
      for (Mode mode : Mode.values()) {
        CmdSaintExpress cmdSaintExpress = new CmdSaintExpress(true, wd);
        if (cmdSaintExpress.configure(this, mode, m.maxReplicates, m.virtualControls, m.cmdOpts)) {
          ProcessBuildersDescriptor processBuildersDescriptor = cmdSaintExpress.getBuilderDescriptor();
          processBuildersDescriptor.setParallelGroup(mode.toString());
          pbDescsBuilderDescs.add(processBuildersDescriptor);
        }
      }

      toConsole(OsUtils.OsInfo() + "\n" + OsUtils.JavaInfo() + "\n" + OsUtils.NetCoreInfo() + "\n", console);
      toConsole("", console);
      toConsole("Version info:\n" + createVersionsString(), console);
      toConsole("", console);

      final List<ProcessBuilderInfo> pbis = pbDescsBuilderDescs.stream().flatMap(pbd -> pbd.pbis.stream().map(pbi -> {
        PbiBuilder b = new PbiBuilder();
        b.setPb(pbi.pb);
        b.setName(pbi.name != null ? pbi.name : pbd.name);
        b.setFnStdOut(pbi.fnStdout != null ? pbi.fnStdout : pbd.fnStdout);
        b.setFnStdErr(pbi.fnStderr != null ? pbi.fnStderr : pbd.fnStderr);
        b.setParallelGroup(pbi.parallelGroup != null ? pbi.parallelGroup : pbd.getParallelGroup());
        return b.create();
      })).collect(Collectors.toList());

      toConsole(String.format(Locale.ROOT, "%d commands to execute:", pbis.size()), console);
      for (final ProcessBuilderInfo pbi : pbis) {
        printProcessDescription(pbi, console);
      }
      toConsole("~~~~~~~~~~~~~~~~~~~~~~", console);
      toConsole("", console);

      if (m.isDryRun) {
        toConsole(Fragpipe.COLOR_RED_DARKEST, "\nIt's a dry-run, not running the commands.\n", true, console);
        printReference();
        return 0;
      }

      // run everything
      long startTime = System.nanoTime();
      final List<RunnableDescription> toRun = new ArrayList<>();
      for (final ProcessBuilderInfo pbi : pbis) {
        Runnable runnable = ProcessBuilderInfo.toRunnable(pbi, wd, FragpipeRun::printProcessDescription, console, true);
        ProcessDescription.Builder b = new ProcessDescription.Builder().setName(pbi.name);
        if (pbi.pb.directory() != null) {
          b.setWorkDir(pbi.pb.directory().toString());
        }
        if (pbi.pb.command() != null && !pbi.pb.command().isEmpty()) {
          b.setCommand(String.join(" ", pbi.pb.command()));
        }
        toRun.add(new RunnableDescription(b.create(), runnable, pbi.parallelGroup, pbi));
      }

      // add finalizer process
      final Runnable finalizerRun = () -> {
        String totalTime = String.format("%.1f", (System.nanoTime() - startTime) * 1e-9 / 60);
        toConsole(Fragpipe.COLOR_RED_DARKEST, "\n=============================================================ALL JOBS DONE IN " + totalTime + " MINUTES=============================================================", true, console);

        Bus.post(new MessageRunButtonEnabled(true));
      };
      toRun.add(new RunnableDescription(new Builder().setName("Finalizer Task").create(), finalizerRun));

      Bus.post(new MessageStartProcesses(toRun));

      runConfigurationDone = true;
    } catch (Exception ex) {
      toConsole(Fragpipe.COLOR_RED_DARKEST, ex.getMessage(), true, console);
      return 1;
    } finally {
      if (!runConfigurationDone) {
        Bus.post(new MessageRunButtonEnabled(true));
      }
    }

    return 0;
  }

  private void printReference() {
    toConsole(Fragpipe.COLOR_RED_DARKEST, "\nPlease cite:", true, console);
    toConsole(Fragpipe.COLOR_BLACK, "Teo, G., et al. SAINTexpress: improvements and additional features in Significance Analysis of INTeractome software. J Proteomics, 100:37 (2014)", true, console);
  }

  private JPanel createPanelBottom(TextConsole console) {
    uiCheckDryRun = UiUtils.createUiCheck("Dry Run", false);
    btnRun = UiUtils.createButton("Run", e -> Bus.post(new MessageRunDownstream(isDryRun(), pSaintExpress.isRunSaintexpress(), pSaintExpress.getMaxReplicates(), pSaintExpress.getVirtualControls(), pSaintExpress.getCmdOpts())));

    JButton btnClearConsole = UiUtils.createButton("Clear Console", e -> clearConsole());
    uiCheckWordWrap = UiUtils.createUiCheck("Word wrap", true, e -> {
      console.setScrollableTracksViewportWidth(uiCheckWordWrap.isSelected());
      console.setVisible(false);
      console.setVisible(true);
    });

    console.setScrollableTracksViewportWidth(true);

    JPanel p = mu.newPanel(null, true);
    mu.add(p, btnRun).split(5);
    mu.add(p, uiCheckDryRun);
    mu.add(p, btnClearConsole);
    mu.add(p, uiCheckWordWrap).wrap();

    return p;
  }

  @Subscribe(threadMode = ThreadMode.ASYNC)
  public void on(MessageRunDownstream m) {
    int returnCode =  runSaintExpress(m);
    if (Fragpipe.headless && returnCode != 0) {
      System.exit(returnCode);
    }
  }

  public boolean isDryRun() {
    return SwingUtils.isEnabledAndChecked(uiCheckDryRun);
  }


  protected void init() {
    defTextColor = UIManager.getColor("TextField.foreground");
    if (defTextColor == null) {
      defTextColor = Color.BLACK;
    }

    pFpopCoadaptr = new FpopCoadaptrPanel();
    pFpop = new FpopQuantPanel();

    pSaintExpress = new SaintexpressPanel();
    pBottom = createPanelBottom(console);
    pBottom.setPreferredSize(new Dimension(400, 50));
    initConsole(console);
    pConsole = createPanelConsole(console);

    mu.layout(this).fillX();
    mu.add(this, pFpopCoadaptr).growX().alignY("top").wrap();
    mu.add(this, pFpop).growX().alignY("top").wrap();
    mu.add(this, pSaintExpress).growX().alignY("top").wrap();
    mu.add(this, pBottom).growX().alignY("top").wrap();
    mu.add(this, pConsole).grow().push().alignY("top").wrap();
  }


  private TextConsole createConsole() {
    TextConsole c = new TextConsole(false);
    final Font currentFont = c.getFont();
    c.setFont(new Font(Font.MONOSPACED, currentFont.getStyle(), currentFont.getSize()));
    c.setContentType("text/plain; charset=UTF-8");
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
