package com.dmtavt.fragpipe.tabs;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.api.Bus;
import com.dmtavt.fragpipe.messages.MessageExportLog;
import com.dmtavt.fragpipe.messages.MessageSaveAsWorkflow;
import com.dmtavt.fragpipe.messages.MessageShowAboutDialog;
import com.github.chhh.utils.swing.FileChooserUtils;
import com.github.chhh.utils.swing.FileChooserUtils.FcMode;
import com.github.chhh.utils.swing.FormEntry;
import com.github.chhh.utils.swing.MigUtils;
import com.github.chhh.utils.swing.UiText;
import com.github.chhh.utils.swing.UiUtils;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.stream.Stream;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.UIManager;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import com.github.chhh.utils.swing.JPanelWithEnablement;
import com.github.chhh.utils.swing.TextConsole;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class TabRun extends JPanelWithEnablement {
  private static final Logger log = LoggerFactory.getLogger(TabRun.class);
  public static final MigUtils mu = MigUtils.get();
  private static final String TAB_PREFIX = "tab-run.";
  private static final String LAST_WORK_DIR = "workdir.last-path";
  final TextConsole console;
  JScrollPane scrollConsole;
  Color defTextColor;
  private UiText uiTextWorkdir;

  public TabRun(TextConsole console) {
    this.console = console;
    init();
    initMore();
  }

  private void initMore() {
    Bus.registerQuietly(this);
  }

//  private JPanel createPanelTop() {
//    JButton btnSaveAsWorkflow = UiUtils
//        .createButton("Save current config as Workflow",
//            e -> Bus.post(new MessageSaveAsWorkflow()));
//    JButton btnAbout = UiUtils.createButton("About", e -> Bus.post(new MessageShowAboutDialog()));
//    uiTextWorkdir = UiUtils.uiTextBuilder().cols(30).create();
//    FormEntry feWorkdir = mu.feb("workdir", uiTextWorkdir).label("Output dir:")
//        .tooltip("Processing results will be stored in this directory").create();
//    feWorkdir.browseButton(() -> FileChooserUtils.builder("Select output directory")
//        .mode(FcMode.DIRS_ONLY).multi(false)
//        .paths(Stream.of(uiTextWorkdir.getNonGhostText(), Fragpipe.propsVar().getProperty(LAST_WORK_DIR))).create(),
//        "Select output directory",
//        selected -> {
//          uiTextWorkdir.setText(selected.get(0).toString());
//        }
//        );
//    // TODO: compose Run panel of these buttons
//
//    return adsf;
//  }

  protected void init() {
    //this.setLayout(new MigLayout(new LC().fillX()));
    BoxLayout layout = new BoxLayout(this, BoxLayout.Y_AXIS);
    this.setLayout(layout);
    initConsole(console);
    scrollConsole = new JScrollPane();
    scrollConsole.setViewportView(console);
    //this.scrollConsole = SwingUtils.scroll(console);

    defTextColor = UIManager.getColor("TextField.foreground");
    if (defTextColor == null) {
      defTextColor = Color.BLACK;
    }

    JPanel pTop = new JPanel(new MigLayout(new LC().fillX()));
    pTop.setMinimumSize(new Dimension(300, 50));
    pTop.add(new JLabel("SOme text", JLabel.CENTER), new CC().spanX().wrap());

    add(pTop);
    add(scrollConsole);
  }

  private void initConsole(TextConsole console) {
    final Font currentFont = console.getFont();
    console.setFont(new Font(Font.MONOSPACED, currentFont.getStyle(), currentFont.getSize()));
    console.setContentType("text/plain; charset=UTF-8");
    console.addMouseListener(new MouseAdapter() {

      @Override
      public void mouseReleased(MouseEvent e) {
        if (e.isPopupTrigger()) {
          doPop(e);
        }
      }

      private void doPop(MouseEvent e) {
        JPopupMenu menu = new JPopupMenu();
        JMenuItem ctxItemExport = new JMenuItem("Export log to text file");
        ctxItemExport.addActionListener(e1 -> {
          Bus.post(new MessageExportLog());
        });
        menu.add(ctxItemExport);
        menu.show(e.getComponent(), e.getX(), e.getY());
      }
    });
  }
}
