package com.dmtavt.fragpipe.tabs;

import com.dmtavt.fragpipe.api.Bus;
import com.dmtavt.fragpipe.messages.MessageExportLog;
import com.github.chhh.utils.SwingUtils;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.UIManager;
import javax.swing.border.Border;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import com.github.chhh.utils.swing.JPanelWithEnablement;
import com.github.chhh.utils.swing.TextConsole;

public class TabRun extends JPanelWithEnablement {

  final TextConsole console;
  JScrollPane scrollConsole;
  Color defTextColor;

  public TabRun(TextConsole console) {
    this.console = console;
    init();
  }

  private void init() {
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
