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

package com.dmtavt.fragpipe.dialogs;

import static com.dmtavt.fragpipe.api.InputLcmsFile.disallowedExperimentPattern;

import com.dmtavt.fragpipe.cmd.ToolingUtils;
import com.github.chhh.utils.swing.UiText;
import com.github.chhh.utils.swing.UiUtils;
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.KeyStroke;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;

public class SetExpDialog extends javax.swing.JDialog {

  private JPanel p;
  private JButton buttonOK;
  private JButton buttonCancel;
  private final Frame parent;
  private UiText uiTextExp;
  private int dialogResult = JOptionPane.CLOSED_OPTION;

  public SetExpDialog(Frame parent) {
    super(parent);
    this.parent = parent;
    init();
    postInit();
  }

  @Override
  public void dispose() {
    super.dispose();
  }

  private void postInit() {
    this.setDefaultCloseOperation(DISPOSE_ON_CLOSE);
    this.setLocationRelativeTo(parent);
    pack();
  }

  public boolean isOk() {
    return dialogResult == JOptionPane.OK_OPTION;
  }

  public String getExperimentName() {
    return disallowedExperimentPattern.matcher(uiTextExp.getNonGhostText()).replaceAll("_");
  }

  private void init() {
    Dimension dim = new Dimension(450, 80);
    this.setPreferredSize(dim);
    this.setLayout(new BorderLayout());

    p = new JPanel();
    buttonOK = new JButton("Save");
    buttonCancel = new JButton("Cancel");

    uiTextExp = UiUtils.uiTextBuilder().cols(20).text("").create();

    JLabel jLabelExp = new JLabel("Set experiment:");
    jLabelExp.setLabelFor(uiTextExp);

    MigLayout layout = new MigLayout(new LC().fillX());
    p.setLayout(layout);

    p.add(jLabelExp, new CC().alignX("right"));
    p.add(uiTextExp, new CC().pushX());

    p.add(buttonOK, new CC().tag("ok").split());
    p.add(buttonCancel, new CC().tag("cancel").wrap());

    setContentPane(p);
    setModal(true);
    setModalityType(ModalityType.APPLICATION_MODAL);
    setTitle("Set experiment:");
    setIconImages(ToolingUtils.loadIcon());
    getRootPane().setDefaultButton(buttonOK);

    buttonOK.addActionListener(e -> onOK());
    buttonCancel.addActionListener(e -> onCancel());
    setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
    addWindowListener(new WindowAdapter() {
      public void windowClosing(WindowEvent e) {
        onCancel();
      }
    });
    p.registerKeyboardAction(e -> onCancel(), KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0), JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);
  }

  private void onOK() {
    dialogResult = JOptionPane.OK_OPTION;
    dispose();
  }

  private void onCancel() {
    dialogResult = JOptionPane.CANCEL_OPTION;
    dispose();
  }
}