/*
 * Copyright 2018 Dmitry Avtonomov.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package umich.msfragger.params.fragger;

import java.awt.BorderLayout;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.border.EmptyBorder;
import javax.swing.border.TitledBorder;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.greenrobot.eventbus.EventBus;
import umich.msfragger.gui.api.SearchTypeProp;
import umich.msfragger.messages.MessageSearchType;

/**
 *
 * @author Dmitry Avtonomov
 */
public class FraggerMigPanel extends JPanel {

  private ImageIcon icon;
  private JCheckBox checkRun;
  private JScrollPane scroll;

  public FraggerMigPanel() {
    initMore();
  }

  private void initMore() {
    icon = new ImageIcon(
        getClass().getResource("/umich/msfragger/gui/icons/bolt-16.png"));

    this.setLayout(new BorderLayout());

    LC lc = new LC();//.debug();
//    LC lc = new LC().debug();

    // Top panel with checkbox and buttons
    {
      JPanel pTop = new JPanel(new MigLayout(lc));
      checkRun = new JCheckBox("Run DIA-Umpire SE (Signal Extraction)");
      JButton closed = new JButton("Closed");
      closed.addActionListener(e -> {
        EventBus.getDefault().post(new MessageSearchType(SearchTypeProp.closed));
      });
      JButton open = new JButton("Open");
      open.addActionListener(e -> {
        EventBus.getDefault().post(new MessageSearchType(SearchTypeProp.open));
      });
      JButton nonspecific = new JButton("Non-specific");
      open.addActionListener(e -> {
        EventBus.getDefault().post(new MessageSearchType(SearchTypeProp.nonspecific));
      });

      pTop.add(checkRun);
      pTop.add(new JLabel("Defaults:"), new CC().gapLeft("35px"));
      pTop.add(closed, new CC().gapLeft("1px"));
      pTop.add(open, new CC().gapLeft("1px"));
      pTop.add(nonspecific, new CC().gapLeft("1px"));

      this.add(pTop, BorderLayout.NORTH);
    }

    JPanel pContent = new JPanel();
    pContent.setLayout(new MigLayout(new LC().fillX()));
    scroll = new JScrollPane(pContent);
    scroll.setBorder(new EmptyBorder(0,0,0,0));
    scroll.getVerticalScrollBar().setUnitIncrement(16);

    // Panel with all the basic options
    {
      JPanel pBasic = new JPanel(new MigLayout(lc));
      pBasic.setBorder(new TitledBorder("Basic Options"));
      pBasic.add(new JLabel("Basic Options panel"), new CC().wrap());
      pBasic.add(new JLabel("Basic Options panel"), new CC().wrap());
      pBasic.add(new JLabel("Basic Options panel"), new CC().wrap());
      pBasic.add(new JLabel("Basic Options panel"), new CC().wrap());
      pBasic.add(new JLabel("Basic Options panel"), new CC());
      pBasic.add(new JLabel("Basic Options panel"), new CC());
      pBasic.add(new JLabel("Basic Options panel"), new CC());
      pBasic.add(new JLabel("Basic Options panel"), new CC());
      pBasic.add(new JLabel("Basic Options panel"), new CC());
      pBasic.add(new JLabel("Basic Options panel"), new CC());
      pBasic.add(new JLabel("Basic Options panel"), new CC());
      pBasic.add(new JLabel("Basic Options panel"), new CC().wrap());
      pBasic.add(new JLabel("Basic Options panel"), new CC().wrap());
      pBasic.add(new JLabel("Basic Options panel"), new CC().wrap());
      pBasic.add(new JLabel("Basic Options panel"), new CC().wrap());
      pBasic.add(new JLabel("Basic Options panel"), new CC().wrap());
      pBasic.add(new JLabel("Basic Options panel"), new CC().wrap());
      pBasic.add(new JLabel("Basic Options panel"), new CC().wrap());
      pBasic.add(new JLabel("Basic Options panel"), new CC().wrap());

      pContent.add(pBasic, new CC().wrap().growX());
    }

    // Panel with all the advanced options
    {
      JPanel pAdvanced = new JPanel(new MigLayout(lc));
      pAdvanced.setBorder(new TitledBorder("Advanced Options"));
      pAdvanced.add(new JLabel("Advanced Options panel"), new CC().wrap());
      pAdvanced.add(new JLabel("Advanced Options panel"), new CC().wrap());
      pAdvanced.add(new JLabel("Advanced Options panel"), new CC().wrap());
      pAdvanced.add(new JLabel("Advanced Options panel"), new CC().wrap());
      pAdvanced.add(new JLabel("Advanced Options panel"), new CC().wrap());
      pAdvanced.add(new JLabel("Advanced Options panel"), new CC().wrap());
      pAdvanced.add(new JLabel("Advanced Options panel"), new CC().wrap());
      pAdvanced.add(new JLabel("Advanced Options panel"), new CC().wrap());
      pAdvanced.add(new JLabel("Advanced Options panel"), new CC().wrap());
      pAdvanced.add(new JLabel("Advanced Options panel"), new CC().wrap());
      pAdvanced.add(new JLabel("Advanced Options panel"), new CC().wrap());
      pAdvanced.add(new JLabel("Advanced Options panel"), new CC().wrap());
      pAdvanced.add(new JLabel("Advanced Options panel"), new CC().wrap());
      pAdvanced.add(new JLabel("Advanced Options panel"), new CC().wrap());
      pAdvanced.add(new JLabel("Advanced Options panel"), new CC().wrap());
      pAdvanced.add(new JLabel("Advanced Options panel"), new CC().wrap());
      pAdvanced.add(new JLabel("Advanced Options panel"), new CC().wrap());
      pAdvanced.add(new JLabel("Advanced Options panel"), new CC().wrap());
      pAdvanced.add(new JLabel("Advanced Options panel"), new CC().wrap());
      pAdvanced.add(new JLabel("Advanced Options panel"), new CC().wrap());
      pAdvanced.add(new JLabel("Advanced Options panel"), new CC().wrap());
      pAdvanced.add(new JLabel("Advanced Options panel"), new CC().wrap());
      pAdvanced.add(new JLabel("Advanced Options panel"), new CC().wrap());
      pAdvanced.add(new JLabel("Advanced Options panel"), new CC().wrap());
      pAdvanced.add(new JLabel("Advanced Options panel"), new CC().wrap());

      pContent.add(pAdvanced, new CC().wrap().growX());
    }

    this.add(scroll, BorderLayout.CENTER);
  }
}
