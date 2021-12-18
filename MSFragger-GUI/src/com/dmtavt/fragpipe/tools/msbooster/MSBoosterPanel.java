package com.dmtavt.fragpipe.tools.msbooster;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.tabs.TabMsfragger;
import com.dmtavt.fragpipe.tools.percolator.PercolatorPanel;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.swing.JPanelBase;
import com.github.chhh.utils.swing.MigUtils;
import com.github.chhh.utils.swing.UiCheck;
import java.awt.Component;
import java.awt.ItemSelectable;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class MSBoosterPanel extends JPanelBase {
  private static final Logger log = LoggerFactory.getLogger(MSBoosterPanel.class);
  private static final String PREFIX = "msbooster.";
  private JPanel pTop;
  private JPanel pContent;
  private static final MigUtils mu = MigUtils.get();
  private UiCheck checkRun;
  private UiCheck uiCheckPredictRT;
  private UiCheck uiCheckPredictSpectra;

  public MSBoosterPanel() {
    super();
  }

  @Override
  protected ItemSelectable getRunCheckbox() {
    return checkRun;
  }

  @Override
  protected Component getEnablementToggleComponent() {
    return pContent;
  }

  @Override
  protected String getComponentNamePrefix() {
    return PREFIX;
  }

  @Override
  protected void init() {
    mu.layout(this, mu.lcFillXNoInsetsTopBottom());
    mu.border(this, "Rescoring Using Deep Learning Prediction");

    pTop = createPanelTop();

    uiCheckPredictRT = new UiCheck("Predict RT", null, true);
    uiCheckPredictRT.setName("predict-rt");

    uiCheckPredictSpectra = new UiCheck("Predict spectra", null, true);
    uiCheckPredictSpectra.setName("predict-spectra");

    pContent = mu.newPanel(null, mu.lcFillXNoInsetsTopBottom());
    mu.add(pContent, uiCheckPredictRT).split();
    mu.add(pContent, uiCheckPredictSpectra);

    mu.add(this, pTop).growX().wrap();
    mu.add(this, pContent).growX().wrap();
  }

  private JPanel createPanelTop() {

    JPanel p = mu.newPanel(null, mu.lcFillXNoInsetsTopBottom());

    checkRun = new UiCheck("Run MSBooster", null, false);
    checkRun.setName("run-msbooster");
    JLabel info = new JLabel("<html>Rescoring using deep learning prediction. Require <b>Run Percolator</b> in PSM validation panel.");

    mu.add(p, checkRun).split();
    mu.add(p, info).gapLeft("80px").wrap();
    return p;
  }

  public boolean isRun() {
    return SwingUtils.isEnabledAndChecked(checkRun);
  }

  public boolean predictRt() {
    return uiCheckPredictRT.isSelected();
  }

  public boolean predictSpectra() {
    return uiCheckPredictSpectra.isSelected();
  }

  @Override
  public void initMore() {
    checkRun.addItemListener(e -> {
      if (isRun()) {
        final TabMsfragger tabMsfragger = Fragpipe.getStickyStrict(TabMsfragger.class);
        if (tabMsfragger.isOpenMassOffsetSearch()) {
          if (Fragpipe.headless) {
            log.error("MSBooster is incompatible with open search or mass offset search.");
            System.exit(1);
          }
          JOptionPane.showMessageDialog(this, "<html>MSBooster is incompatible with open search or mass offset search. Uncheck it.", "Incompatible options", JOptionPane.WARNING_MESSAGE);
          checkRun.setSelected(false);
        } else {
          final PercolatorPanel percolatorPanel = Fragpipe.getStickyStrict(PercolatorPanel.class);
          percolatorPanel.checkRun.setSelected(true);
        }
      }
    });

    updateEnabledStatus(this, true);
    super.initMore();
  }
}
