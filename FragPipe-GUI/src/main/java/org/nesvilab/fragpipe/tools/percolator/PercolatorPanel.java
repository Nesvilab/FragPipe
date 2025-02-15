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

package org.nesvilab.fragpipe.tools.percolator;

import org.nesvilab.fragpipe.Fragpipe;
import org.nesvilab.fragpipe.api.SearchTypeProp;
import org.nesvilab.fragpipe.messages.MessageSearchType;
import org.nesvilab.fragpipe.tabs.TabMsfragger;
import org.nesvilab.fragpipe.tools.PSMValidation;
import org.nesvilab.utils.SwingUtils;
import org.nesvilab.utils.swing.FormEntry;
import org.nesvilab.utils.swing.JPanelBase;
import org.nesvilab.utils.swing.MigUtils;
import org.nesvilab.utils.swing.UiCheck;
import org.nesvilab.utils.swing.UiRadio;
import org.nesvilab.utils.swing.UiSpinnerDouble;
import org.nesvilab.utils.swing.UiText;
import org.nesvilab.utils.swing.UiUtils;
import java.awt.Component;
import java.awt.ItemSelectable;
import javax.swing.ButtonGroup;
import javax.swing.JComponent;
import javax.swing.JPanel;
import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.ThreadMode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class PercolatorPanel extends JPanelBase {
    private static final Logger log = LoggerFactory.getLogger(PercolatorPanel.class);
    private static MigUtils mu = MigUtils.get();
    public UiRadio checkRun;
    private UiCheck checkKeepTsvFiles;
    private UiSpinnerDouble spinMinProb;
    private UiText uiTextCmdOpts;
    private UiCheck uiCheckCombinePepxml;
    private JPanel pTop;
    private JPanel pContent;
    public static final String PREFIX = "percolator.";

    public PercolatorPanel(ButtonGroup buttonGroup) {
        buttonGroup.add(checkRun);
    }

    public boolean isKeepTsvFiles() {
        return checkKeepTsvFiles.isSelected();
    }

    public boolean isRun() {
        PSMValidation psmValidation = Fragpipe.getStickyStrict(PSMValidation.class);
        return psmValidation.isRun() && SwingUtils.isEnabledAndChecked(checkRun);
    }

    public boolean isSelected() {
        return checkRun.isSelected();
    }

    public String getCmdOpts() {
        return uiTextCmdOpts.getNonGhostText().trim();
    }

    public double getMinProb() {
        return Double.parseDouble(spinMinProb.asString());
    }

    public boolean isCombinePepxml() {
        return uiCheckCombinePepxml.isSelected();
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
    protected void initMore() {
        checkRun.addItemListener(e -> {
            if (isRun()) {
                final TabMsfragger tabMsfragger = Fragpipe.getStickyStrict(TabMsfragger.class);
                tabMsfragger.uiComboOutputType.setSelectedIndex(5);
            }
        });

        updateEnabledStatus(this, true);
        super.initMore();
    }

    @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
    public void on(MessageSearchType m) {
        log.debug("Got MessageSearchType of type [{}], loading defaults for it", m.type.toString());
        switch (m.type) {
            case open:
            case offset:
            case glyco:
                checkRun.setSelected(false);
                break;
            case closed:
            case nonspecific:
                checkRun.setSelected(true);
                break;
            default:
                throw new IllegalStateException("Not covered enum option: " + m.type.name());
        }
        loadDefaults(m.type);
    }

    @Override
    protected void init() {
        checkRun = new UiRadio("Run Percolator", null, true);
        checkRun.setName("run-percolator");

        checkKeepTsvFiles = new UiCheck("Keep intermediate files", null, false);
        checkKeepTsvFiles.setName("keep-tsv-files");

        spinMinProb = UiUtils.spinnerDouble(0.50, 0, 1, 0.01).setCols(4).setFormat("#.##").create();
        FormEntry feMinProb = mu.feb(spinMinProb).name("min-prob").label("Min probability").tooltip("Minimum probability threshold").create();

        uiTextCmdOpts = UiUtils.uiTextBuilder().cols(20).text(defaultCmdOpts()).create();
        FormEntry feCmdOpts = fe(uiTextCmdOpts, "cmd-opts")
                .label("Cmd line opts:")
                .tooltip("These options will be passed on to Percolator.\n"
                        + "This set will be merged with some additional options\n"
                        + "added as a requirement by other stages in the pipeline.\n"
                        + "To set --num-threads, please adjust the Parallelism setting in the Workflow tab.\n"
                        + "See output log (e.g. dry-run results) for the complete command.").create();
        uiCheckCombinePepxml = UiUtils
                .createUiCheck("<html>Single <b>combined</b> pepxml file per experiment / group", false);
        uiCheckCombinePepxml.setName("combine-pepxml");


        mu.layout(this).fillX();
        mu.borderEmpty(this);

        pTop = mu.newPanel(null, mu.lcFillXNoInsetsTopBottom());
        mu.add(pTop, checkRun).split();
        mu.add(pTop, checkKeepTsvFiles);
        mu.add(pTop, feMinProb.label(), mu.ccR()).gapLeft("80px");
        mu.add(pTop, feMinProb.comp).pushX().wrap();

        pContent = mu.newPanel(null, mu.lcFillXNoInsetsTopBottom());
        mu.add(pContent, feCmdOpts.label()).alignX("right");
        mu.add(pContent, feCmdOpts.comp).growX().pushX().wrap();

        mu.add(this, pTop).growX().wrap();
        mu.add(this, pContent).growX().wrap();
    }

    private void loadDefaults(SearchTypeProp type) {
        Fragpipe.getPropsFixAndSetVal("percolator.cmd.line.opts." + type.name(), uiTextCmdOpts);
        Fragpipe.getPropsFixAndSetVal("percolator.combine.pepxml." + type.name(), uiCheckCombinePepxml);
    }

    private String defaultCmdOpts() {
        String v = Fragpipe.getPropFix("percolator.cmd.line.opts", "closed");
        log.debug("Peptide prophet default value for Cmd Opts in ui fetched from properties: percolator.cmd.line.opts.closed={}", v);
        return v;
    }

    private FormEntry.Builder fe(JComponent comp, String name) {
        return Fragpipe.fe(comp, name, PREFIX);
    }

}
