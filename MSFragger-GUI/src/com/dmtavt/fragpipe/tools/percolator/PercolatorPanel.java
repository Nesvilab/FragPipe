package com.dmtavt.fragpipe.tools.percolator;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.api.SearchTypeProp;
import com.dmtavt.fragpipe.messages.MessageSearchType;
import com.dmtavt.fragpipe.tabs.TabMsfragger;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.swing.FormEntry;
import com.github.chhh.utils.swing.JPanelBase;
import com.github.chhh.utils.swing.MigUtils;
import com.github.chhh.utils.swing.UiCheck;
import com.github.chhh.utils.swing.UiCombo;
import com.github.chhh.utils.swing.UiRadio;
import com.github.chhh.utils.swing.UiText;
import com.github.chhh.utils.swing.UiUtils;
import java.awt.Component;
import java.awt.ItemSelectable;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.ThreadMode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class PercolatorPanel extends JPanelBase {
    private static final Logger log = LoggerFactory.getLogger(PercolatorPanel.class);
    private static MigUtils mu = MigUtils.get();
    private UiRadio checkRun;
    private UiText uiTextCmdOpts;
    private UiCheck uiCheckCombinePepxml;
    private JPanel pTop;
    private JPanel pContent;
    private final boolean parentPanelEnabled;
    public static final String PREFIX = "percolator.";

    public PercolatorPanel(ButtonGroup buttonGroup, boolean parentPanelEnabled) {
        buttonGroup.add(checkRun);
        this.parentPanelEnabled = parentPanelEnabled;
    }

    public boolean isRun() {
        return parentPanelEnabled && SwingUtils.isEnabledAndChecked(checkRun);
    }

    public String getCmdOpts() {
        return uiTextCmdOpts.getNonGhostText().trim();
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
            final TabMsfragger tabMsfragger = Fragpipe.getStickyStrict(TabMsfragger.class);
            if (isRun() && tabMsfragger.isOpenMassOffsetSearch()) {
                JOptionPane.showMessageDialog(this,
                    "<html>Percolator is incompatible with open search. Please check <code>Run PeptideProphet</code>.",
                    "Incompatible options", JOptionPane.WARNING_MESSAGE);
            }
        });

        updateEnabledStatus(this, true);
        super.initMore();
    }

//    @Subscribe(sticky = true, threadMode = ThreadMode.MAIN_ORDERED)
//    public void on(NoteConfigPhilosopher m) {
//        updateEnabledStatus(this, m.isValid());
//    }

    @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
    public void on(MessageSearchType m) {
        log.debug("Got MessageSearchType of type [{}], loading defaults for it", m.type.toString());
        checkRun.setSelected(false);
        loadDefaults(m.type);
    }

    @Override
    protected void init() {
        checkRun = new UiRadio("Run Percolator", null, true);
        checkRun.setName("run-percolator");
        JLabel labelDefaults = new JLabel("Defaults for:");
        final LinkedHashMap<String, SearchTypeProp> defaults = new LinkedHashMap<>();
        defaults.put("Closed Search", SearchTypeProp.closed);
        defaults.put("Open Search", SearchTypeProp.open);
        defaults.put("Non-Specific Search", SearchTypeProp.nonspecific);
        defaults.put("Offset Search", SearchTypeProp.offset);
        final UiCombo uiComboDefaults = UiUtils.createUiCombo(new ArrayList<>(defaults.keySet()));
        JButton btnLoadDefaults = UiUtils
                .createButton("Load", "Load Percolator settings for given search type", e -> {
                    SearchTypeProp type = defaults.get((String) uiComboDefaults.getSelectedItem());
                    loadDefaults(type);
                });
        uiTextCmdOpts = UiUtils.uiTextBuilder().cols(20).text(defaultCmdOpts()).create();
        FormEntry feCmdOpts = fe(uiTextCmdOpts, "cmd-opts")
                .label("Cmd line opts:")
                .tooltip("These options will be passed on to Percolator.\n"
                        + "This set will be merged with some additional options\n"
                        + "added as a requirement by other stages in the pipeline.\n"
                        + "See output log (e.g. dry-run results) for the complete command.").create();
        uiCheckCombinePepxml = UiUtils
                .createUiCheck("<html>Single <b>combined</b> pepxml file per experiment / group", false);
        uiCheckCombinePepxml.setName("combine-pepxml");


        mu.layout(this).fillX();
        mu.borderEmpty(this);

        pTop = mu.newPanel(null, mu.lcFillXNoInsetsTopBottom());
        mu.add(pTop, checkRun).split();
//        mu.add(pTop, labelDefaults);
//        mu.add(pTop, uiComboDefaults);
//        mu.add(pTop, btnLoadDefaults).wrap();

        pContent = mu.newPanel(null, mu.lcFillXNoInsetsTopBottom());
        mu.add(pContent, feCmdOpts.label()).alignX("right");
        mu.add(pContent, feCmdOpts.comp).growX().pushX().wrap();
//        mu.add(pContent, uiCheckCombinePepxml).skip(1).wrap();

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
