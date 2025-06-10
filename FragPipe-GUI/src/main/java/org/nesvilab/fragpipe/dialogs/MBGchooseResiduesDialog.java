package org.nesvilab.fragpipe.dialogs;

import org.nesvilab.fragpipe.Fragpipe;
import org.nesvilab.fragpipe.api.GlycoModsTable;
import org.nesvilab.fragpipe.api.GlycoModsTableModel;
import org.nesvilab.fragpipe.cmd.ToolingUtils;
import org.nesvilab.fragpipe.tabs.TabMsfragger;
import org.nesvilab.fragpipe.tools.glyco.GlycoMassLoader;
import org.nesvilab.utils.swing.*;
import org.nesvilab.utils.swing.renderers.TableCellDoubleRenderer;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import umich.ms.glyco.GlycanMod;
import umich.ms.glyco.GlycanResidue;

import javax.swing.*;
import java.awt.*;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class MBGchooseResiduesDialog extends javax.swing.JDialog  {
    private final Frame parent;
    private int dialogResult = JOptionPane.CLOSED_OPTION;

    private MigUtils mu;
    public GlycoModsTable tableGlycoMods;
    private final GlycoMassLoader glycoLoader;
    private static final String[] TABLE_MODS_COL_NAMES = {"Enabled", "Name", "Mass"};
    public static final String TAB_PREFIX = "mbg.";
    private static final ArrayList<String> DEFAULT_ENABLED_RESIDUES;
    private final String initialResidues;

    static {
        DEFAULT_ENABLED_RESIDUES = new ArrayList<>();
        DEFAULT_ENABLED_RESIDUES.add("HexNAc");
        DEFAULT_ENABLED_RESIDUES.add("Hex");
        DEFAULT_ENABLED_RESIDUES.add("Fuc");
    }


    public MBGchooseResiduesDialog(Frame parent, GlycoMassLoader loader, String currentResidues) {
        super(parent);
        this.parent = parent;
        this.glycoLoader = loader;
        this.initialResidues = currentResidues;
        init();
        postInit();
    }

    private void postInit() {
        this.setDefaultCloseOperation(DISPOSE_ON_CLOSE);
        this.setLocationRelativeTo(parent);
        pack();
    }

    private void init() {
        JPanel p = new JPanel();
        JScrollPane scroll = new JScrollPane(p, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        this.setContentPane(scroll);
        mu = MigUtils.get();

        JPanel glycoModsPanel = createPanelChooseResidues();

        JButton buttonOK = new JButton("OK");
        JButton buttonCancel = new JButton("Cancel");

        setModal(true);
        setModalityType(Dialog.ModalityType.APPLICATION_MODAL);
        setTitle("Choose MBG residues:");
        setIconImages(ToolingUtils.loadIcon());
        getRootPane().setDefaultButton(buttonOK);

        buttonOK.addActionListener(e -> onOK());
        buttonCancel.addActionListener(e -> onCancel());
        // call onCancel() when cross is clicked
        setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
        addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e) {
                onCancel();
            }
        });
        // call onCancel() on ESCAPE
        p.registerKeyboardAction(e -> onCancel(), KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0),
                JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);

        MigLayout layout = new MigLayout(new LC().fillX());//.debug());
        p.setLayout(layout);

        p.add(new JScrollPane(glycoModsPanel), new CC().grow().spanX().wrap());

        p.add(buttonOK, new CC().tag("ok").split());
        p.add(buttonCancel, new CC().tag("cancel").wrap());
    }

    private void onOK() {
        dialogResult = JOptionPane.OK_OPTION;
        dispose();
    }

    private void onCancel() {
        dialogResult = JOptionPane.CANCEL_OPTION;
        dispose();
    }

    public int getDialogResult() {
        return dialogResult;
    }

    private JPanel createPanelChooseResidues() {
        JPanel p = mu.newPanel("Choose Glycan Residues/Mods for MBG Matching", true);

        tableGlycoMods = createTableMBGresidues();
        SwingUtilities.invokeLater(() -> {
            TabMsfragger.setJTableColSize(tableGlycoMods, 0, 20, 150, 50);
        });
        JScrollPane varModsScroll = new JScrollPane(tableGlycoMods, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        p.add(varModsScroll, new CC().minHeight("200px").maxHeight("250px").spanX().wrap());

        return p;
    }

    private GlycoModsTable createTableMBGresidues() {
        List<GlycanMod> residuesAndMods = getResiduesAndMods();
        Object[][] data = convertGlycoModsToData(residuesAndMods);
        GlycoModsTableModel m = new GlycoModsTableModel(
                TABLE_MODS_COL_NAMES,
                new Class<?>[]{Boolean.class, String.class, Float.class},
                new boolean[]{true, false, false},
                data);
        final GlycoModsTable t = new GlycoModsTable(m, TABLE_MODS_COL_NAMES, MBGchooseResiduesDialog::convertGlycoModsToData);
        Fragpipe.rename(t, "table.mbg-residues", TAB_PREFIX);

        t.setToolTipText("Check the box(es) at left for each residue/mod to include in MBG");
        t.setDefaultRenderer(Float.class, new TableCellDoubleRenderer());

        return t;
    }

    private static Object[][] convertGlycoModsToData(List<GlycanMod> mods) {
        Object[][] data = new Object[mods.size()][TABLE_MODS_COL_NAMES.length];
        for (int i = 0; i < data.length; i++) {
            data[i][0] = false;
            for (int j = 1; j < TABLE_MODS_COL_NAMES.length; j++) {
                data[i][j] = null;
            }
        }

        for (int i = 0; i < mods.size(); i++) {
            GlycanMod m = mods.get(i);
            data[i][0] = m.isEnabled;
            data[i][1] = m.name;
            data[i][2] = m.mass;
        }
        return data;
    }

    /**
     * Convert GlycanResidues to GlycanMods to have access to the "enabled" attribute for specifying which residues/mods
     * to use for MBG. Creates new objects so as not to mess with the residue/mod definitions.
     * @return list of GlycanMods
     */
    private List<GlycanMod> getResiduesAndMods() {
        ArrayList<GlycanMod> residuesAndMods = new ArrayList<>();
        // initialize previously entered residue(s) if present
        ArrayList<String> initialResList = new ArrayList<>();
        if (!initialResidues.isEmpty()) {
            String[] residues = initialResidues.split("[\\s,;/]+");
            initialResList.addAll(Arrays.asList(residues));
        }

        for (GlycanResidue residue: glycoLoader.glycanResidueDefinitions) {
            GlycanMod newResidue = new GlycanMod(residue);
            if (initialResidues.isEmpty()) {
                // use defaults if the param is not set
                if (DEFAULT_ENABLED_RESIDUES.contains(newResidue.name)) {
                    newResidue.isEnabled = true;
                }
            } else {
                // use previous setting
                if (initialResList.contains(newResidue.name)) {
                    newResidue.isEnabled = true;
                }
            }
            residuesAndMods.add(newResidue);
        }
        for (GlycanMod mod: glycoLoader.glycanModDefinitions) {
            GlycanMod newMod = new GlycanMod(mod);
            if (!initialResidues.isEmpty()) {
                if (initialResList.contains(newMod.name)) {
                    newMod.isEnabled = true;
                }
            }
            residuesAndMods.add(newMod);
        }
        return residuesAndMods;
    }
}
