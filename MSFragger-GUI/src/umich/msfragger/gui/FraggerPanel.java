/* 
 * Copyright (C) 2018 Dmitry Avtonomov
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package umich.msfragger.gui;

import java.awt.Component;
import java.awt.Container;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.lang.ref.WeakReference;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.Locale;
import javax.swing.ComboBoxModel;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.JSpinner;
import javax.swing.JTable;
import javax.swing.SwingUtilities;
import javax.swing.filechooser.FileNameExtensionFilter;
import javax.swing.table.TableModel;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.DocumentFilter;
import javax.swing.text.PlainDocument;
import net.java.balloontip.BalloonTip;
import static umich.msfragger.gui.MsfraggerGuiFrame.DEFAULT_TYPE;

import umich.msfragger.gui.api.SearchTypeProp;
import umich.msfragger.gui.renderers.TableCellDoubleRenderer;
import umich.msfragger.params.ThisAppProps;
import umich.msfragger.params.enums.CleavageType;
import umich.msfragger.params.enums.FraggerOutputType;
import umich.msfragger.params.enums.MassTolUnits;
import umich.msfragger.params.enums.MsLevel;
import umich.msfragger.params.fragger.Mod;
import umich.msfragger.params.fragger.MsfraggerParams;
import umich.msfragger.util.DocumentFilters;
import umich.msfragger.util.StringUtils;
import umich.msfragger.util.SwingUtils;
/**
 *
 * @author Dmitry Avtonomov
 */
public class FraggerPanel extends javax.swing.JPanel {

    private static final long serialVersionUID = 1L;

    public static final String PROP_FILECHOOSER_LAST_PATH = "msfragger.filechooser.path";
    
    private MsfraggerParams params;
    private static final String[] TABLE_VAR_MODS_COL_NAMES = {"Enabled", "Site (editable)", "Mass Delta (editable)"};
    private ModificationsTableModel tableModelVarMods;
    private static final String[] TABLE_ADD_MODS_COL_NAMES = {"Enabled", "Site", "Mass Delta (editable)"};
    private ModificationsTableModel tableModelAddMods;
    
    public static FileNameExtensionFilter fileNameExtensionFilter = new FileNameExtensionFilter("LCMS files (mzML/mzXML/mgf)", "mzml", "mzxml", "mgf");
    
    private BalloonTip dbPathTip = null;
    WeakReference<MsfraggerGuiFrame> frame = null;
    
    int stateDbSlicing = 1;
    
    /**
     * Creates new form FraggerPanel
     * @param frame The frame this panel is added to. This is just a kludge as
     * the whole application is just one large mess of a java swing form.
     */
    public FraggerPanel(MsfraggerGuiFrame frame) {
        this.frame = new WeakReference<>(frame);
        this.setLocale(frame.getLocale());
        
        initComponents();
        initMore();
    }

    private void initMore() {
        
        updateRowHeights(tableVarMods);
        tableVarMods.setDefaultRenderer(Double.class, new TableCellDoubleRenderer());
        tableVarMods.setFillsViewportHeight(true);
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                tableVarMods.getColumnModel().getColumn(0).setMaxWidth(150);
                tableVarMods.getColumnModel().getColumn(0).setMinWidth(20);
                tableVarMods.getColumnModel().getColumn(0).setPreferredWidth(50);
            }
        });
        tableAdditionalMods.setDefaultRenderer(Double.class, new TableCellDoubleRenderer());
        tableAdditionalMods.setFillsViewportHeight(true);
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                tableAdditionalMods.getColumnModel().getColumn(0).setMaxWidth(150);
                tableAdditionalMods.getColumnModel().getColumn(0).setMinWidth(20);
                tableAdditionalMods.getColumnModel().getColumn(0).setPreferredWidth(50);
            }
        });
        
        params = new MsfraggerParams();
        try {
            params.load();
            fillFormFromParams(params);
            
        } catch (Exception e) {
            // something went wrong when loading defaults from the temp storage
            String message = String.format(Locale.ROOT, "Could not load previously stored "
                    + "parameters while creating MSFragger panel.\n\n"
                    + "Load defaults instead?\n\n"
                    + "If you choose to load defaults you might also want to click\n"
                    + "the 'Load defaults..' button on the Config panel to make sure\n"
                    + "that other panels are in synch with all the correct options.\n\n"
                    + "If you choose cancel, some parts of the MSFragger panel might not\n"
                    + "be pre-populated with data.");
            String[] options = {"Cancel", "Load defaults for Closed", "Load defaults of Open"};
            int result = JOptionPane.showOptionDialog(this, message, "Reset to defautls", 
                        JOptionPane.DEFAULT_OPTION, JOptionPane.QUESTION_MESSAGE, null, options, options[0]);
            switch (result) {
                case 1:
                    params.clear();
                    params.loadDefaultsClosedSearch();
                    fillFormFromParams(params);
                    break;
                case 2:
                    params.clear();
                    params.loadDefaultsOpenSearch();
                    fillFormFromParams(params);
                    break;
            }
        }
    }
    
    public int getNumSlices() {
        return (Integer)spinnerSlices.getValue();
    }
    
    public String getFastaPath() {
        MsfraggerGuiFrame f = frame.get();
        if (f == null)
            throw new IllegalStateException("getFastaPath() called while not attached to an MsfraggerGuiFrame.");
        return f.getFastaPath();
    }
    
    public int getRamGb() {
        return (Integer)spinnerFraggerRam.getValue();
    }
    
    public int getThreads() {
        return (Integer)spinnerFraggerThreads.getValue();
    }
    
    public String getOutputFileExt() {
        return getOutputType().getExtension();
    }
    
    public FraggerOutputType getOutputType() {
        String val = comboFraggerOutputType.getItemAt(comboFraggerOutputType.getSelectedIndex());
        return FraggerOutputType.valueOf(val);
    }
    
    private void fillFormFromParams(MsfraggerParams params) {
        
        // just skip the fasta file, it's handled separately now
//        MsfraggerGuiFrame f = frame.get();
//        if (f == null)
//            throw new IllegalStateException("fillFormFromParams() called while not attached to an MsfraggerGuiFrame.");
//        f.setFastaPath(params.getDatabaseName());
//        validateFraggerDbPath();
        clearFormTables();
        
        
        int ram = params.getFragpipeRam();
        spinnerFraggerRam.setValue(ram);
        spinnerFraggerThreads.setValue(params.getNumThreads());
        
        
        comboPrecursorMassTol.setSelectedItem(params.getPrecursorMassUnits().toString());
        Double precursorMassTolerance = params.getPrecursorMassTolerance();
        Double precursorMassLower = params.getPrecursorMassLower();
        Double precursorMassUpper = params.getPrecursorMassUpper();
        boolean asymmetricTolerancePresent = precursorMassLower != null && precursorMassUpper != null;
        boolean isAsymmetric = asymmetricTolerancePresent && 
                Math.abs(precursorMassLower) != Math.abs(precursorMassUpper);
        
        if (asymmetricTolerancePresent) {
            spinnerPrecursorMassTolLo.setValue(precursorMassLower);
            spinnerPrecursorMassTolHi.setValue(precursorMassUpper);
        } else if (precursorMassTolerance != null) {
            spinnerPrecursorMassTolLo.setValue(Math.abs(precursorMassTolerance) * -1);
            spinnerPrecursorMassTolHi.setValue(Math.abs(precursorMassTolerance));
        }
        
        comboPrecursorTrueTol.setSelectedItem(params.getPrecursorTrueUnits().toString());
        spinnerPrecursorTrueTol.setValue(params.getPrecursorTrueTolerance());
        
        comboFragMassTol.setSelectedItem(params.getFragmentMassUnits().toString());
        spinnerFragMassTol.setValue(params.getFragmentMassTolerance());
        
        comboFraggerOutputType.setSelectedItem(params.getOutputFormat().toString());
        spinnerReportTopN.setValue(params.getOutputReportTopN());
        spinnerOutputMaxExpect.setValue(params.getOutputMaxExpect());
        
        textIsotopeError.setText(params.getIsotopeError());
        textMassOffsets.setText(params.getMassOffsets());
        spinnerPrecursorChargeLo.setValue(params.getPrecursorCharge()[0]);
        spinnerPrecursorChargeHi.setValue(params.getPrecursorCharge()[1]);
        checkOverrideCharge.setSelected(params.getOverrideCharge());
        
        textEnzymeName.setText(params.getSearchEnzymeName());
        textCutAfter.setText(params.getSearchEnzymeCutAfter());
        textButNotAfter.setText(params.getSearchEnzymeButNotAfter());
        
        comboCleavage.setSelectedItem(params.getNumEnzymeTermini().toString());
        spinnerMissedCleavages.setValue(params.getAllowedMissedCleavage());
        checkClipNTerm.setSelected(params.getClipNTermM());
        
        spinnerDigestLenMin.setValue(params.getDigestMinLength());
        spinnerDigestLenMax.setValue(params.getDigestMaxLength());
        spinnerDigestMassMin.setValue(params.getDigestMassRange()[0]);
        spinnerDigestMassMax.setValue(params.getDigestMassRange()[1]);
        spinnerMaxFragCharge.setValue(params.getMaxFragmentCharge());
        
        spinnerMinPeaks.setValue(params.getMinimumPeaks());
        spinnerUseTopNPeaks.setValue(params.getUseTopNPeaks());
        spinnerMinFragsModelling.setValue(params.getMinFragmentsModelling());
        spinnerMinMatchedFrags.setValue(params.getMinMatchedFragments());
        spinnerMinRatio.setValue(params.getMinimumRatio());
        spinnerClearMzRangeMin.setValue(params.getClearMzRange()[0]);
        spinnerClearMzRangeMax.setValue(params.getClearMzRange()[1]);
        
//        checkZeroBinAcceptExpect.setSelected(params.getZeroBinAcceptExpect());
//        checkZeroBinMultiplyExpect.setSelected(params.getZeroBinMultExpect());
//        checkTrackZeroTopN.setSelected(params.getTrackZeroTopN());
//        checkAddTopNComplementary.setSelected(params.getAddTopNComplementary());
        spinnerZeroBinAcceptExpect.setValue(params.getZeroBinAcceptExpect());
        spinnerZeroBinMultiplyExpect.setValue(params.getZeroBinMultExpect());
        spinnerTrackZeroTopN.setValue(params.getTrackZeroTopN());
        spinnerAddTopNComplementary.setValue(params.getAddTopNComplementary());
        
        checkMultipleVarMods.setSelected(params.getAllowMultipleVariableModsOnResidue());
        spinnerMaxVarModsPerMod.setValue(params.getMaxVariableModsPerMod());
        spinnerMaxCombos.setValue(params.getMaxVariableModsCombinations());
        
        // variable modifications
        Object[][] varModsData = new Object[MsfraggerParams.VAR_MOD_COUNT_MAX][3];
        // set defaults for all fields
        for (int i = 0; i < MsfraggerParams.VAR_MOD_COUNT_MAX; i++) {
            varModsData[i][0] = false;
            varModsData[i][1] = null;
            varModsData[i][2] = null;
        }
        // fill in the actual variable mod data
        List<Mod> varMods = params.getVariableMods();
        boolean hasIllegalSeq = false;
        for (int i = 0; i < varMods.size(); i++) {
            Mod m = varMods.get(i);
            varModsData[i][0] = m.isEnabled;
            varModsData[i][1] = m.sites;
            if (m.sites != null && m.sites.contains("[*")) {
                hasIllegalSeq = true;
            }
            varModsData[i][2] = m.massDelta;
        }
        tableModelVarMods.setDataVector(varModsData, TABLE_VAR_MODS_COL_NAMES);
        
        // fixed modifications
        Object[][] addModsData = new Object[MsfraggerParams.ADDON_NAMES.length][3];
        for (int i = 0; i < MsfraggerParams.ADDON_NAMES.length; i++) {
            addModsData[i][0] = false;
            addModsData[i][1] = null;
            addModsData[i][2] = null;
        }
        List<Mod> addMods = params.getAdditionalMods();
        for (int i = 0; i < addMods.size(); i++) {
            Mod m = addMods.get(i);
            addModsData[i][0] = m.isEnabled;
            addModsData[i][1] = m.sites;
            addModsData[i][2] = m.massDelta;
        }
        tableModelAddMods.setDataVector(addModsData, TABLE_ADD_MODS_COL_NAMES);
        
        if (hasIllegalSeq) {
            String msg = "When populating the form we've noticed that variable modifications\n"
                    + "table contains the site specification '[*'. This is likely a carryover\n"
                    + "from older versions of Fragger parameters. Newer versions of MSFragger\n"
                    + "use '[^' to mark N-terminal modifications for improved compatibility\n"
                    + "with downstream tools.\n\n"
                    + "We suggest reloading default values.\n\n"
                    + "You can fix it yourself in the variable modifications table "
                    + "later if you'd like.";
            String[] options = {"Cancel", "Load defaults for Closed", "Load defaults of Open"};
                int result = JOptionPane.showOptionDialog(this, msg, "Reset to defautls", 
                        JOptionPane.DEFAULT_OPTION, JOptionPane.QUESTION_MESSAGE, null, options, options[0]);
            switch (result) {
                case 1:
                    loadDefaultsClosed();
                    break;
                case 2:
                    loadDefaultsOpen();
                    break;
            }
        }
    }
    
    public MsfraggerParams collectParams() throws IOException {
        MsfraggerParams p = new MsfraggerParams();
        p.loadDefaultsOpenSearch();
        fillParamsFromForm(p);
        return p;
    }
    
    private void fillParamsFromForm(MsfraggerParams params) {
        MsfraggerGuiFrame f = frame.get();
        if (f == null)
            throw new IllegalStateException("fillFormFromParams() called while not attached to an MsfraggerGuiFrame.");
        
        params.setDatabaseName(f.getFastaPath());
        Integer ram = (Integer)spinnerFraggerRam.getValue();
        params.setFragpipeRam(ram);
        params.setNumThreads(utilSpinnerValue(spinnerFraggerThreads, Integer.class));
        
        params.setPrecursorMassUnits(MassTolUnits.valueOf(comboPrecursorMassTol.getItemAt(comboPrecursorMassTol.getSelectedIndex())));
        params.setPrecursorMassLower((Double)spinnerPrecursorMassTolLo.getValue());
        params.setPrecursorMassUpper((Double)spinnerPrecursorMassTolHi.getValue());
        
        params.setPrecursorTrueUnits(MassTolUnits.valueOf(comboPrecursorTrueTol.getItemAt(comboPrecursorTrueTol.getSelectedIndex())));
        params.setPrecursorTrueTolerance((Double)spinnerPrecursorTrueTol.getValue());
        
        params.setFragmentMassUnits(MassTolUnits.valueOf(comboFragMassTol.getItemAt(comboFragMassTol.getSelectedIndex())));
        params.setFragmentMassTolerance((Double)spinnerFragMassTol.getValue());
        
        //params.setOutputFileExtension(textOutputFileExt.getText());
        FraggerOutputType outputType = getOutputType();
        params.setOutputFileExtension(outputType.getExtension());
        params.setOutputFormat(outputType);
        params.setOutputReportTopN((Integer)spinnerReportTopN.getValue());
        params.setOutputMaxExpect((Double)spinnerOutputMaxExpect.getValue());
        
        params.setIsotopeError(textIsotopeError.getText());
        params.setMassOffsets(textMassOffsets.getText());
        
        int zLo = (Integer)spinnerPrecursorChargeLo.getValue();
        int zHi = (Integer)spinnerPrecursorChargeHi.getValue();
        params.setPrecursorCharge(new int[] {zLo, zHi});
        params.setOverrideCharge(checkOverrideCharge.isSelected());
        
        params.setSearchEnzymeName(textEnzymeName.getText());
        params.setSearchEnzymeCutAfter(textCutAfter.getText());
        params.setSearchEnzymeButNotAfter(textButNotAfter.getText());
        
        params.setNumEnzymeTermini(CleavageType.valueOf(comboCleavage.getItemAt(comboCleavage.getSelectedIndex())));
        params.setAllowedMissedCleavage((Integer)spinnerMissedCleavages.getValue());
        params.setClipNTermM(checkClipNTerm.isSelected());
        
        params.setDigestMinLength((Integer)spinnerDigestLenMin.getValue());
        params.setDigestMaxLength((Integer)spinnerDigestLenMax.getValue());
        
        double massLo = (Double)spinnerDigestMassMin.getValue();
        double massHi = (Double)spinnerDigestMassMax.getValue();
        params.setDigestMassRange(new double[] {massLo, massHi});
        params.setMaxFragmentCharge((Integer)spinnerMaxFragCharge.getValue());
        
        params.setMinimumPeaks(utilSpinnerValue(spinnerMinPeaks, Integer.class));
        params.setUseTopNPeaks((Integer)spinnerUseTopNPeaks.getValue());
        
        params.setMinFragmentsModelling((Integer)spinnerMinFragsModelling.getValue());
        params.setMinMatchedFragments((Integer)spinnerMinMatchedFrags.getValue());
        params.setMinimumRatio((Double)spinnerMinRatio.getValue());
        
        double clearMzLo = (Double)spinnerClearMzRangeMin.getValue();
        double clearMzHi = (Double)spinnerClearMzRangeMax.getValue();
        params.setClearMzRange(new double[] {clearMzLo, clearMzHi});
        
//        params.setZeroBinAcceptExpect(checkZeroBinAcceptExpect.isSelected());
//        params.setZeroBinMultExpect(checkZeroBinMultiplyExpect.isSelected());
//        params.setTrackZeroTopN(checkTrackZeroTopN.isSelected());
//        params.setAddTopNComplementary(checkAddTopNComplementary.isSelected());
        params.setTrackZeroTopN((Integer)spinnerTrackZeroTopN.getValue());
        params.setZeroBinAcceptExpect((Double)spinnerZeroBinAcceptExpect.getValue());
        params.setZeroBinMultExpect((Double)spinnerZeroBinMultiplyExpect.getValue());
        params.setAddTopNComplementary((Integer)spinnerAddTopNComplementary.getValue());
        
        params.setAllowMultipleVariableModsOnResidue(checkMultipleVarMods.isSelected());
        params.setMaxVariableModsPerMod((Integer)spinnerMaxVarModsPerMod.getValue());
        params.setMaxVariableModsCombinations((Integer)spinnerMaxCombos.getValue());
        
        List<Mod> modsVar = tableModelVarMods.getModifications();
        params.setVariableMods(modsVar);
        
        List<Mod> modsAdd = tableModelAddMods.getModifications();
        params.setAdditionalMods(modsAdd);
    }
    
    private<T> T utilSpinnerValue(JSpinner spinner, Class<T> clazz) {
        return (T)spinner.getValue();
    }
    
    public MsfraggerParams getParamsFromForm() {
        throw new UnsupportedOperationException("Not implemented yet");
    }
    
    private synchronized TableModel getDefaultVarModTableModel() {
        if (tableModelVarMods != null)
            return tableModelVarMods;
        int cols = 3;
        Object[][] data = new Object[MsfraggerParams.VAR_MOD_COUNT_MAX][cols];
        for (int i = 0; i < data.length; i++) {
            data[i][0] = false;
            data[i][1] = null;
            data[i][2] = null;
        }

        tableModelVarMods = new ModificationsTableModel(
                TABLE_VAR_MODS_COL_NAMES,
                new Class<?>[] { Boolean.class, String.class, Double.class },
                new boolean[] {true, true, true},
                new int[] {0, 1, 2},
                data);

        return tableModelVarMods;
    }
    
    private synchronized TableModel getDefaultAddonTableModel() {
        if (tableModelAddMods != null)
            return tableModelAddMods;
        
        int cols = 3;
        Object[][] data = new Object[MsfraggerParams.ADDONS_HUMAN_READABLE.length][cols];
        for (int i = 0; i < data.length; i++) {
            data[i][0] = false;
            data[i][1] = MsfraggerParams.ADDONS_HUMAN_READABLE[i];
            data[i][2] = 0.0;
        }

        tableModelAddMods = new ModificationsTableModel(
                TABLE_ADD_MODS_COL_NAMES,
                new Class<?>[] {Boolean.class, String.class, Double.class},
                new boolean[] {true, false, true},
                new int[] {0, 1, 2},
                data);
        
        return tableModelAddMods;
    }
    
    private void updateRowHeights(JTable table) {
        for (int row = 0; row < table.getRowCount(); row++) {
            int rowHeight = table.getRowHeight();

            for (int column = 0; column < table.getColumnCount(); column++) {
                Component comp = table.prepareRenderer(table.getCellRenderer(row, column), row, column);
                rowHeight = Math.max(rowHeight, comp.getPreferredSize().height);
            }

            table.setRowHeight(row, rowHeight);
        }
    }
    
    /**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        panelMsFragger = new javax.swing.JPanel();
        panelMsfraggerParams = new javax.swing.JPanel();
        jPanel1 = new javax.swing.JPanel();
        comboCleavage = new javax.swing.JComboBox<>();
        jLabel11 = new javax.swing.JLabel();
        jLabel8 = new javax.swing.JLabel();
        textEnzymeName = new javax.swing.JTextField();
        jLabel9 = new javax.swing.JLabel();
        textCutAfter = new javax.swing.JTextField();
        jLabel10 = new javax.swing.JLabel();
        textButNotAfter = new javax.swing.JTextField();
        jLabel7 = new javax.swing.JLabel();
        spinnerMissedCleavages = new javax.swing.JSpinner();
        checkClipNTerm = new javax.swing.JCheckBox();
        jLabel14 = new javax.swing.JLabel();
        spinnerDigestLenMin = new javax.swing.JSpinner();
        jLabel15 = new javax.swing.JLabel();
        spinnerDigestLenMax = new javax.swing.JSpinner();
        jLabel16 = new javax.swing.JLabel();
        spinnerDigestMassMin = new javax.swing.JSpinner();
        jLabel17 = new javax.swing.JLabel();
        spinnerDigestMassMax = new javax.swing.JSpinner();
        jLabel18 = new javax.swing.JLabel();
        spinnerMaxFragCharge = new javax.swing.JSpinner();
        spinnerSlices = new javax.swing.JSpinner();
        lblSlices = new javax.swing.JLabel();
        jPanel2 = new javax.swing.JPanel();
        checkMultipleVarMods = new javax.swing.JCheckBox();
        jLabel12 = new javax.swing.JLabel();
        spinnerMaxVarModsPerMod = new javax.swing.JSpinner();
        jLabel13 = new javax.swing.JLabel();
        spinnerMaxCombos = new javax.swing.JSpinner();
        jScrollPane1 = new javax.swing.JScrollPane();
        tableVarMods = new javax.swing.JTable();
        jLabel31 = new javax.swing.JLabel();
        jLabel32 = new javax.swing.JLabel();
        jScrollPane2 = new javax.swing.JScrollPane();
        tableAdditionalMods = new javax.swing.JTable();
        jPanel3 = new javax.swing.JPanel();
        jLabel19 = new javax.swing.JLabel();
        spinnerMinPeaks = new javax.swing.JSpinner();
        jLabel21 = new javax.swing.JLabel();
        spinnerMinFragsModelling = new javax.swing.JSpinner();
        jLabel23 = new javax.swing.JLabel();
        spinnerMinRatio = new javax.swing.JSpinner();
        jLabel20 = new javax.swing.JLabel();
        spinnerUseTopNPeaks = new javax.swing.JSpinner();
        jLabel22 = new javax.swing.JLabel();
        jLabel24 = new javax.swing.JLabel();
        spinnerMinMatchedFrags = new javax.swing.JSpinner();
        spinnerClearMzRangeMin = new javax.swing.JSpinner();
        jLabel26 = new javax.swing.JLabel();
        spinnerClearMzRangeMax = new javax.swing.JSpinner();
        jPanel4 = new javax.swing.JPanel();
        spinnerTrackZeroTopN = new javax.swing.JSpinner();
        lblTrackZeroTopN = new javax.swing.JLabel();
        lblZeroBinAcceptExpect = new javax.swing.JLabel();
        spinnerZeroBinAcceptExpect = new javax.swing.JSpinner();
        lblZeroBinMultiplyExpect = new javax.swing.JLabel();
        spinnerZeroBinMultiplyExpect = new javax.swing.JSpinner();
        lblAddTopNComplementary = new javax.swing.JLabel();
        spinnerAddTopNComplementary = new javax.swing.JSpinner();
        spinnerFraggerThreads = new javax.swing.JSpinner();
        lblThreads = new javax.swing.JLabel();
        spinnerFraggerRam = new javax.swing.JSpinner();
        lblRam = new javax.swing.JLabel();
        jLabel35 = new javax.swing.JLabel();
        panelFraggerMatchingConfig = new javax.swing.JPanel();
        jPanel6 = new javax.swing.JPanel();
        jLabel4 = new javax.swing.JLabel();
        comboPrecursorTrueTol = new javax.swing.JComboBox<>();
        jLabel5 = new javax.swing.JLabel();
        comboFragMassTol = new javax.swing.JComboBox<>();
        spinnerPrecursorTrueTol = new javax.swing.JSpinner();
        spinnerFragMassTol = new javax.swing.JSpinner();
        jPanel7 = new javax.swing.JPanel();
        jLabel6 = new javax.swing.JLabel();
        textIsotopeError = new javax.swing.JTextField();
        jLabel29 = new javax.swing.JLabel();
        spinnerPrecursorChargeLo = new javax.swing.JSpinner();
        jLabel36 = new javax.swing.JLabel();
        spinnerPrecursorChargeHi = new javax.swing.JSpinner();
        checkOverrideCharge = new javax.swing.JCheckBox();
        jLabel25 = new javax.swing.JLabel();
        comboFraggerOutputType = new javax.swing.JComboBox<>();
        jLabel1 = new javax.swing.JLabel();
        textMassOffsets = new javax.swing.JTextField();
        jPanel8 = new javax.swing.JPanel();
        spinnerReportTopN = new javax.swing.JSpinner();
        spinnerOutputMaxExpect = new javax.swing.JSpinner();
        jLabel27 = new javax.swing.JLabel();
        jLabel28 = new javax.swing.JLabel();
        jPanel9 = new javax.swing.JPanel();
        jLabel3 = new javax.swing.JLabel();
        comboPrecursorMassTol = new javax.swing.JComboBox<>();
        spinnerPrecursorMassTolLo = new javax.swing.JSpinner();
        jLabel2 = new javax.swing.JLabel();
        spinnerPrecursorMassTolHi = new javax.swing.JSpinner();
        chkMsadjuster = new javax.swing.JCheckBox();
        btnSave = new javax.swing.JButton();
        btnLoad = new javax.swing.JButton();
        chkRunMsfragger = new javax.swing.JCheckBox();
        btnMsfraggerDefaultsClosed = new javax.swing.JButton();
        btnMsfraggerDefaultsOpen = new javax.swing.JButton();

        panelMsFragger.addComponentListener(new java.awt.event.ComponentAdapter() {
            public void componentShown(java.awt.event.ComponentEvent evt) {
                panelMsFraggerComponentShown(evt);
            }
        });

        panelMsfraggerParams.setBorder(javax.swing.BorderFactory.createTitledBorder("Options"));

        jPanel1.setBorder(javax.swing.BorderFactory.createTitledBorder("Digest"));

        comboCleavage.setModel(createCleavageComboBoxModel());

        jLabel11.setText("Cleavage");

        jLabel8.setText("Enzyme Name");

        textEnzymeName.setText("Trypsin");
        textEnzymeName.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                textEnzymeNameActionPerformed(evt);
            }
        });

        jLabel9.setText("Cut After");

        textCutAfter.setDocument(DocumentFilters.getFilter("[^A-Z]+"));
        textCutAfter.setText("KR");
        textCutAfter.setToolTipText("Enzyme cleaves after these residues");

        jLabel10.setText("But Not After");

        textButNotAfter.setDocument(DocumentFilters.getFilter("[^A-Z]+"));
        textButNotAfter.setText("P");
        textButNotAfter.setToolTipText("Enzyme cleaves if previous residues are not followed by this one");

        jLabel7.setText("Missed Cleavages");

        spinnerMissedCleavages.setModel(new javax.swing.SpinnerNumberModel(1, 0, 5, 1));

        checkClipNTerm.setSelected(true);
        checkClipNTerm.setText("Clip N-Term M");
        checkClipNTerm.setToolTipText("<html>Specifies the trimming of a protein N-terminal<br>\nmethionine as a variable modification");

        jLabel14.setText("Digest Length");

        spinnerDigestLenMin.setModel(new javax.swing.SpinnerNumberModel(5, 3, null, 1));
        spinnerDigestLenMin.setToolTipText("Min Peptide Length");

        jLabel15.setText("-");

        spinnerDigestLenMax.setModel(new javax.swing.SpinnerNumberModel(60, 3, null, 1));
        spinnerDigestLenMax.setToolTipText("Max Peptide Length");

        jLabel16.setText("Digest mass range");

        spinnerDigestMassMin.setModel(new javax.swing.SpinnerNumberModel(500.0d, 0.0d, null, 500.0d));

        jLabel17.setText("-");

        spinnerDigestMassMax.setModel(new javax.swing.SpinnerNumberModel(7000.0d, 0.0d, 7000.0d, 500.0d));

        jLabel18.setText("Max Fragment Charge");

        spinnerMaxFragCharge.setModel(new javax.swing.SpinnerNumberModel(2, 0, null, 1));

        spinnerSlices.setModel(new javax.swing.SpinnerNumberModel(1, 1, null, 1));
        spinnerSlices.setToolTipText("<html>Split database into smaller chunks.<br/>\nIf you're wondering what's that for, you likely don't need it.<br/>\nRequires MSFragger 20180924+.");

        lblSlices.setFont(new java.awt.Font("Tahoma", 2, 11)); // NOI18N
        lblSlices.setText("Slice up database");
        lblSlices.setToolTipText("<html>Split database into smaller chunks.<br/>\nIf you're wondering what's that for, you likely don't need it.<br/>\nRequires MSFragger 20180924+.");

        javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel1);
        jPanel1.setLayout(jPanel1Layout);
        jPanel1Layout.setHorizontalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
                    .addGroup(jPanel1Layout.createSequentialGroup()
                        .addContainerGap()
                        .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                            .addComponent(jLabel14)
                            .addComponent(jLabel11)
                            .addComponent(jLabel8))
                        .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addGroup(jPanel1Layout.createSequentialGroup()
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(textEnzymeName, javax.swing.GroupLayout.PREFERRED_SIZE, 121, javax.swing.GroupLayout.PREFERRED_SIZE))
                            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel1Layout.createSequentialGroup()
                                .addGap(7, 7, 7)
                                .addComponent(comboCleavage, javax.swing.GroupLayout.PREFERRED_SIZE, 119, javax.swing.GroupLayout.PREFERRED_SIZE)))
                        .addGap(18, 18, 18)
                        .addComponent(jLabel9)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(textCutAfter, javax.swing.GroupLayout.PREFERRED_SIZE, 50, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(jLabel10)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(textButNotAfter, javax.swing.GroupLayout.PREFERRED_SIZE, 50, javax.swing.GroupLayout.PREFERRED_SIZE))
                    .addGroup(jPanel1Layout.createSequentialGroup()
                        .addGap(47, 47, 47)
                        .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                            .addGroup(jPanel1Layout.createSequentialGroup()
                                .addComponent(jLabel7)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(spinnerMissedCleavages, javax.swing.GroupLayout.PREFERRED_SIZE, 50, javax.swing.GroupLayout.PREFERRED_SIZE))
                            .addGroup(jPanel1Layout.createSequentialGroup()
                                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                                    .addGroup(jPanel1Layout.createSequentialGroup()
                                        .addComponent(spinnerDigestLenMin, javax.swing.GroupLayout.PREFERRED_SIZE, 66, javax.swing.GroupLayout.PREFERRED_SIZE)
                                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                        .addComponent(jLabel15))
                                    .addComponent(jLabel18))
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
                                    .addComponent(spinnerMaxFragCharge)
                                    .addComponent(spinnerDigestLenMax, javax.swing.GroupLayout.DEFAULT_SIZE, 66, Short.MAX_VALUE))
                                .addGap(18, 18, 18)
                                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
                                    .addGroup(jPanel1Layout.createSequentialGroup()
                                        .addComponent(jLabel16)
                                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                        .addComponent(spinnerDigestMassMin, javax.swing.GroupLayout.PREFERRED_SIZE, 70, javax.swing.GroupLayout.PREFERRED_SIZE)
                                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                        .addComponent(jLabel17, javax.swing.GroupLayout.PREFERRED_SIZE, 4, javax.swing.GroupLayout.PREFERRED_SIZE)
                                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                        .addComponent(spinnerDigestMassMax, javax.swing.GroupLayout.PREFERRED_SIZE, 70, javax.swing.GroupLayout.PREFERRED_SIZE))
                                    .addGroup(jPanel1Layout.createSequentialGroup()
                                        .addComponent(checkClipNTerm)
                                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                        .addComponent(lblSlices)
                                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                        .addComponent(spinnerSlices, javax.swing.GroupLayout.PREFERRED_SIZE, 45, javax.swing.GroupLayout.PREFERRED_SIZE)))))))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
        jPanel1Layout.setVerticalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel8)
                    .addComponent(textEnzymeName, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel9)
                    .addComponent(textCutAfter, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel10)
                    .addComponent(textButNotAfter, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(comboCleavage, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel11)
                    .addComponent(jLabel7)
                    .addComponent(spinnerMissedCleavages, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel14)
                    .addComponent(spinnerDigestLenMin, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel15)
                    .addComponent(spinnerDigestLenMax, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel16)
                    .addComponent(spinnerDigestMassMin, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel17)
                    .addComponent(spinnerDigestMassMax, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel18)
                    .addComponent(spinnerMaxFragCharge, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(checkClipNTerm)
                    .addComponent(spinnerSlices, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(lblSlices)))
        );

        jPanel2.setBorder(javax.swing.BorderFactory.createTitledBorder("Modifications"));

        checkMultipleVarMods.setSelected(true);
        checkMultipleVarMods.setText("Multiple variable mods on residue");

        jLabel12.setText("Max var mods per mod");

        spinnerMaxVarModsPerMod.setModel(new javax.swing.SpinnerNumberModel(2, 0, 5, 1));

        jLabel13.setText("Max combos");

        spinnerMaxCombos.setModel(new javax.swing.SpinnerNumberModel(5000, 1, 65534, 50));

        tableVarMods.setModel(getDefaultVarModTableModel());
        tableVarMods.setToolTipText("<html>Variable Modifications.<br/>\nValues:<br/>\n<ul>\n<li>A-Z amino acid codes</li>\n<li>*​ ​is​ ​used​ ​to​ ​represent​ ​any​ ​amino​ ​acid</li>\n<li>^​ ​is​ ​used​ ​to​ ​represent​ ​a​ ​terminus</li>\n<li>[​ ​is​ ​a​ ​modifier​ ​for​ ​protein​ ​N-terminal</li>\n<li>]​ ​is​ ​a​ ​modifier​ ​for​ ​protein​ ​C-terminal</li>\n<li>n​ ​is​ ​a​ ​modifier​ ​for​ ​peptide​ ​N-terminal</li>\n<li>c​ ​is​ ​a​ ​modifier​ ​for​ ​peptide​ ​C-terminal</li>\n</ul>\nSyntax​ ​Examples:\n<ul>\n<li>15.9949​ ​M​ ​(for​ ​oxidation​ ​on​ ​methionine)</li>\n<li>79.66331​ ​STY​ ​(for​ ​phosphorylation)</li>\n<li>-17.0265​ ​nQnC​ ​(for​ ​pyro-Glu​ ​or​ ​loss​ ​of​ ​ammonia​ ​at peptide​ ​N-terminal)</li>\n</ul>\nExample​ ​(M​ ​oxidation​ ​and​ ​N-terminal​ ​acetylation):\n<ul>\n<li>variable_mod_01​ ​=​ ​15.9949​ ​M</li>\n<li>variable_mod_02​ ​=​ ​42.0106​ ​[^</li>\n</ul>");
        jScrollPane1.setViewportView(tableVarMods);

        jLabel31.setText("Variable Modifications");

        jLabel32.setText("Fixed Modifications");

        tableAdditionalMods.setModel(getDefaultAddonTableModel());
        jScrollPane2.setViewportView(tableAdditionalMods);

        javax.swing.GroupLayout jPanel2Layout = new javax.swing.GroupLayout(jPanel2);
        jPanel2.setLayout(jPanel2Layout);
        jPanel2Layout.setHorizontalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel2Layout.createSequentialGroup()
                        .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jLabel31)
                            .addComponent(jLabel32))
                        .addGap(0, 0, Short.MAX_VALUE))
                    .addGroup(jPanel2Layout.createSequentialGroup()
                        .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jScrollPane1)
                            .addComponent(jScrollPane2)
                            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel2Layout.createSequentialGroup()
                                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                    .addGroup(jPanel2Layout.createSequentialGroup()
                                        .addComponent(jLabel12)
                                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                        .addComponent(spinnerMaxVarModsPerMod, javax.swing.GroupLayout.PREFERRED_SIZE, 50, javax.swing.GroupLayout.PREFERRED_SIZE)
                                        .addGap(18, 18, 18)
                                        .addComponent(jLabel13))
                                    .addComponent(checkMultipleVarMods))
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(spinnerMaxCombos, javax.swing.GroupLayout.PREFERRED_SIZE, 70, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addGap(0, 248, Short.MAX_VALUE)))
                        .addContainerGap())))
        );
        jPanel2Layout.setVerticalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addComponent(checkMultipleVarMods)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel12)
                    .addComponent(spinnerMaxVarModsPerMod, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel13)
                    .addComponent(spinnerMaxCombos, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addGap(18, 18, 18)
                .addComponent(jLabel31)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jScrollPane1, javax.swing.GroupLayout.PREFERRED_SIZE, 160, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(18, 18, 18)
                .addComponent(jLabel32)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jScrollPane2, javax.swing.GroupLayout.DEFAULT_SIZE, 251, Short.MAX_VALUE)
                .addContainerGap())
        );

        jPanel3.setBorder(javax.swing.BorderFactory.createTitledBorder("Spectral Processing"));

        jLabel19.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
        jLabel19.setText("Min peaks");

        spinnerMinPeaks.setModel(new javax.swing.SpinnerNumberModel(6, 0, null, 1));
        spinnerMinPeaks.setToolTipText("required minimum number of peaks in spectrum to search (default 10)");

        jLabel21.setText("Min Frags Modelling");
        jLabel21.setToolTipText("<html>Minimum number of matched peaks in PSM for<br>\ninclusion in statistical modeling");

        spinnerMinFragsModelling.setToolTipText("<html>Minimum number of matched peaks in PSM for<br>\ninclusion in statistical modeling");

        jLabel23.setText("Min Ratio");
        jLabel23.setToolTipText("filter peaks below this fraction of strongest peak");

        spinnerMinRatio.setModel(new javax.swing.SpinnerNumberModel(0.01d, 0.0d, null, 0.1d));
        spinnerMinRatio.setToolTipText("filter peaks below this fraction of strongest peak");

        jLabel20.setText("Use Top N Peaks");

        spinnerUseTopNPeaks.setModel(new javax.swing.SpinnerNumberModel(100, 0, null, 10));

        jLabel22.setText("Min Matched Frags");

        jLabel24.setText("Clear m/z range");
        jLabel24.setToolTipText("for iTRAQ/TMT type data; will clear out all peaks in the specified m/z range");

        spinnerMinMatchedFrags.setModel(new javax.swing.SpinnerNumberModel(6, 0, null, 1));

        spinnerClearMzRangeMin.setModel(new javax.swing.SpinnerNumberModel(0.0d, 0.0d, null, 10.0d));
        spinnerClearMzRangeMin.setToolTipText("for iTRAQ/TMT type data; will clear out all peaks in the specified m/z range");

        jLabel26.setText("-");

        spinnerClearMzRangeMax.setModel(new javax.swing.SpinnerNumberModel(0.0d, 0.0d, null, 10.0d));
        spinnerClearMzRangeMax.setToolTipText("for iTRAQ/TMT type data; will clear out all peaks in the specified m/z range");

        javax.swing.GroupLayout jPanel3Layout = new javax.swing.GroupLayout(jPanel3);
        jPanel3.setLayout(jPanel3Layout);
        jPanel3Layout.setHorizontalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addComponent(jLabel23)
                    .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING, false)
                        .addComponent(jLabel19, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(jLabel21, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addGroup(jPanel3Layout.createSequentialGroup()
                        .addComponent(spinnerMinRatio, javax.swing.GroupLayout.PREFERRED_SIZE, 70, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addGap(33, 33, 33)
                        .addComponent(jLabel24))
                    .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
                        .addGroup(jPanel3Layout.createSequentialGroup()
                            .addComponent(spinnerMinPeaks, javax.swing.GroupLayout.PREFERRED_SIZE, 70, javax.swing.GroupLayout.PREFERRED_SIZE)
                            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                            .addComponent(jLabel20))
                        .addGroup(jPanel3Layout.createSequentialGroup()
                            .addComponent(spinnerMinFragsModelling, javax.swing.GroupLayout.PREFERRED_SIZE, 70, javax.swing.GroupLayout.PREFERRED_SIZE)
                            .addGap(18, 18, 18)
                            .addComponent(jLabel22))))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
                    .addComponent(spinnerUseTopNPeaks, javax.swing.GroupLayout.DEFAULT_SIZE, 70, Short.MAX_VALUE)
                    .addComponent(spinnerClearMzRangeMin)
                    .addComponent(spinnerMinMatchedFrags))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jLabel26)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(spinnerClearMzRangeMax, javax.swing.GroupLayout.PREFERRED_SIZE, 70, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
        jPanel3Layout.setVerticalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(spinnerMinPeaks, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel19)
                    .addComponent(jLabel20)
                    .addComponent(spinnerUseTopNPeaks, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel21)
                    .addComponent(spinnerMinFragsModelling, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel22)
                    .addComponent(spinnerMinMatchedFrags, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel23)
                    .addComponent(spinnerMinRatio, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel24)
                    .addComponent(spinnerClearMzRangeMin, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel26)
                    .addComponent(spinnerClearMzRangeMax, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jPanel4.setBorder(javax.swing.BorderFactory.createTitledBorder("Open Search Parameters"));

        spinnerTrackZeroTopN.setModel(new javax.swing.SpinnerNumberModel(0, 0, null, 5));
        spinnerTrackZeroTopN.setToolTipText(lblTrackZeroTopN.getToolTipText());

        lblTrackZeroTopN.setText("Track Zero Top N");
        lblTrackZeroTopN.setToolTipText("<html>Track top N unmodified peptide results separately<br/>\nfrom main results internally for boosting features.<br/>\nShould be set to a number greater than<br/>\noutput_report_topN if zero bin boosting is desired<br/>"); // NOI18N

        lblZeroBinAcceptExpect.setText("Zero Bin Accept Expect");
        lblZeroBinAcceptExpect.setToolTipText("<html>Ranks a zero-bin hit above all non-zero-bin hit if it<br/>\nhas expectation less than this value.");

        spinnerZeroBinAcceptExpect.setModel(new javax.swing.SpinnerNumberModel(0.0d, 0.0d, null, 0.1d));
        spinnerZeroBinAcceptExpect.setToolTipText(lblZeroBinAcceptExpect.getToolTipText());

        lblZeroBinMultiplyExpect.setText("Zero Bin Multiply Expect");
        lblZeroBinMultiplyExpect.setToolTipText("<html>Multiplies expect value of PSMs in the zero-bin<br/>\nduring results ordering (set to less than 1 for<br/>\nboosting)"); // NOI18N

        spinnerZeroBinMultiplyExpect.setModel(new javax.swing.SpinnerNumberModel(1.0d, 0.0d, 1.0d, 0.05d));
        spinnerZeroBinMultiplyExpect.setToolTipText(lblZeroBinMultiplyExpect.getToolTipText());

        lblAddTopNComplementary.setText("Add Top N Complementary");
        lblAddTopNComplementary.setToolTipText("<html>Inserts complementary ions corresponding to the top<br/>\nN most intense fragments in each experimental<br/>\nspectra. Useful for recovery of modified peptides<br/>\nnear C-terminal in open search. Should be set to 0<br/>\n(disabled) otherwise"); // NOI18N

        spinnerAddTopNComplementary.setModel(new javax.swing.SpinnerNumberModel(0, 0, null, 2));
        spinnerAddTopNComplementary.setToolTipText(lblAddTopNComplementary.getToolTipText());

        javax.swing.GroupLayout jPanel4Layout = new javax.swing.GroupLayout(jPanel4);
        jPanel4.setLayout(jPanel4Layout);
        jPanel4Layout.setHorizontalGroup(
            jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel4Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(lblZeroBinAcceptExpect)
                    .addComponent(lblTrackZeroTopN))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addGroup(jPanel4Layout.createSequentialGroup()
                        .addComponent(spinnerZeroBinAcceptExpect, javax.swing.GroupLayout.PREFERRED_SIZE, 75, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addGap(24, 24, 24)
                        .addComponent(lblZeroBinMultiplyExpect))
                    .addGroup(jPanel4Layout.createSequentialGroup()
                        .addComponent(spinnerTrackZeroTopN, javax.swing.GroupLayout.PREFERRED_SIZE, 75, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                        .addComponent(lblAddTopNComplementary)))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
                    .addComponent(spinnerZeroBinMultiplyExpect, javax.swing.GroupLayout.DEFAULT_SIZE, 75, Short.MAX_VALUE)
                    .addComponent(spinnerAddTopNComplementary))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
        jPanel4Layout.setVerticalGroup(
            jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel4Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(spinnerTrackZeroTopN, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(lblTrackZeroTopN)
                    .addComponent(lblAddTopNComplementary)
                    .addComponent(spinnerAddTopNComplementary, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(lblZeroBinAcceptExpect)
                    .addComponent(spinnerZeroBinAcceptExpect, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(lblZeroBinMultiplyExpect)
                    .addComponent(spinnerZeroBinMultiplyExpect, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap(12, Short.MAX_VALUE))
        );

        spinnerFraggerThreads.setModel(new javax.swing.SpinnerNumberModel(0, 0, null, 1));
        spinnerFraggerThreads.setToolTipText(lblThreads.getToolTipText());

        lblThreads.setText("Threads");
        lblThreads.setToolTipText("<html>0 means auto-identify the number of processing cores available.");

        spinnerFraggerRam.setModel(new javax.swing.SpinnerNumberModel(0, 0, null, 1));
        spinnerFraggerRam.setToolTipText(lblRam.getToolTipText());

        lblRam.setText("RAM");
        lblRam.setToolTipText("<html>When set to 0 won't pass -Xmx parameter when starting JVM.<br/>\nIf unsure, just leave it at 0.");

        jLabel35.setText("GB");

        panelFraggerMatchingConfig.setBorder(javax.swing.BorderFactory.createTitledBorder("Peak Matching Configuration"));

        jLabel4.setText("Precursor True Tolerance");
        jLabel4.setToolTipText("<html>True precursor mass tolerance <br>\nshould be set to your instrument's precursor mass accuracy <br>\n(window is +/- this value).  This value is used for tie breaking <br>\nof results and boosting of unmodified peptides in open search.<br>");

        comboPrecursorTrueTol.setModel(createMassToleranceUnitsComboModel());
        comboPrecursorTrueTol.setToolTipText("<html>True precursor mass tolerance <br>\nshould be set to your instrument's precursor mass accuracy <br>\n(window is +/- this value).  This value is used for tie breaking <br>\nof results and boosting of unmodified peptides in open search.<br>");
        comboPrecursorTrueTol.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                comboPrecursorTrueTolActionPerformed(evt);
            }
        });

        jLabel5.setText("Fragment Mass Tolerance");

        comboFragMassTol.setModel(createMassToleranceUnitsComboModel());
        comboFragMassTol.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                comboFragMassTolActionPerformed(evt);
            }
        });

        spinnerPrecursorTrueTol.setModel(new javax.swing.SpinnerNumberModel(50.0d, 0.0d, null, 5.0d));
        spinnerPrecursorTrueTol.setToolTipText("<html>True precursor mass tolerance <br>\nshould be set to your instrument's precursor mass accuracy <br>\n(window is +/- this value).  This value is used for tie breaking <br>\nof results and boosting of unmodified peptides in open search.<br>");

        spinnerFragMassTol.setModel(new javax.swing.SpinnerNumberModel(50.0d, 0.0d, null, 5.0d));

        javax.swing.GroupLayout jPanel6Layout = new javax.swing.GroupLayout(jPanel6);
        jPanel6.setLayout(jPanel6Layout);
        jPanel6Layout.setHorizontalGroup(
            jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel6Layout.createSequentialGroup()
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addGroup(jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel6Layout.createSequentialGroup()
                        .addComponent(jLabel4, javax.swing.GroupLayout.PREFERRED_SIZE, 123, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(comboPrecursorTrueTol, javax.swing.GroupLayout.PREFERRED_SIZE, 66, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(spinnerPrecursorTrueTol, javax.swing.GroupLayout.PREFERRED_SIZE, 55, javax.swing.GroupLayout.PREFERRED_SIZE))
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel6Layout.createSequentialGroup()
                        .addComponent(jLabel5)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(comboFragMassTol, javax.swing.GroupLayout.PREFERRED_SIZE, 66, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(spinnerFragMassTol, javax.swing.GroupLayout.PREFERRED_SIZE, 55, javax.swing.GroupLayout.PREFERRED_SIZE)))
                .addContainerGap())
        );
        jPanel6Layout.setVerticalGroup(
            jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel6Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(spinnerPrecursorTrueTol, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(comboPrecursorTrueTol, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel4))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(spinnerFragMassTol, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(comboFragMassTol, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel5))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jLabel6.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
        jLabel6.setText("Isotope Error");
        jLabel6.setToolTipText("<html>0=off, -1/0/1/2/3 (standard C13 error)<br><br>\nIsotope correction for MS/MS events triggered on<br>\nisotopic peaks. Should be set to 0 (disabled) for open<br>\nsearch or 0/1/2 for correction of narrow window<br>\nsearches. Shifts the precursor mass window to<br>\nmultiples of this value multiplied by the mass difference<br>\nof C13-C12.");

        textIsotopeError.setDocument(getFilterIsotopeCorrection());
        textIsotopeError.setText("-1/0/1/2");
        textIsotopeError.setToolTipText("<html>0=off, -1/0/1/2/3 (standard C13 error)<br><br>\nIsotope correction for MS/MS events triggered on<br>\nisotopic peaks. Should be set to 0 (disabled) for open<br>\nsearch or 0/1/2 for correction of narrow window<br>\nsearches. Shifts the precursor mass window to<br>\nmultiples of this value multiplied by the mass difference<br>\nof C13-C12.");
        textIsotopeError.addFocusListener(new java.awt.event.FocusAdapter() {
            public void focusGained(java.awt.event.FocusEvent evt) {
                textIsotopeErrorFocusGained(evt);
            }
            public void focusLost(java.awt.event.FocusEvent evt) {
                textIsotopeErrorFocusLost(evt);
            }
        });

        jLabel29.setText("Precursor Charge");
        jLabel29.setToolTipText("<html>Assume range of potential precursor charge states.<br>\nOnly relevant when override_charge is set to 1.<br>\nSpecified as space separated range of integers.<br>");

        spinnerPrecursorChargeLo.setToolTipText("<html>Assume range of potential precursor charge states.<br>\nOnly relevant when override_charge is set to 1.<br>\nSpecified as space separated range of integers.<br>");

        jLabel36.setText("-");

        spinnerPrecursorChargeHi.setToolTipText("<html>Assume range of potential precursor charge states.<br>\nOnly relevant when override_charge is set to 1.<br>\nSpecified as space separated range of integers.<br>");

        checkOverrideCharge.setText("Override Charge");
        checkOverrideCharge.setToolTipText("<html>Ignores precursor charge and uses charge state<br>\nspecified in precursor_charge range.<br>");

        jLabel25.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
        jLabel25.setText("Output Type");
        jLabel25.setToolTipText("<html>How the search results are to be reported.<br>\nDownstream tools only support PepXML format.<br><br>\nOnly use TSV (tab delimited file) if you want to process <br>\nsearch resutls yourself for easier import into other software.<br>");

        comboFraggerOutputType.setModel(createOutputFormatComboModel());
        comboFraggerOutputType.setToolTipText("<html>How the search results are to be reported.<br>\nDownstream tools only support PepXML format.<br><br>\nOnly use TSV (tab delimited file) if you want to process <br>\nsearch resutls yourself for easier import into other software.<br>");

        jLabel1.setText("Mass Offsets");
        jLabel1.setToolTipText("<html>Mass_offsets in MSFragger creates multiple precursor tolerance windows with<br>\nspecified mass offsets. These values are multiplexed with the isotope error option. <br><br>\n\nFor example, mass_offsets = 0/79.966 can be used as a restricted \"open\" search that <br>\nlooks for unmodified and phosphorylated peptides (on any residue).<br><br>\n\nSetting isotope_error to 0/1/2 in combination with this example will create <br>\nsearch windows around (0,1,2,79.966, 80.966, 81.966).");

        textMassOffsets.setToolTipText("<html>Mass_offsets in MSFragger creates multiple precursor tolerance windows with<br> specified mass offsets. These values are multiplexed with the isotope error option. <br><br>  For example, mass_offsets = 0/79.966 can be used as a restricted \"open\" search that <br> looks for unmodified and phosphorylated peptides (on any residue).<br><br>  Setting isotope_error to 0/1/2 in combination with this example will create <br> search windows around (0,1,2,79.966, 80.966, 81.966).");
        textMassOffsets.addFocusListener(new java.awt.event.FocusAdapter() {
            public void focusGained(java.awt.event.FocusEvent evt) {
                textMassOffsetsFocusGained(evt);
            }
            public void focusLost(java.awt.event.FocusEvent evt) {
                textMassOffsetsFocusLost(evt);
            }
        });

        javax.swing.GroupLayout jPanel7Layout = new javax.swing.GroupLayout(jPanel7);
        jPanel7.setLayout(jPanel7Layout);
        jPanel7Layout.setHorizontalGroup(
            jPanel7Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel7Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel7Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addComponent(jLabel29, javax.swing.GroupLayout.PREFERRED_SIZE, 84, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel6, javax.swing.GroupLayout.PREFERRED_SIZE, 93, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel7Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
                    .addComponent(textIsotopeError, javax.swing.GroupLayout.PREFERRED_SIZE, 125, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addGroup(jPanel7Layout.createSequentialGroup()
                        .addComponent(spinnerPrecursorChargeLo, javax.swing.GroupLayout.PREFERRED_SIZE, 52, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jLabel36, javax.swing.GroupLayout.PREFERRED_SIZE, 6, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(spinnerPrecursorChargeHi)))
                .addGap(18, 18, 18)
                .addGroup(jPanel7Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
                    .addGroup(jPanel7Layout.createSequentialGroup()
                        .addComponent(checkOverrideCharge)
                        .addGap(18, 18, 18)
                        .addComponent(jLabel25))
                    .addGroup(jPanel7Layout.createSequentialGroup()
                        .addComponent(jLabel1)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(textMassOffsets)))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(comboFraggerOutputType, javax.swing.GroupLayout.PREFERRED_SIZE, 100, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
        jPanel7Layout.setVerticalGroup(
            jPanel7Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel7Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel7Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel6)
                    .addComponent(textIsotopeError, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel1)
                    .addComponent(textMassOffsets, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel7Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(spinnerPrecursorChargeLo, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel36)
                    .addComponent(spinnerPrecursorChargeHi, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(checkOverrideCharge)
                    .addComponent(jLabel29)
                    .addComponent(jLabel25)
                    .addComponent(comboFraggerOutputType, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        spinnerReportTopN.setModel(new javax.swing.SpinnerNumberModel(3, 1, null, 1));
        spinnerReportTopN.setToolTipText("Reports top N PSMs per input spectrum");

        spinnerOutputMaxExpect.setModel(new javax.swing.SpinnerNumberModel(50.0d, 0.0d, null, 1.0d));
        spinnerOutputMaxExpect.setToolTipText("<html>Suppresses reporting of PSM if top hit has<br> expectation greater than this threshold");

        jLabel27.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
        jLabel27.setText("Report Top N");
        jLabel27.setToolTipText("Reports top N PSMs per input spectrum");

        jLabel28.setText("Output Max Expect");
        jLabel28.setToolTipText("<html>Suppresses reporting of PSM if top hit has<br>\nexpectation greater than this threshold");

        javax.swing.GroupLayout jPanel8Layout = new javax.swing.GroupLayout(jPanel8);
        jPanel8.setLayout(jPanel8Layout);
        jPanel8Layout.setHorizontalGroup(
            jPanel8Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel8Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel8Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addGroup(jPanel8Layout.createSequentialGroup()
                        .addComponent(jLabel28)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(spinnerOutputMaxExpect, javax.swing.GroupLayout.PREFERRED_SIZE, 80, javax.swing.GroupLayout.PREFERRED_SIZE))
                    .addGroup(jPanel8Layout.createSequentialGroup()
                        .addComponent(jLabel27, javax.swing.GroupLayout.PREFERRED_SIZE, 93, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(spinnerReportTopN, javax.swing.GroupLayout.PREFERRED_SIZE, 80, javax.swing.GroupLayout.PREFERRED_SIZE)))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
        jPanel8Layout.setVerticalGroup(
            jPanel8Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel8Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel8Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel27)
                    .addComponent(spinnerReportTopN, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanel8Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(spinnerOutputMaxExpect, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel28))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jLabel3.setText("Precursor Mass Tolerance");

        comboPrecursorMassTol.setModel(createMassToleranceUnitsComboModel());
        comboPrecursorMassTol.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                comboPrecursorMassTolActionPerformed(evt);
            }
        });

        spinnerPrecursorMassTolLo.setModel(new javax.swing.SpinnerNumberModel(-20.0d, null, null, 10.0d));

        jLabel2.setText("-");

        spinnerPrecursorMassTolHi.setModel(new javax.swing.SpinnerNumberModel(20.0d, null, null, 10.0d));

        chkMsadjuster.setFont(new java.awt.Font("Tahoma", 2, 11)); // NOI18N
        chkMsadjuster.setText("Adjust precursor mass");
        chkMsadjuster.setToolTipText("<html>Try to trace MS1 signals, adjusting MS2 precursor mass.<br/>\nRequires MSFragger 20180924+.");
        chkMsadjuster.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                chkMsadjusterActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout jPanel9Layout = new javax.swing.GroupLayout(jPanel9);
        jPanel9.setLayout(jPanel9Layout);
        jPanel9Layout.setHorizontalGroup(
            jPanel9Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel9Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jLabel3)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(comboPrecursorMassTol, javax.swing.GroupLayout.PREFERRED_SIZE, 65, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(spinnerPrecursorMassTolLo, javax.swing.GroupLayout.PREFERRED_SIZE, 70, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jLabel2)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(spinnerPrecursorMassTolHi, javax.swing.GroupLayout.PREFERRED_SIZE, 70, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(18, 18, 18)
                .addComponent(chkMsadjuster)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
        jPanel9Layout.setVerticalGroup(
            jPanel9Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel9Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel9Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel3)
                    .addComponent(comboPrecursorMassTol, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(spinnerPrecursorMassTolLo, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel2)
                    .addComponent(spinnerPrecursorMassTolHi, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(chkMsadjuster))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        loadLastMsadjuster();

        javax.swing.GroupLayout panelFraggerMatchingConfigLayout = new javax.swing.GroupLayout(panelFraggerMatchingConfig);
        panelFraggerMatchingConfig.setLayout(panelFraggerMatchingConfigLayout);
        panelFraggerMatchingConfigLayout.setHorizontalGroup(
            panelFraggerMatchingConfigLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(panelFraggerMatchingConfigLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(panelFraggerMatchingConfigLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jPanel9, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jPanel7, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addGroup(panelFraggerMatchingConfigLayout.createSequentialGroup()
                        .addComponent(jPanel6, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                        .addComponent(jPanel8, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)))
                .addContainerGap())
        );
        panelFraggerMatchingConfigLayout.setVerticalGroup(
            panelFraggerMatchingConfigLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(panelFraggerMatchingConfigLayout.createSequentialGroup()
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addComponent(jPanel9, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(panelFraggerMatchingConfigLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addComponent(jPanel6, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jPanel8, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addGap(8, 8, 8)
                .addComponent(jPanel7, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
        );

        btnSave.setText("Save");
        btnSave.setToolTipText("<html>Save current parameters to a file.");
        btnSave.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnSaveActionPerformed(evt);
            }
        });

        btnLoad.setText("Load");
        btnLoad.setToolTipText("<html>Load a previously saved MSFragger parameter file.<br/>\nYou can find a copy of parameter files (fragger.params) in your old <br/>\nsearch results directories, they are copied there automatically.");
        btnLoad.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnLoadActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout panelMsfraggerParamsLayout = new javax.swing.GroupLayout(panelMsfraggerParams);
        panelMsfraggerParams.setLayout(panelMsfraggerParamsLayout);
        panelMsfraggerParamsLayout.setHorizontalGroup(
            panelMsfraggerParamsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jPanel2, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
            .addComponent(jPanel4, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
            .addComponent(jPanel3, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
            .addComponent(jPanel1, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
            .addGroup(panelMsfraggerParamsLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(btnSave)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(btnLoad)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addComponent(lblRam)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(spinnerFraggerRam, javax.swing.GroupLayout.PREFERRED_SIZE, 50, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jLabel35)
                .addGap(18, 18, 18)
                .addComponent(lblThreads)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(spinnerFraggerThreads, javax.swing.GroupLayout.PREFERRED_SIZE, 60, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap())
            .addComponent(panelFraggerMatchingConfig, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
        );
        panelMsfraggerParamsLayout.setVerticalGroup(
            panelMsfraggerParamsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(panelMsfraggerParamsLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(panelMsfraggerParamsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(spinnerFraggerThreads, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(lblThreads)
                    .addComponent(spinnerFraggerRam, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(lblRam)
                    .addComponent(jLabel35)
                    .addComponent(btnSave)
                    .addComponent(btnLoad))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(panelFraggerMatchingConfig, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(40, 40, 40)
                .addComponent(jPanel1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanel3, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanel4, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanel2, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addContainerGap())
        );

        chkRunMsfragger.setSelected(true);
        chkRunMsfragger.setText("Run MSFragger");
        chkRunMsfragger.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                chkRunMsfraggerActionPerformed(evt);
            }
        });

        btnMsfraggerDefaultsClosed.setText("Defaults Closed Search");
        btnMsfraggerDefaultsClosed.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnMsfraggerDefaultsClosedActionPerformed(evt);
            }
        });

        btnMsfraggerDefaultsOpen.setText("Defaults Open Search");
        btnMsfraggerDefaultsOpen.setToolTipText("Load default parameters");
        btnMsfraggerDefaultsOpen.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnMsfraggerDefaultsOpenActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout panelMsFraggerLayout = new javax.swing.GroupLayout(panelMsFragger);
        panelMsFragger.setLayout(panelMsFraggerLayout);
        panelMsFraggerLayout.setHorizontalGroup(
            panelMsFraggerLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(panelMsFraggerLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(panelMsFraggerLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(panelMsFraggerLayout.createSequentialGroup()
                        .addComponent(panelMsfraggerParams, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addGap(0, 0, Short.MAX_VALUE))
                    .addGroup(panelMsFraggerLayout.createSequentialGroup()
                        .addComponent(chkRunMsfragger)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(btnMsfraggerDefaultsOpen)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(btnMsfraggerDefaultsClosed)))
                .addContainerGap())
        );
        panelMsFraggerLayout.setVerticalGroup(
            panelMsFraggerLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, panelMsFraggerLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(panelMsFraggerLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(chkRunMsfragger)
                    .addComponent(btnMsfraggerDefaultsClosed)
                    .addComponent(btnMsfraggerDefaultsOpen))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(panelMsfraggerParams, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addContainerGap())
        );

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(panelMsFragger, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(panelMsFragger, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
        );
    }// </editor-fold>//GEN-END:initComponents

    private void chkRunMsfraggerActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_chkRunMsfraggerActionPerformed
        boolean selected = chkRunMsfragger.isSelected();
        Container[] comps;
        comps = new Container[] {
            panelMsfraggerParams
        };
        for (Container c : comps) {
            SwingUtils.enableComponents(c, selected);
        }
        
        if (frame != null) {
            MsfraggerGuiFrame f = frame.get();
            if (f != null) {
                f.validatePythonAndSlicingVersion();
                f.validateMsadjusterEligibility();
            }
        }
    }//GEN-LAST:event_chkRunMsfraggerActionPerformed

    private void btnLoadActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnLoadActionPerformed
        JFileChooser fc = new JFileChooser();
        fc.setApproveButtonText("Load");
        fc.setApproveButtonToolTipText("Load into the form");
        fc.setDialogTitle("Select saved file");
        fc.setMultiSelectionEnabled(false);
        
        fc.setAcceptAllFileFilterUsed(true);
        FileNameExtensionFilter filter = new FileNameExtensionFilter("Properties/Params", 
                "properties", "params", "para", "conf", "txt");
        fc.setFileFilter(filter);

        final String propName = ThisAppProps.PROP_FRAGGER_PARAMS_FILE_IN;
        ThisAppProps.load(propName, fc);
        
        Component parent = SwingUtils.findParentComponentForDialog(this);
        int saveResult = fc.showOpenDialog(parent);
        if (JFileChooser.APPROVE_OPTION == saveResult) {
            File selectedFile = fc.getSelectedFile();
            Path path = Paths.get(selectedFile.getAbsolutePath());
            ThisAppProps.save(propName, path.toString());
            
            if (Files.exists(path)) {
                try {
                    params.clear();
                    params.load(new FileInputStream(selectedFile), true);
                    fillFormFromParams(params);
                    params.save();
                } catch (Exception ex) {
                    JOptionPane.showMessageDialog(parent, "<html>Could not load the saved file: <br/>" + ex.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
                }
            } else {
                JOptionPane.showMessageDialog(parent, "<html>This is strange,<br/> "
                    + "but the file you chose to load doesn't exist anymore.", "Strange", JOptionPane.ERROR_MESSAGE);
            }
        }
    }//GEN-LAST:event_btnLoadActionPerformed

    private void btnSaveActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnSaveActionPerformed

        // first save the current state to a temp file
        try {
            fillParamsFromForm(params);
            params.save();
        } catch (IOException ex) {
            // don't need to show anything, worst case we can't save temp files
        }

        // now save the actual user's choice
        JFileChooser fc = new JFileChooser();
        fc.setApproveButtonText("Save");
        fc.setApproveButtonToolTipText("Save to a file");
        fc.setDialogTitle("Choose where params file should be saved");
        fc.setMultiSelectionEnabled(false);
        
        final String propName = ThisAppProps.PROP_FRAGGER_PARAMS_FILE_IN;
        ThisAppProps.load(propName, fc);
        
        fc.setSelectedFile(new File(MsfraggerParams.DEFAULT_FILE));
        Component parent = SwingUtils.findParentComponentForDialog(this);
        int saveResult = fc.showSaveDialog(parent);
        if (JFileChooser.APPROVE_OPTION == saveResult) {
            File selectedFile = fc.getSelectedFile();
            Path path = Paths.get(selectedFile.getAbsolutePath());
            ThisAppProps.save(propName, path.toString());
            
            // if exists, overwrite
            if (Files.exists(path)) {
                int overwrite = JOptionPane.showConfirmDialog(parent, "<html>File exists,<br/> overwrtie?", "Overwrite", JOptionPane.OK_CANCEL_OPTION);
                if (JOptionPane.OK_OPTION == overwrite) {
                    try {
                        Files.delete(path);
                    } catch (IOException ex) {
                        JOptionPane.showMessageDialog(parent, "Could not overwrite", "Overwrite", JOptionPane.ERROR_MESSAGE);
                        return;
                    }
                }
            }
            try {
                ThisAppProps.save(PROP_FILECHOOSER_LAST_PATH, path.toAbsolutePath().toString());
                MsfraggerParams saved = new MsfraggerParams();
                saved.load();               // load defaults
                fillParamsFromForm(saved);  // overwrite with data from form
                saved.save(new FileOutputStream(path.toFile()));
            } catch (IOException ex) {
                JOptionPane.showMessageDialog(parent, "<html>Could not save file: <br/>" + path.toString() +
                    "<br/>" + ex.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
                return;
            }
        }
    }//GEN-LAST:event_btnSaveActionPerformed

    private void btnMsfraggerDefaultsOpenActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnMsfraggerDefaultsOpenActionPerformed
        int confirmation = JOptionPane.showConfirmDialog(SwingUtils.findParentComponentForDialog(this),
            "Are you sure you want to load defaults for open search?\n"
                    + "It's a search with large precursor mass tolerance\n"
                    + "usually used to identify PTMs.", "Confirmation", JOptionPane.OK_CANCEL_OPTION);
        if (JOptionPane.OK_OPTION == confirmation) {
            loadDefaultsOpen();
            
            MsfraggerGuiFrame f = frame.get();
            if (f != null) {
                int confirmProphets = JOptionPane.showConfirmDialog(SwingUtils.findParentComponentForDialog(this),
                "Loaded MSFragger defaults for 'Open' search.\n"
                        + "Would you like to update Prophets' options as well?\n"
                        + "(Highly recommended, unless you're sure what you're doing)", "Confirmation", JOptionPane.OK_CANCEL_OPTION);
                if (JOptionPane.OK_OPTION == confirmProphets) {
                    f.loadDefaultsPeptideProphet(SearchTypeProp.open);
                    f.loadDefaultsProteinProphet(SearchTypeProp.open);
                }
            }
        }
    }//GEN-LAST:event_btnMsfraggerDefaultsOpenActionPerformed

    public void loadDefaultsOpen() {
        params.loadDefaultsOpenSearch();
        fillFormFromParams(params);
        loadDefaultsMsadjuster(SearchTypeProp.open);
    }
    
    private void textEnzymeNameActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_textEnzymeNameActionPerformed
        
    }//GEN-LAST:event_textEnzymeNameActionPerformed

    private void comboPrecursorTrueTolActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_comboPrecursorTrueTolActionPerformed
        
    }//GEN-LAST:event_comboPrecursorTrueTolActionPerformed

    private void comboPrecursorMassTolActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_comboPrecursorMassTolActionPerformed
        
    }//GEN-LAST:event_comboPrecursorMassTolActionPerformed

    public JCheckBox getCheckboxIsRunFragger() {
        return chkRunMsfragger;
    }
    
    private MsfraggerGuiFrame getMsfraggerGuiFrame() {
        MsfraggerGuiFrame f = frame.get();
        if (f == null)
            throw new IllegalStateException("validateFraggerDbPath() called while not attached to an MsfraggerGuiFrame.");
        return f;
    }
    
    public void enableDbSlicing(boolean enabled) {
        spinnerSlices.setEnabled(enabled);
        lblSlices.setEnabled(enabled);
        if (!enabled) {
            stateDbSlicing = (Integer)spinnerSlices.getValue();
            spinnerSlices.setValue(1);
        } else {
            spinnerSlices.setValue(stateDbSlicing);
        }
    }
    
    public void enableMsadjuster(boolean enabled) {
        chkMsadjuster.setEnabled(enabled);
    }
    
    private void validateFraggerDbPath() {
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                if (dbPathTip != null) {
                    dbPathTip.closeBalloon();
                    dbPathTip = null;
                }
                
                final JComponent anchor = panelFraggerMatchingConfig;
                String text = getMsfraggerGuiFrame().getFastaPath();
                
                if (StringUtils.isNullOrWhitespace(text)) {
                    dbPathTip = new BalloonTip(anchor, "Empty fasta path (see Sequence DB tab)!");
                    dbPathTip.setVisible(true);
                    return;
                }

                Path path = Paths.get(text).toAbsolutePath();
                if (!Files.exists(path)) {
                    dbPathTip = new BalloonTip(anchor, "File does not exist (see Sequence DB tab)!");
                    dbPathTip.setVisible(true);
                    return;
                }
                if (Files.isDirectory(path)) {
                    dbPathTip = new BalloonTip(anchor, "DB path should not be a directory (see Sequence DB tab)!");
                    dbPathTip.setVisible(true);
                    return;
                }
            }
        });
    }
    
    private void comboFragMassTolActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_comboFragMassTolActionPerformed
        
    }//GEN-LAST:event_comboFragMassTolActionPerformed

    private void btnMsfraggerDefaultsClosedActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnMsfraggerDefaultsClosedActionPerformed
        int confirmation = JOptionPane.showConfirmDialog(SwingUtils.findParentComponentForDialog(this),
            "Are you sure you want to load defaults for 'closed' search?\n"
                    + "It's a standard search with tight mass tolerances.", "Confirmation", JOptionPane.OK_CANCEL_OPTION);
        if (JOptionPane.OK_OPTION == confirmation) {
            loadDefaultsClosed();
            
            MsfraggerGuiFrame f = frame.get();
            if (f != null) {
                int confirmProphets = JOptionPane.showConfirmDialog(SwingUtils.findParentComponentForDialog(this),
                "Loaded MSFragger defaults for 'Closed' search.\n"
                        + "Would you like to update Prophets' options as well?\n"
                        + "(Highly recommended, unless you're sure what you're doing)", "Confirmation", JOptionPane.OK_CANCEL_OPTION);
                if (JOptionPane.OK_OPTION == confirmProphets) {
                    f.loadDefaultsPeptideProphet(SearchTypeProp.closed);
                    f.loadDefaultsProteinProphet(SearchTypeProp.closed);
                }
            }
        }
    }//GEN-LAST:event_btnMsfraggerDefaultsClosedActionPerformed

    private void panelMsFraggerComponentShown(java.awt.event.ComponentEvent evt) {//GEN-FIRST:event_panelMsFraggerComponentShown
        
    }//GEN-LAST:event_panelMsFraggerComponentShown

    private void textIsotopeErrorFocusLost(java.awt.event.FocusEvent evt) {//GEN-FIRST:event_textIsotopeErrorFocusLost
        // TODO add your handling code here:
    }//GEN-LAST:event_textIsotopeErrorFocusLost

    private void textIsotopeErrorFocusGained(java.awt.event.FocusEvent evt) {//GEN-FIRST:event_textIsotopeErrorFocusGained
        // TODO add your handling code here:
    }//GEN-LAST:event_textIsotopeErrorFocusGained

    private void textMassOffsetsFocusLost(java.awt.event.FocusEvent evt) {//GEN-FIRST:event_textMassOffsetsFocusLost
        // TODO add your handling code here:
    }//GEN-LAST:event_textMassOffsetsFocusLost

    private void textMassOffsetsFocusGained(java.awt.event.FocusEvent evt) {//GEN-FIRST:event_textMassOffsetsFocusGained
        // TODO add your handling code here:
    }//GEN-LAST:event_textMassOffsetsFocusGained

    private void chkMsadjusterActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_chkMsadjusterActionPerformed
        MsfraggerGuiFrame.save(chkMsadjuster, ThisAppProps.PROP_MSADJUSTER_USE);
    }//GEN-LAST:event_chkMsadjusterActionPerformed

    public boolean isMsadjuster() {
        return chkMsadjuster.isSelected();
    }
    
    public void loadLastMsadjuster() {
        if (!MsfraggerGuiFrame.load(chkMsadjuster, ThisAppProps.PROP_MSADJUSTER_USE)) {
            loadDefaultsMsadjuster(DEFAULT_TYPE);
        }
    }
    
    public void loadDefaultsMsadjuster(SearchTypeProp type) {
        MsfraggerGuiFrame.loadDefaults(chkMsadjuster, ThisAppProps.PROP_MSADJUSTER_USE, type);
    }
    
    public void loadDefaultsClosed() {
        params.loadDefaultsClosedSearch();
        fillFormFromParams(params);
        loadDefaultsMsadjuster(SearchTypeProp.closed);
    }
    
    public void loadDefaults(SearchTypeProp type) {
        switch (type) {
            case open:
                loadDefaultsOpen();
                break;
            case closed:
                loadDefaultsClosed();
                break;
            default:
                throw new AssertionError(type.name());
        }
    }
    
    /**
     * Tables need to be cleared separately as they may contain more rows
     * than would be filled by the new properties. So old 'ghost' entries
     * might be left at the end of the table in such a case.
     */
    private void clearFormTables() {
        Object[][] varModsData = new Object[MsfraggerParams.VAR_MOD_COUNT_MAX][3];
        // set defaults for all fields
        for (int i = 0; i < MsfraggerParams.VAR_MOD_COUNT_MAX; i++) {
            varModsData[i][0] = false;
            varModsData[i][1] = null;
            varModsData[i][2] = null;
        }
        tableModelVarMods.setDataVector(varModsData, TABLE_VAR_MODS_COL_NAMES);
        
        Object[][] addModsData = new Object[MsfraggerParams.ADDON_NAMES.length][3];
        for (int i = 0; i < MsfraggerParams.ADDON_NAMES.length; i++) {
            addModsData[i][0] = false;
            addModsData[i][1] = null;
            addModsData[i][2] = null;
        }
        tableModelAddMods.setDataVector(addModsData, TABLE_ADD_MODS_COL_NAMES);
    }
    
    public static PlainDocument getFilterIsotopeCorrection() {
        final String filteredCharsRegex = "[^\\-\\d/]";
        PlainDocument doc = new PlainDocument();
        doc.setDocumentFilter(new DocumentFilter() {
            @Override
            public void insertString(DocumentFilter.FilterBypass fb, int off, String str, AttributeSet attr)
                    throws BadLocationException {
                str = str.replaceAll(filteredCharsRegex, "");
                fb.insertString(off, str, attr);
            }

            @Override
            public void replace(DocumentFilter.FilterBypass fb, int off, int len, String str, AttributeSet attr)
                    throws BadLocationException {
                str = str.replaceAll(filteredCharsRegex, "");
                fb.replace(off, len, str, attr);
            }
        });
        return doc;
    }
        
    private String getDefaultTextMsfragger() {
        String path = ThisAppProps.load(ThisAppProps.PROP_BIN_PATH_MSFRAGGER);
        return path == null ? "MSFragger.jar" : path;
    }

    

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton btnLoad;
    private javax.swing.JButton btnMsfraggerDefaultsClosed;
    private javax.swing.JButton btnMsfraggerDefaultsOpen;
    private javax.swing.JButton btnSave;
    private javax.swing.JCheckBox checkClipNTerm;
    private javax.swing.JCheckBox checkMultipleVarMods;
    private javax.swing.JCheckBox checkOverrideCharge;
    private javax.swing.JCheckBox chkMsadjuster;
    private javax.swing.JCheckBox chkRunMsfragger;
    private javax.swing.JComboBox<String> comboCleavage;
    private javax.swing.JComboBox<String> comboFragMassTol;
    private javax.swing.JComboBox<String> comboFraggerOutputType;
    private javax.swing.JComboBox<String> comboPrecursorMassTol;
    private javax.swing.JComboBox<String> comboPrecursorTrueTol;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel10;
    private javax.swing.JLabel jLabel11;
    private javax.swing.JLabel jLabel12;
    private javax.swing.JLabel jLabel13;
    private javax.swing.JLabel jLabel14;
    private javax.swing.JLabel jLabel15;
    private javax.swing.JLabel jLabel16;
    private javax.swing.JLabel jLabel17;
    private javax.swing.JLabel jLabel18;
    private javax.swing.JLabel jLabel19;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel20;
    private javax.swing.JLabel jLabel21;
    private javax.swing.JLabel jLabel22;
    private javax.swing.JLabel jLabel23;
    private javax.swing.JLabel jLabel24;
    private javax.swing.JLabel jLabel25;
    private javax.swing.JLabel jLabel26;
    private javax.swing.JLabel jLabel27;
    private javax.swing.JLabel jLabel28;
    private javax.swing.JLabel jLabel29;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel31;
    private javax.swing.JLabel jLabel32;
    private javax.swing.JLabel jLabel35;
    private javax.swing.JLabel jLabel36;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JLabel jLabel6;
    private javax.swing.JLabel jLabel7;
    private javax.swing.JLabel jLabel8;
    private javax.swing.JLabel jLabel9;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanel4;
    private javax.swing.JPanel jPanel6;
    private javax.swing.JPanel jPanel7;
    private javax.swing.JPanel jPanel8;
    private javax.swing.JPanel jPanel9;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JScrollPane jScrollPane2;
    private javax.swing.JLabel lblAddTopNComplementary;
    private javax.swing.JLabel lblRam;
    private javax.swing.JLabel lblSlices;
    private javax.swing.JLabel lblThreads;
    private javax.swing.JLabel lblTrackZeroTopN;
    private javax.swing.JLabel lblZeroBinAcceptExpect;
    private javax.swing.JLabel lblZeroBinMultiplyExpect;
    private javax.swing.JPanel panelFraggerMatchingConfig;
    private javax.swing.JPanel panelMsFragger;
    private javax.swing.JPanel panelMsfraggerParams;
    private javax.swing.JSpinner spinnerAddTopNComplementary;
    private javax.swing.JSpinner spinnerClearMzRangeMax;
    private javax.swing.JSpinner spinnerClearMzRangeMin;
    private javax.swing.JSpinner spinnerDigestLenMax;
    private javax.swing.JSpinner spinnerDigestLenMin;
    private javax.swing.JSpinner spinnerDigestMassMax;
    private javax.swing.JSpinner spinnerDigestMassMin;
    private javax.swing.JSpinner spinnerFragMassTol;
    private javax.swing.JSpinner spinnerFraggerRam;
    private javax.swing.JSpinner spinnerFraggerThreads;
    private javax.swing.JSpinner spinnerMaxCombos;
    private javax.swing.JSpinner spinnerMaxFragCharge;
    private javax.swing.JSpinner spinnerMaxVarModsPerMod;
    private javax.swing.JSpinner spinnerMinFragsModelling;
    private javax.swing.JSpinner spinnerMinMatchedFrags;
    private javax.swing.JSpinner spinnerMinPeaks;
    private javax.swing.JSpinner spinnerMinRatio;
    private javax.swing.JSpinner spinnerMissedCleavages;
    private javax.swing.JSpinner spinnerOutputMaxExpect;
    private javax.swing.JSpinner spinnerPrecursorChargeHi;
    private javax.swing.JSpinner spinnerPrecursorChargeLo;
    private javax.swing.JSpinner spinnerPrecursorMassTolHi;
    private javax.swing.JSpinner spinnerPrecursorMassTolLo;
    private javax.swing.JSpinner spinnerPrecursorTrueTol;
    private javax.swing.JSpinner spinnerReportTopN;
    private javax.swing.JSpinner spinnerSlices;
    private javax.swing.JSpinner spinnerTrackZeroTopN;
    private javax.swing.JSpinner spinnerUseTopNPeaks;
    private javax.swing.JSpinner spinnerZeroBinAcceptExpect;
    private javax.swing.JSpinner spinnerZeroBinMultiplyExpect;
    private javax.swing.JTable tableAdditionalMods;
    private javax.swing.JTable tableVarMods;
    private javax.swing.JTextField textButNotAfter;
    private javax.swing.JTextField textCutAfter;
    private javax.swing.JTextField textEnzymeName;
    private javax.swing.JTextField textIsotopeError;
    private javax.swing.JTextField textMassOffsets;
    // End of variables declaration//GEN-END:variables

    private ComboBoxModel<String> createOutputFormatComboModel() {
        String[] items = new String[FraggerOutputType.values().length];
        for (int i = 0; i < items.length; i++) {
            items[i] = FraggerOutputType.values()[i].toString();
        }
        return new DefaultComboBoxModel<>(items);
    }
    
    private ComboBoxModel<String> createMassToleranceUnitsComboModel() {
        String[] items = new String[MassTolUnits.values().length];
        for (int i = 0; i < items.length; i++) {
            items[i] = MassTolUnits.values()[i].toString();
        }
        return new DefaultComboBoxModel<>(items);
    }

    private ComboBoxModel<String> createMsLevelComboBoxModel() {
        String[] items = new String[MsLevel.values().length];
        for (int i = 0; i < items.length; i++) {
            items[i] = MsLevel.values()[i].toString();
        }
        
        return new DefaultComboBoxModel<>(items);
    }

    private ComboBoxModel<String> createCleavageComboBoxModel() {
        String[] items = new String[CleavageType.values().length];
        for (int i = 0; i < items.length; i++) {
            items[i] = CleavageType.values()[i].toString();
        }
        
        return new DefaultComboBoxModel<>(items);
    }

    public boolean isRunMsfragger() {
        return chkRunMsfragger.isSelected();
    }

    
}
