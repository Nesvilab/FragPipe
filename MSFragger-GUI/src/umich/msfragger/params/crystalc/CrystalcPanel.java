package umich.msfragger.params.crystalc;

import java.util.ArrayList;
import java.util.List;
import javax.swing.JCheckBox;
import javax.swing.JPanel;
import javax.swing.JSpinner;
import javax.swing.SpinnerNumberModel;
import javax.swing.border.TitledBorder;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import umich.msfragger.util.swing.FormEntry;

public class CrystalcPanel extends JPanel {
  public JCheckBox checkRun;
  private JPanel pPar;
  private List<String> paramNames;

  public CrystalcPanel() {
    paramNames = new ArrayList<>();
    initMore();
  }

  private void initMore() {
    this.setLayout(new MigLayout(new LC().flowY().fillX()));
    LC lc = new LC();//.debug();

    // Panel - top
    JPanel pTop = new JPanel(new MigLayout(lc));
    //pTop.setBorder(new TitledBorder("General options"));
    checkRun = new JCheckBox("Run DIA-Umpire SE (Signal Extraction)");
    pTop.add(checkRun);

    // Panel - Crystal-C parameters
    pPar = new JPanel(new MigLayout(lc));
    pPar.setBorder(new TitledBorder("Crystal-C parameters"));
    List<FormEntry> fes = new ArrayList<>();

    fes.add(new FormEntry(CrystalcParams.PROP_MaxZ, "Max charge",
        new JSpinner(new SpinnerNumberModel(6, 1, 20, 1))));
    fes.add(new FormEntry(CrystalcParams.PROP_IsoNum, "Number of isotopes",
        new JSpinner(new SpinnerNumberModel(3, 1, 10, 1))));
    fes.add(new FormEntry(CrystalcParams.PROP_MassTol, "Mass tolerance (ppm)",
        new JSpinner(new SpinnerNumberModel(20, 1, 200, 5))));
    fes.add(new FormEntry(CrystalcParams.PROP_PrecursorIsolationWindow, "Precursor isolation window",
        new JSpinner(new SpinnerNumberModel(0.7, 0.001, 10, 0.2))));
  }

}
