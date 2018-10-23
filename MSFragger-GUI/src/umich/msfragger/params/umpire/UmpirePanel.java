package umich.msfragger.params.umpire;

import static umich.msfragger.params.umpire.UmpireParams.PROP_AdjustFragIntensity;
import static umich.msfragger.params.umpire.UmpireParams.PROP_BoostComplementaryIon;
import static umich.msfragger.params.umpire.UmpireParams.PROP_CorrThreshold;
import static umich.msfragger.params.umpire.UmpireParams.PROP_DeltaApex;
import static umich.msfragger.params.umpire.UmpireParams.PROP_EstimateBG;
import static umich.msfragger.params.umpire.UmpireParams.PROP_MS1PPM;
import static umich.msfragger.params.umpire.UmpireParams.PROP_MS2PPM;
import static umich.msfragger.params.umpire.UmpireParams.PROP_MS2SN;
import static umich.msfragger.params.umpire.UmpireParams.PROP_MaxCurveRTRange;
import static umich.msfragger.params.umpire.UmpireParams.PROP_MaxNoPeakCluster;
import static umich.msfragger.params.umpire.UmpireParams.PROP_MinFrag;
import static umich.msfragger.params.umpire.UmpireParams.PROP_MinMSIntensity;
import static umich.msfragger.params.umpire.UmpireParams.PROP_MinMSMSIntensity;
import static umich.msfragger.params.umpire.UmpireParams.PROP_MinNoPeakCluster;
import static umich.msfragger.params.umpire.UmpireParams.PROP_NoMissedScan;
import static umich.msfragger.params.umpire.UmpireParams.PROP_RFmax;
import static umich.msfragger.params.umpire.UmpireParams.PROP_RPmax;
import static umich.msfragger.params.umpire.UmpireParams.PROP_RTOverlap;
import static umich.msfragger.params.umpire.UmpireParams.PROP_SN;
import static umich.msfragger.params.umpire.UmpireParams.PROP_Thread;
import static umich.msfragger.params.umpire.UmpireParams.PROP_WindowSize;
import static umich.msfragger.params.umpire.UmpireParams.PROP_WindowType;

import java.awt.Component;
import java.awt.Container;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import javax.swing.DefaultComboBoxModel;
import javax.swing.ImageIcon;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JFormattedTextField;
import javax.swing.JPanel;
import javax.swing.JSpinner;
import javax.swing.JTextField;
import javax.swing.SpinnerNumberModel;
import javax.swing.border.TitledBorder;
import javax.swing.text.DefaultFormatterFactory;
import javax.swing.text.NumberFormatter;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.greenrobot.eventbus.EventBus;
import rx.swing.sources.DocumentEventSource;
import umich.msfragger.events.MessageIsUmpireRun;
import umich.msfragger.gui.ToolingUtils;
import umich.msfragger.params.ThisAppProps;
import umich.msfragger.util.StringUtils;
import umich.msfragger.util.SwingUtils;
import umich.msfragger.util.swing.FormEntry;

public class UmpirePanel extends JPanel {
  public JCheckBox checkRunUmpireSe;
  public JSpinner spinnerRam;
  private JTextField textBinMsconvert;
  private final String ghostTextBinMsconvert = "MsConvert is optional on Linux and mandatory on Windows";
  private JTextField textConfigFile;
  private final String ghostTextConfigFile = "Optional path to a config file with defaults";
  private JPanel pFrag;
  private JPanel pSe;
  private JPanel pSwath;
  private JPanel pOther;
  private ImageIcon icon;

  private final List<String> paramNames = Arrays.asList(
      PROP_Thread,
      PROP_RPmax,
      PROP_RFmax,
      PROP_CorrThreshold,
      PROP_DeltaApex,
      PROP_RTOverlap,
      PROP_AdjustFragIntensity,
      PROP_BoostComplementaryIon,
      PROP_MS1PPM,
      PROP_MS2PPM,
      PROP_SN,
      PROP_MS2SN,
      PROP_MinMSIntensity,
      PROP_MinMSMSIntensity,
      PROP_MaxCurveRTRange,
      PROP_NoMissedScan,
      PROP_MinFrag,
      PROP_EstimateBG,
      PROP_MinNoPeakCluster,
      PROP_MaxNoPeakCluster,
      PROP_WindowType,
      PROP_WindowSize);

  public UmpirePanel() {
    //EventBus.getDefault().register(this);
    initMore();
  }

  private void initMore() {
    icon = new ImageIcon(
        getClass().getResource("/umich/msfragger/gui/icons/dia-umpire-16x16.png"));

    this.setLayout(new MigLayout(new LC().flowY().fillX()));

    LC lc = new LC();//.debug();

    // Panel - top
    JPanel pTop = new JPanel(new MigLayout(lc));
    //pTop.setBorder(new TitledBorder("General options"));

    checkRunUmpireSe = new JCheckBox("Run DIA-Umpire SE (Signal Extraction)");
    pTop.add(checkRunUmpireSe);


    // Panel - fragment grouping options
    pFrag = new JPanel(new MigLayout(lc));
    pFrag.setBorder(new TitledBorder("Fragment grouping options"));

    DefaultFormatterFactory decimalAsInt = new DefaultFormatterFactory(
        new NumberFormatter(new DecimalFormat("#0")));
    DefaultFormatterFactory decimal = new javax.swing.text.DefaultFormatterFactory(
        new javax.swing.text.NumberFormatter());

    FormEntry feRpMax = new FormEntry(PROP_RPmax, "RP max", new JFormattedTextField(decimalAsInt));
    FormEntry feRfMax = new FormEntry(PROP_RFmax, "RF max", new JFormattedTextField(decimal));
    FormEntry feCorrThresh = new FormEntry(PROP_CorrThreshold, "Corr Threshold", new JFormattedTextField(decimal));
    FormEntry feDeltaApex = new FormEntry(PROP_DeltaApex, "Delta Apex", new JFormattedTextField(decimal));
    FormEntry feRtOverlap = new FormEntry(PROP_RTOverlap, "RT Overlap", new JFormattedTextField(decimal));
    //FormEntry fe = new FormEntry(UmpireParams.PROP_, "", );
    JCheckBox checkBoostComplimentaryIons = new JCheckBox("Boost complimentary ions");
    JCheckBox checkAdjustFragIntensity = new JCheckBox("Adjust fragment intensity");

    CC ccComp = new CC().width("30:50:70px");
    CC ccFmtWrap = new CC().width("30:50:70px").wrap();
    CC ccLbl = new CC().alignX("right").gapBefore("5px");
    pFrag.add(feRpMax.label(), ccLbl);
    pFrag.add(feRpMax.comp, ccComp);
    pFrag.add(feRfMax.label(), ccLbl);
    pFrag.add(feRfMax.comp, ccFmtWrap);
    pFrag.add(feCorrThresh.label(), ccLbl);
    pFrag.add(feCorrThresh.comp, ccComp);
    pFrag.add(feDeltaApex.label(), ccLbl);
    pFrag.add(feDeltaApex.comp, ccComp);
    pFrag.add(feRtOverlap.label(), ccLbl);
    pFrag.add(feRtOverlap.comp, ccFmtWrap);
    pFrag.add(checkBoostComplimentaryIons, "span, split 2");
    pFrag.add(checkAdjustFragIntensity);


    // Panel - fragment grouping options
    pSe = new JPanel(new MigLayout(lc));
    pSe.setBorder(new TitledBorder("Signal Extraction Parameters"));
    List<FormEntry> feSe = new ArrayList<>();
    //entries.add(new FormEntry(UmpireParams.PROP_, "", new JFormattedTextField()));
    feSe.add(new FormEntry(UmpireParams.PROP_MS1PPM, "MS1 PPM", new JFormattedTextField(decimalAsInt)));
    feSe.add(new FormEntry(UmpireParams.PROP_MS2PPM, "MS2 PPM", new JFormattedTextField(decimalAsInt)));
    feSe.add(new FormEntry(UmpireParams.PROP_MaxCurveRTRange, "Max Curve RT Range", new JFormattedTextField(decimal)));

    feSe.add(new FormEntry(UmpireParams.PROP_MinMSIntensity, "Min MS1 Intensity", new JFormattedTextField(decimal)));
    feSe.add(new FormEntry(UmpireParams.PROP_MinMSMSIntensity, "Min MS2 Intensity", new JFormattedTextField(decimal)));
    feSe.add(new FormEntry(UmpireParams.PROP_MinFrag, "Min Fragments", new JFormattedTextField(decimalAsInt)));

    feSe.add(new FormEntry(UmpireParams.PROP_SN, "MS1 S/N", new JFormattedTextField(decimalAsInt)));
    feSe.add(new FormEntry(UmpireParams.PROP_MS2SN, "MS2 S/N", new JFormattedTextField(decimalAsInt)));
    feSe.add(new FormEntry(UmpireParams.PROP_EstimateBG, "Estimate Background", new JCheckBox()));

    feSe.add(new FormEntry(UmpireParams.PROP_MinNoPeakCluster, "Min N Peaks/Cluster", new JFormattedTextField(decimalAsInt)));
    feSe.add(new FormEntry(PROP_MaxNoPeakCluster, "Max N Peaks/Cluster", new JFormattedTextField(decimalAsInt)));
    feSe.add(new FormEntry(UmpireParams.PROP_NoMissedScan, "Max Missed Scans", new JFormattedTextField(decimalAsInt)));
    //entries.add(new FormEntry(UmpireParams.PROP_, "", new JFormattedTextField()));

    for (int i = 0; i < feSe.size(); i++) {
      CC ccLabel = new CC().alignX("right").gapBefore("5px");
      CC cc = (i+1) % 3 != 0
          ? new CC().width("30:50:70px")
          : new CC().width("30:50:70px").wrap();
      FormEntry fe = feSe.get(i);
      pSe.add(fe.label(), ccLabel);
      pSe.add(fe.comp, cc);
    }


    // Panel - SWATH window parameters
    pSwath = new JPanel(new MigLayout(lc));
    pSwath.setBorder(new TitledBorder("SWATH window parameters"));

    JComboBox<String> comboWindowType = new JComboBox<>();
    comboWindowType.setModel(new DefaultComboBoxModel<>(new String[]{"SWATH"}));
    FormEntry feWinType = new FormEntry(PROP_WindowType, "Window type", comboWindowType);
    FormEntry feWinSize = new FormEntry(PROP_WindowSize, "Window size", new JFormattedTextField(decimal));

    pSwath.add(feWinType.label(), ccLbl);
    pSwath.add(feWinType.comp, new CC().width("70:80:120px"));
    pSwath.add(feWinSize.label(), ccLbl);
    pSwath.add(feWinSize.comp, ccComp);


    // Panel - Other options
    pOther = new JPanel(new MigLayout(lc));
    pOther.setBorder(new TitledBorder("Other options"));

    JPanel panelSpinners = new JPanel(new MigLayout(lc));

    // RAM spinner
    spinnerRam = new JSpinner(new SpinnerNumberModel(0, 0, 64, 1));
    String ram = ThisAppProps.load(UmpireParams.ETC_PARAM_RAM);
    if (ram != null)
      spinnerRam.setValue(Integer.valueOf(ram));
    spinnerRam.addChangeListener(e -> ThisAppProps.save(UmpireParams.ETC_PARAM_RAM, ((Integer)spinnerRam.getValue()).toString()));
    FormEntry feRam = new FormEntry(UmpireParams.ETC_PARAM_RAM, "Max RAM (GB)", spinnerRam);
    panelSpinners.add(feRam.label(), new CC().alignX("right"));
    panelSpinners.add(feRam.comp, new CC().width("30:50:70px"));

    // Threads spinner
    int availableThreads = Runtime.getRuntime().availableProcessors();
    JSpinner spinnerThreads = new JSpinner(new SpinnerNumberModel(0, 0, availableThreads * 2, 1));
    FormEntry feThreads = new FormEntry(PROP_Thread, "Threads", spinnerThreads);
    panelSpinners.add(feThreads.label(),  new CC().alignX("right").gapBefore("5px"));
    panelSpinners.add(feThreads.comp, new CC().width("30:50:70px").wrap());

    pOther.add(panelSpinners, "span");

    // default config file
    String pathConfigFile = ThisAppProps.load(UmpireParams.DEFAULT_FILE);
    textConfigFile = new JTextField(pathConfigFile);
    DocumentEventSource.fromDocumentEventsOf(textConfigFile.getDocument())
        .debounce(3, TimeUnit.SECONDS)
        .subscribe(documentEvent -> {
          try {
            final String val = textConfigFile.getText();
            final String toSave = ghostTextConfigFile.equals(val) ? null : val;
            ThisAppProps.save(UmpireParams.DEFAULT_FILE, toSave);

            if (!StringUtils.isNullOrWhitespace(toSave)) {
              Path path = Paths.get(toSave);
              if (Files.exists(path)) {
                UmpireParams params = new UmpireParams();
                try (InputStream is = Files.newInputStream(path, StandardOpenOption.READ)) {
                  params.load(is);
                  UmpirePanel.this.fillFrom(params);
                }
              }
            }

          } catch (Exception ignore) {}
        });
    FormEntry feConfigFile = new FormEntry(UmpireParams.DEFAULT_FILE, "Default config file", textConfigFile);
    pOther.add(feConfigFile.label(), ccLbl);
    pOther.add(feConfigFile.comp, new CC().growX().pushX());
    pOther.add(feConfigFile.browseButton("Browse", "Select file", "Config file",
        this, ghostTextConfigFile),
        new CC().minWidth("button").wrap());


    // msconvert binary
    String binMsconvert = ThisAppProps.load(ThisAppProps.PROP_BIN_PATH_MSCONVERT);
    if (binMsconvert == null)
      binMsconvert = ToolingUtils.getBinMsconvert();
    textBinMsconvert = new JTextField(binMsconvert);
    // save the text once there are no change events for 3 seconds
    DocumentEventSource.fromDocumentEventsOf(textBinMsconvert.getDocument())
        .debounce(3, TimeUnit.SECONDS)
        .subscribe(documentEvent -> {
          try {
            final String val = textBinMsconvert.getText();
            final String toSave = ghostTextBinMsconvert.equals(val) ? null : val;
            ThisAppProps.save(ThisAppProps.PROP_BIN_PATH_MSCONVERT, toSave);
          } catch (Exception ignore) {}
        });

    FormEntry feBinMsconvert = new FormEntry(ThisAppProps.PROP_BIN_PATH_MSCONVERT, "MsConvert binary", textBinMsconvert);
    pOther.add(feBinMsconvert.label(), ccLbl);
    pOther.add(feBinMsconvert.comp, new CC().growX().pushX());
    pOther.add(feBinMsconvert.browseButton("Browse", "Select file", "MSConvert binary",
        this, "MsConvert is optional on Linux and mandatory on Windows"),
        new CC().minWidth("button").wrap());


    CC ccGrowX = new CC().growX();
    this.add(pTop, ccGrowX);
    this.add(pFrag, ccGrowX);
    this.add(pSe, ccGrowX);
    this.add(pSwath, ccGrowX);
    this.add(pOther, ccGrowX);

    enablePanels(checkRunUmpireSe.isSelected());
    checkRunUmpireSe.addChangeListener(e -> {
      final boolean isRun = checkRunUmpireSe.isSelected();
      enablePanels(isRun);
      EventBus.getDefault().post(new MessageIsUmpireRun(isRun));
    });

    reloadUmpireParams();
  }

  private void enablePanels(boolean enabled) {
    List<Container> comps = Arrays.asList(pFrag, pSe, pSwath, pOther);
    for (Container c : comps) {
      SwingUtils.enableComponents(c, enabled);
    }
  }

  public String getDefaultConfigFile() {
    String text = textConfigFile.getText();
    if (StringUtils.isNullOrWhitespace(text) || ghostTextConfigFile.equals(text))
      return null;
    return text;
  }

  public String getBinMsconvert() {
    String text = textBinMsconvert.getText();
    if (StringUtils.isNullOrWhitespace(text) || ghostTextBinMsconvert.equals(text))
      return null;
    return text;
  }

  public void reloadUmpireParams() {
    new Thread(() -> {
      UmpireParams params = new UmpireParams();
      try {
        // load original defaults
        params.loadDefault();
        // load user specified defaults
        final String newDefaultsPath = getDefaultConfigFile();
        if (!StringUtils.isNullOrWhitespace(newDefaultsPath)) {
          Path path = Paths.get(newDefaultsPath);
          try (InputStream is = Files.newInputStream(path)) {
            params.load(is);
          }
        }
        // load cached
        params.loadCache();

        fillFrom(params);

      } catch (IOException ignore) {}
    }).run();
  }

  /** Use {@link SwingUtils#getStrVal} to get string values from most common Java Swing GUI elements. */
  public UmpireParams collect() {

    // The map contains all named params with their corresponding UI elements.
    Map<String, Component> map = SwingUtils.mapComponentsByName(this, true);

    UmpireParams params = new UmpireParams();
    for (String paramName : paramNames) {
      Component component = map.get(paramName);
      if (component != null) {
        String strVal = SwingUtils.getStrVal(component);
        if (PROP_Thread.equals(paramName) && "0".equals(strVal))
          continue;
        params.getProps().setProperty(paramName, strVal);
      }
    }
    return params;
  }

  public void fillFrom(UmpireParams params) {
    Map<String, Component> map = SwingUtils.mapComponentsByName(this, true);
    for (String name : params.getProps().stringPropertyNames()) {
      Component component = map.get(name);
      if (component != null) {
        String val = params.getProps().getProperty(name);
        SwingUtils.setStrVal(component, val);
      }
    }

  }

  public ImageIcon getIcon() {
    return icon;
  }
}
