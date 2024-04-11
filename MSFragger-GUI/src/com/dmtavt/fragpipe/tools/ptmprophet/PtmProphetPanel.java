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

package com.dmtavt.fragpipe.tools.ptmprophet;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.tabs.TabMsfragger;
import com.dmtavt.fragpipe.tools.fragger.Mod;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.swing.FormEntry;
import com.github.chhh.utils.swing.JPanelBase;
import com.github.chhh.utils.swing.MigUtils;
import com.github.chhh.utils.swing.UiCheck;
import com.github.chhh.utils.swing.UiText;
import com.github.chhh.utils.swing.UiUtils;
import java.awt.Component;
import java.awt.ItemSelectable;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class PtmProphetPanel extends JPanelBase {
  private static final Logger log = LoggerFactory.getLogger(PtmProphetPanel.class);
  private static final String PREFIX = "ptmprophet.";
  private JPanel pTop;
  private JPanel pContent;
  private static final MigUtils mu = MigUtils.get();
  private UiCheck checkRun;
  private UiText uiTextCmd;

  public PtmProphetPanel() {
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

  public void setRunStatus(boolean status) {
    checkRun.setEnabled(status);
  }

  @Override
  protected void init() {
    mu.layout(this, mu.lcFillXNoInsetsTopBottom());
    mu.border(this, "PTM Site Localization");

    pTop = createPanelTop();
    pContent = createPanelContent();

    mu.add(this, pTop).growX().wrap();
    mu.add(this, pContent).growX().wrap();
  }

  private JPanel createPanelContent() {
    JPanel p = mu.newPanel(null, mu.lcNoInsetsTopBottom());
    //mu.border(p, "PTMProphet options");

    //mu.add(p, new JLabel("Some dummy property")).wrap();

    uiTextCmd = UiUtils.uiTextBuilder().text(getDefaults()).create();
    FormEntry feCmdLineOpts = mu.feb("cmdline", uiTextCmd).label("Cmd line opts")
        .tooltip("Command line options for PTMProphet").create();

    mu.add(p, feCmdLineOpts.label(), mu.ccR());
    mu.add(p, feCmdLineOpts.comp).spanX().growX().pushX().wrap();

    return p;
  }

  private String getDefaults() {
    final String prop = "ptmprophet.cmdline.default";
    String val = Fragpipe.getPropFix(prop);
    if (val == null) {
      val = "NOSTACK KEEPOLD STATIC EM=1 NIONS=b STY:79.966331,M:15.9949 MINPROB=0.5";
      log.warn("Property [{}] not found in Bundle.properties, default to hardcoded value: {}", prop, val);
    }
    return val;
  }

  /**
   * Load modification info into the command line args string for PTMProphet automatically.
   * @return
   */
  private String loadMSFraggerMods() {
    String cmdText = uiTextCmd.getNonGhostText();
    if (cmdText.isEmpty()) {
      // command is currently empty. Load default command as a starting point
      cmdText = getDefaults();
    }
    // Find the mod argument (if present)
    String[] splits = cmdText.split(" ");
    Pattern modPattern = Pattern.compile("[^:]+:[\\d+.-]+");
    int modIndex = -1;
    for (int i=0; i < splits.length; i++) {
      Matcher m = modPattern.matcher(splits[i]);
      if (m.find()) {
        modIndex = i;
        break;
      }
    }
    // replace the mod argument with the current MSFragger mods
    String newMods = getMSFraggerMods();
    if (modIndex == -1) {
      // no previous mod argument, append to the array
      cmdText = cmdText + " " + newMods;
    } else {
      splits[modIndex] = newMods;
      cmdText = String.join(" ", splits);
    }
    return cmdText;
  }

  /**
   * Helper method to get the MSFragger mods as a PTMProphet-formatted string. Considers all
   * variable and offset mods on the MSFragger tab.
   * @return
   */
  private String getMSFraggerMods(){
    ArrayList<String> modStrings = new ArrayList<>();
    TabMsfragger tabMsfragger = Fragpipe.getStickyStrict(TabMsfragger.class);
    List<Mod> mods = tabMsfragger.getVarModsTable();

    // merge mass offsets with var mods if the mass is equal to avoid duplicate entries (which crash PTMProphet)
    for (Mod offsetMod: tabMsfragger.getOffsetsAsMods()) {
      if (offsetMod.isEnabled && offsetMod.massDelta != 0) {
        boolean foundMatchingVarmod = false;
        for (Mod varMod : mods) {
          if (varMod.isEnabled && Math.abs(varMod.massDelta - offsetMod.massDelta) < 0.0001) {
            foundMatchingVarmod = true;
            // merge sites and replace the varmod with a new merged offset+var mod
            StringBuilder finalSites = new StringBuilder(varMod.sites);
            for (int i=0; i < offsetMod.sites.length(); i++) {
              if (!varMod.sites.contains(String.valueOf(offsetMod.sites.charAt(i)))) {
                finalSites.append(offsetMod.sites.charAt(i));
              }
            }
            Mod newMod = new Mod(varMod.massDelta, finalSites.toString(), true, varMod.maxOccurrences);
            mods.remove(varMod);
            mods.add(newMod);
            break;
          }
        }
        if (!foundMatchingVarmod) {
          // add offset as normal
          mods.add(offsetMod);
        }
      }
    }

    for (Mod mod: mods) {
      if (mod.isEnabled && mod.massDelta != 0) {
        String sites = mod.sites;
        if (sites.contentEquals("")) {
          sites = "ACDEFGHIKLMNPQRSTVWY";
        }
        if (sites.contains("*")) {
          sites = sites.replace("*", "ACDEFGHIKLMNPQRSTVWY");
        }
        // handle terminal characters
        if (sites.contains("n^") || sites.contains("[^")) {
          sites = sites.replace("n^", "n");
          sites = sites.replace("[^", "n");
        } else {
          sites = sites.replace("n", "");
          sites = sites.replace("[", "");
        }
        if (sites.contains("c^") || sites.contains("]^")) {
          sites = sites.replace("c^", "c");
          sites = sites.replace("]^", "c");
        } else {
          sites = sites.replace("c", "");
          sites = sites.replace("]", "");
        }
        sites = sites.replace("^", "");
        modStrings.add(sites + ":" + mod.massDelta);
      }
    }
    return String.join(",", modStrings);
  }

  private JPanel createPanelTop() {

    JPanel p = mu.newPanel(null, mu.lcFillXNoInsetsTopBottom());

    checkRun = new UiCheck("Run PTMProphet", null, false);
    checkRun.setName("run-ptmprophet");
    JButton btnDefaults = UiUtils.createButton("Load defaults", e -> {
      uiTextCmd.setText(getDefaults());
    });

    JButton btnLoadMSFraggerMods = UiUtils.createButton("Load Mods from MSFragger Settings", e -> {
      uiTextCmd.setText(loadMSFraggerMods());
    });
    btnLoadMSFraggerMods.setToolTipText("Load modifications from the current MSFragger tab setting into the Cmd line options for PTMProphet. Will load all variable mods and mass offsets. The resulting options can be edited after loading as needed.");

    JLabel info = new JLabel(
        "<html>Not for open searches. Mods format example: STY:79.966331,M:15.9949");

    checkRun.addActionListener(e -> {
      if (isRun()) {
        TabMsfragger tabMsfragger = Fragpipe.getStickyStrict(TabMsfragger.class);
        tabMsfragger.setWriteCalMzml(true); // If mass calibration is enabled, let MSFragger write calibrated mzML. If mass calibration is not enable, the calibrated mzML is till not generated.
      }
    });

    mu.add(p, checkRun).split();
    mu.add(p, btnDefaults);
    mu.add(p, btnLoadMSFraggerMods).gapLeft("20px");
    mu.add(p, info).gapLeft("80px").wrap();
    return p;
  }

  public boolean isRun() {
    return SwingUtils.isEnabledAndChecked(checkRun);
  }

  public String getCmdLineOpts() {
    return uiTextCmd.getNonGhostText();
  }

  @Override
  public void initMore() {
    updateEnabledStatus(this, true);
    super.initMore();
  }
}
