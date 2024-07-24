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

package com.dmtavt.fragpipe.tabs;

import static com.dmtavt.fragpipe.Fragpipe.philosopherBinPath;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.api.Bus;
import com.dmtavt.fragpipe.api.DownloadDbHelper;
import com.dmtavt.fragpipe.api.Notifications;
import com.dmtavt.fragpipe.exceptions.ValidationException;
import com.dmtavt.fragpipe.messages.MessageDbNewPath;
import com.dmtavt.fragpipe.messages.MessageDecoyTag;
import com.dmtavt.fragpipe.messages.MessageUiRevalidate;
import com.dmtavt.fragpipe.messages.NoteConfigDatabase;
import com.dmtavt.fragpipe.params.ThisAppProps;
import com.github.chhh.utils.FastaUtils;
import com.github.chhh.utils.FastaUtils.FastaContent;
import com.github.chhh.utils.FastaUtils.FastaDecoyPrefixSearchResult;
import com.github.chhh.utils.PathUtils;
import com.github.chhh.utils.StringUtils;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.swing.ContentChangedFocusAdapter;
import com.github.chhh.utils.swing.FileChooserUtils;
import com.github.chhh.utils.swing.FileChooserUtils.FcMode;
import com.github.chhh.utils.swing.FormEntry;
import com.github.chhh.utils.swing.JPanelWithEnablement;
import com.github.chhh.utils.swing.MigUtils;
import com.github.chhh.utils.swing.UiText;
import com.github.chhh.utils.swing.UiUtils;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.io.IOException;
import java.nio.file.AccessDeniedException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JEditorPane;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.filechooser.FileNameExtensionFilter;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.ThreadMode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class TabDatabase extends JPanelWithEnablement {
  private static final Logger log = LoggerFactory.getLogger(TabDatabase.class);
  private static final MigUtils mu = MigUtils.get();
  public static final String TAB_PREFIX = "database.";
  public static final String TIP_DB_PATH = "tip.db.path";
  public static final long databaseSizeLimit = 1 << 30L;
  private static final String TIP_DB_DOWNLOAD = "tip.db.download";
  private static final String TIP_DB_UPDATE = "tip.db.update";
  public static final Pattern disallowedFastaPattern = Pattern.compile("[^A-Za-z0-9_:\\\\/.+-]");

  private UiText uiTextDbPath;
  private UiText uiTextDecoyTag;
  private JEditorPane epDbInfo;
  private JButton btnDownload;
  private JButton btnUpdate;

  public TabDatabase() {
    init();
    initMore();
  }

  private void initMore() {
    updateEnabledStatus(btnDownload, true);
    Bus.register(this);
    Bus.postSticky(this);
  }

  private void init() {
    this.setLayout(new MigLayout(new LC().fillX()));

    mu.add(this, createPanelDbSelection()).growX().wrap();
    mu.add(this, createPanelInfo()).growX().wrap();

  }

  private FormEntry.Builder fe(JComponent comp, String name) {
    return Fragpipe.fe(comp, name, TAB_PREFIX);
  }

  private JPanel createPanelDbSelection() {

    uiTextDbPath = UiUtils.uiTextBuilder().cols(5).create();
    uiTextDbPath.addFocusListener(new ContentChangedFocusAdapter(uiTextDbPath, (s, s2) -> {
      Bus.post(new MessageDbNewPath(s2));
    }));
    FormEntry feDbPath = fe(uiTextDbPath, "db-path").label("FASTA file path").create();
    JButton btnBrowse = feDbPath.browseButton("Browse", "Select FASTA file",
        () -> createFilechooserFasta(uiTextDbPath),
        paths -> Bus.post(new MessageDbNewPath(paths.get(0).toString())));
    btnDownload = UiUtils.createButton("Download", this::actionDbDownload);
    btnUpdate = UiUtils.createButton("Add decoys", this::actionDbAddDecoys);

    String defaultTag = Fragpipe.propsFix().getProperty(ThisAppProps.PROP_TEXTFIELD_DECOY_TAG);
    uiTextDecoyTag = UiUtils.uiTextBuilder().cols(12).text(defaultTag).create();
    FormEntry feDecoyTag = fe(uiTextDecoyTag, "decoy-tag").label("Decoy protein prefix")
        .tooltip( "Decoys are used for FDR estimation.\n"
            + "Decoy proteins in the database are identified by this prefix in\n"
            + "their headers. If you're unsure what tag is used in your protein\n"
            + "database, use the Try auto-detect tag button to see what proportion\n"
            + "of sequences in the given database contain a decoy tag.").create();
    JButton btnDecoyDetect = feDecoyTag.button("Try auto-detect tag", "Input decoy tag",
        this::actionDetectDecoys);

    epDbInfo = SwingUtils.createClickableHtml(true, "");

    JPanel p = mu.newPanel("FASTA sequence database", true);
    mu.add(p, feDbPath.label()).split();
    mu.add(p, feDbPath.comp).growX();
    mu.add(p, btnBrowse);
    mu.add(p, btnDownload);
    mu.add(p, btnUpdate).wrap();

    mu.add(p, feDecoyTag.label()).split();
    mu.add(p, feDecoyTag.comp);
    uiTextDecoyTag.addFocusListener(new ContentChangedFocusAdapter(uiTextDecoyTag, (s, s2) -> {
      validateFasta(getFastaPath());
    }));
    mu.add(p, btnDecoyDetect);
    mu.add(p, epDbInfo);
    return p;
  }

  private void actionDbAddDecoys(ActionEvent event) {
    String fasta = getFastaPath();
    if (StringUtils.isBlank(fasta)) {
      SwingUtils.showInfoDialog(this, "Select a FASTA file first.", "Select FASTA file");
      return;
    }
    Path fastaPath = PathUtils.existing(fasta);
    if (fastaPath == null) {
      SwingUtils.showInfoDialog(this, "FASTA file does not exist.", "Select FASTA file");
      return;
    }
    String[] opts = new String[]{"Add decoys", "Add decoys and contaminants", "Cancel"};
    int choice = SwingUtils.showChoiceDialog(this, "Update FASTA file", "What would you like to do?", opts, 2);
    if (choice < 0 || choice >= opts.length - 1) {
      log.debug("User cancelled db update action");
      return;
    }
    try {
      DownloadDbHelper.updateDb(this, philosopherBinPath, fastaPath, choice == 1);
    } catch (Exception e) {
      log.error("Database update command error", e);
    }

  }

  private JPanel createPanelInfo() {
    JPanel p = mu.newPanel("", true);
    JEditorPane epInfo = SwingUtils
        .createClickableHtml(createSeqDbExplanationContent());
    epInfo.setPreferredSize(new Dimension(400, 100));
    mu.add(p, epInfo).growX().wrap();

    return p;
  }

  private FastaContent readFasta(Path p) throws ValidationException {
    FastaContent fasta;
    try {
      fasta = FastaUtils.readFasta(p);
    } catch (IOException e) {
      throw new ValidationException(e);
    }
    return fasta;
  }

  public String checkFastaPath() {
    Matcher matcher = disallowedFastaPattern.matcher(getFastaPath());
    if (matcher.find()) {
      return "FASTA file path contains disallowed characters: " + matcher.group() + "\nPlease rename them and try again.";
    } else {
      return null;
    }
  }

  public static JFileChooser createFilechooserFasta(UiText uiTextDbPath) {
    FileNameExtensionFilter exts = new FileNameExtensionFilter("FASTA", "fa", "fas", "fasta");
    JFileChooser fc = FileChooserUtils
        .create("Select FASTA file", false, FcMode.FILES_ONLY, exts);
    fc.setFileFilter(exts);
    FileChooserUtils.setPath(fc, Stream.of(uiTextDbPath.getNonGhostText(), ThisAppProps.load(ThisAppProps.PROP_DB_FILE_IN)));
    return fc;
  }

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void on(MessageDecoyTag m) {
    log.debug("Updating decoy tag text field to: {}", m.tag);
    uiTextDecoyTag.setText(m.tag);
    Path fasta = PathUtils.existing(getFastaPath());
    if (fasta != null) {
      validateFasta(fasta.toString());
    }
  }

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void on(MessageDbNewPath m) {
    uiTextDbPath.setText(m.path);
    validateFasta(m.path);
  }

  public String getDecoyTag() {
    return uiTextDecoyTag.getNonGhostText();
  }

  public String getFastaPath() {
    return uiTextDbPath.getNonGhostText();
  }

  private void validateFasta(String path) {
    try {
      if (path == null) {
        throw new Exception("Invalid fasta file: " + path);
      }

      Path p = Paths.get(path);

      if (!Files.exists(p) || !Files.isRegularFile(p) || !Files.isReadable(p)) {
        throw new Exception("Invalid fasta file: " + path);
      }

      if (Files.size(p) < databaseSizeLimit) {
        FastaContent fasta = FastaUtils.readFasta(p);
        final String tag = getDecoyTag();
        int decoysCnt = (int)FastaUtils.getDecoysCnt(fasta.ordered.get(0), tag);
        int protsTotal = FastaUtils.getProtsTotal(fasta.ordered.get(0));
        Bus.postSticky(new NoteConfigDatabase(p, protsTotal, decoysCnt, false, true));
      } else {
        Bus.postSticky(new NoteConfigDatabase(p, Integer.MAX_VALUE, Integer.MAX_VALUE, true, true));
      }
    } catch (AccessDeniedException e) {
      log.warn("No access to FASTA file path: {}", path);
    } catch (Exception e) {
      log.debug("Got bad FASTA path: {}", path);
      Bus.postSticky(new NoteConfigDatabase());
    }
  }

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void on(NoteConfigDatabase m) {
    if (m.isValid) {
      uiTextDbPath.setText(m.path.toString());
      if (m.isBigDatabase) {
        epDbInfo.setText("The file is very big. Do not check the target and decoy counts. Please make sure that the decoy tag is correct and the percentage is 50%.");
      } else {
        epDbInfo.setText(String.format("File contains <b>%d entries (%d decoys: %.1f%%)", m.numEntries, m.decoysCnt, ((double)m.decoysCnt)/m.numEntries * 100.0));
      }
    } else {
      epDbInfo.setText("");
    }

  }

  private void actionDetectDecoys(ActionEvent e) {
    Path path = PathUtils.existing(getFastaPath());
    if (path == null) {
      SwingUtils.showInfoDialog(this, "Select a valid FASTA file first", "FASTA file missing");
      return;
    }

    FastaDecoyPrefixSearchResult fastaDecoyPrefixSearchResult = new FastaDecoyPrefixSearchResult(
        path, this)
        .invoke();
    if (fastaDecoyPrefixSearchResult.isError()) {
      return;
    }
    String tag = fastaDecoyPrefixSearchResult.getSelectedPrefix();
    if (tag != null) {
      Bus.post(new MessageDecoyTag(tag));
    }
  }

  private void actionDbDownload(ActionEvent e) {
    try {
      DownloadDbHelper.downloadDb(this, philosopherBinPath, getFastaPath());
    } catch (Exception ex) {
      Notifications.showException(TIP_DB_DOWNLOAD, btnDownload, ex, true);
    }
  }

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void on(MessageUiRevalidate m) {
    if (m.validateFasta) {
      validateFasta(getFastaPath());
    }
  }

  public static String createSeqDbExplanationContent() {
    JLabel label = new JLabel();
    String content = ""
        + "\"Browse\" to select a FASTA file from a previous FragPipe analysis, or \"Download\" to retrieve a new one from UniProt.<br/>"
        + "Use \"Add decoys\" to append decoy sequences and/or add common <a href=\"https://www.thegpm.org/crap/\">contaminant sequences</a>.<br><br>"
        + "IMPORTANT: Sequence headers must follow certain <a href=\"https://github.com/Nesvilab/philosopher/wiki/How-to-Prepare-a-Protein-Database#header-formatting\">format rules</a>.<br><br>"
        + "To download a database containing two or more organisms during 'Download', list all UniProt proteome IDs separated by commas, e.g., UP000005640,UP000464024 to get a combined human + COVID-19 database.<br>"
        + "<br/>"
        + "<br/>";
    return content;
  }
}
