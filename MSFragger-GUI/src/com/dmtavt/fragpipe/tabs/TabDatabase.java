package com.dmtavt.fragpipe.tabs;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.api.Bus;
import com.dmtavt.fragpipe.api.Notifications;
import com.dmtavt.fragpipe.exceptions.ValidationException;
import com.dmtavt.fragpipe.messages.MessageDbNewPath;
import com.dmtavt.fragpipe.messages.MessageDecoyTag;
import com.dmtavt.fragpipe.messages.MessageUiRevalidate;
import com.dmtavt.fragpipe.messages.NoteConfigDatabase;
import com.dmtavt.fragpipe.messages.NoteConfigPhilosopher;
import com.github.chhh.utils.FastaUtils;
import com.github.chhh.utils.FastaUtils.FastaContent;
import com.github.chhh.utils.FastaUtils.FastaDecoyPrefixSearchResult;
import com.github.chhh.utils.OsUtils;
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
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.BitSet;
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
import com.dmtavt.fragpipe.api.DownloadDbHelper;
import com.dmtavt.fragpipe.api.OldUtilMethods;
import com.dmtavt.fragpipe.params.ThisAppProps;

public class TabDatabase extends JPanelWithEnablement {
  private static final Logger log = LoggerFactory.getLogger(TabDatabase.class);
  private static final MigUtils mu = MigUtils.get();
  public static final String TAB_PREFIX = "database.";
  public static final String TIP_DB_PATH = "tip.db.path";
  private static final String TIP_DB_DOWNLOAD = "tip.db.download";
  private static final String TIP_DB_UPDATE = "tip.db.update";
  private UiText uiTextDbPath;
  private UiText uiTextDecoyTag;
  private JEditorPane epDbInfo;
  private JButton btnDownload;
  private JButton btnUpdate;

  public TabDatabase() {
    init();
    initMore();
  }

  private void setTooltipBtnDownload(boolean isEnabled) {
    String tip = isEnabled ? null : "Configure Philosopher to enable download button";
    btnDownload.setToolTipText(tip);
  }

  private void initMore() {
    updateEnabledStatus(btnDownload, false);
    setTooltipBtnDownload(false);

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

    uiTextDbPath = UiUtils.uiTextBuilder().filter("[\"'|<>]").cols(5).create();
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
            + "Decoy proteins in the database are identified by this prefix\n"
            + "in their protein ID. If you're unsure what tag is used in your protein\n"
            + "database, use the Try auto-detect button to get a statistic about\n"
            + "the usage of prefixes in the give file.").create();
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
    NoteConfigPhilosopher conf = Bus.getStickyEvent(NoteConfigPhilosopher.class);
    if (conf == null || !conf.isValid()) {
      Notifications.showException(TIP_DB_UPDATE, btnUpdate, new ValidationException("Philosopher not configured"), false);
      return;
    }

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
    int choice = SwingUtils.showChoiceDialog(this, "Update FASTA file",
        "What would you like to do?", opts, 2);
    if (choice < 0 || choice >= opts.length - 1) {
      log.debug("User cancelled db update action");
      return;
    }
    try {
      DownloadDbHelper.updateDb(this, conf.path, fastaPath, choice == 1);
    } catch (Exception e) {
      log.error("Database update command error", e);
    }

  }

  private JPanel createPanelInfo() {
    JPanel p = mu.newPanel("Quick start with protein sequence databases", true);
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

  private JFileChooser createFilechooserFasta(UiText uiTextDbPath) {
    FileNameExtensionFilter exts = new FileNameExtensionFilter("FASTA", "fa", "fas", "fasta");
    JFileChooser fc = FileChooserUtils
        .create("Select FASTA file", false, FcMode.FILES_ONLY, exts);
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

  @Subscribe
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
      Path p = PathUtils.existing(path, true);
      FastaContent fasta = FastaUtils.readFasta(p);
      final String tag = getDecoyTag();
      double decoysPct = FastaUtils.getDecoysPct(fasta.ordered.get(0), tag);
      int decoysCnt = (int)FastaUtils.getDecoysCnt(fasta.ordered.get(0), tag);
      int protsTotal = FastaUtils.getProtsTotal(fasta.ordered.get(0));
      Bus.postSticky(new NoteConfigDatabase(Paths.get(path), protsTotal, decoysCnt, true));
    } catch (Exception e) {
      log.debug("Got bad FASTA path: {}", path);
      Bus.postSticky(new NoteConfigDatabase());
    }
  }

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void on(NoteConfigDatabase m) {
    if (m.isValid) {
      uiTextDbPath.setText(m.path.toString());
      epDbInfo.setText(String.format("File contains <b>%d entries (%d decoys: %.1f%%)", m.numEntries, m.decoysCnt, ((double)m.decoysCnt)/m.numEntries * 100.0));
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

  @Subscribe(sticky = true, threadMode = ThreadMode.MAIN_ORDERED)
  public void on(NoteConfigPhilosopher m) {
    updateEnabledStatus(btnDownload, m.isValid());
    setTooltipBtnDownload(m.isValid());
  }

  private void actionDbDownload(ActionEvent e) {
    NoteConfigPhilosopher conf = Bus.getStickyEvent(NoteConfigPhilosopher.class);
    if (conf == null || !conf.isValid()) {
      Notifications.showException(TIP_DB_DOWNLOAD, btnDownload, new ValidationException("Philosopher not configured"), false);
      return;
    }
    try {
      DownloadDbHelper.downloadDb(this, conf.path, getFastaPath());
    } catch (Exception ex) {
      Notifications.showException(TIP_DB_DOWNLOAD, btnDownload, ex, true);
    }
  }

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void on(MessageUiRevalidate m) {
    validateFasta(getFastaPath());
  }

  public static String createSeqDbExplanationContent() {
    JLabel label = new JLabel();
    Font font = label.getFont();
    final String codeTag = "<code style=\" font-size:" + font.getSize() + "; \">";
    final String bin = OsUtils.isWindows() ? "philosopher.exe" : "philosopher";
    String content = ""
        + "FragPipe requires a standard FASTA formatted protein database to perform the search.<br/>"
        + "Decoy sequences (used for false discovery rate estimation) must be prepended with "
        + "the \"Decoy protein prefix\" (see text field above).<br/><br/>"
        + "If unsure, a good place to start is to go to <a href=\"http://www.uniprot.org/proteomes/\">UniProt website</a>. "
        + "However, if you just download a FASTA file from UniProt, it will not contain decoys.<br/><br/>"
        + "<b>There are two simple ways to get a database complete with decoys added:</b><br/><br/>"
        + "1) Simply click 'Download' button next to the text field above.<br/><br/>"
        + "or<br/><br/>"
        + "2) Run Philosopher from the command line to download protein sequences from UniProt.<br/>"
        + "Execute the following two commands (see <a href=\"https://github.com/Nesvilab/philosopher/wiki/Database\">here</a> for detailed instructions): <br/>"
        + "<br/>"
        + codeTag
        + "&nbsp;&nbsp;&nbsp;&nbsp;" + bin + " workspace --init <br/>"
        + "&nbsp;&nbsp;&nbsp;&nbsp;" + bin + " database --reviewed --contam --id UP000005640<br/>"
        + "</code>"
        + "<br/>"
        + "This will generate a human UniProt (reviewed sequences only) database, with common contaminants and decoys (with a prefix rev_) added.<br/>"
        + "<br/>"
        + "For full UniProt, remove " + codeTag + "--reviewed</code> tag.<br/>"
        + "To include isoforms, add " + codeTag + "--isoform</code> tag.<br/>"
        + "<br/>"
        + "For mouse use UP000000589, to find the proteome ID for other organisms visit <a href=\"http://www.uniprot.org/proteomes/\">UniProt website</a>.<br/>"
        + "<br/>"
        + "To download a database containing two or more organisms, list all UniProt proteome IDs separated by comma.<br/>"
        + "E.g. UP000005640,UP0000464024 to get a combined human + COVID-19 database."
        + "<br/>"
        + "<br/>"
        + "<b>If you have your own custom database:</b><br/><br/>"
        + "The headers in the custom sequence database should follow a certain format. <br/>"
        + "<br/>"
        + "For detailed information on creating and formatting databases for FragPipe analysis, please see <a href=\"https://github.com/Nesvilab/philosopher/wiki/How-to-Prepare-a-Protein-Database\">https://github.com/Nesvilab/philosopher/wiki/How-to-Prepare-a-Protein-Database</a>.<br/>"
        + "<br/>"
        + "<br/>";
    return content;
  }
}
