package com.dmtavt.fragpipe;

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
import com.github.chhh.utils.PathUtils;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.swing.ContentChangedFocusAdapter;
import com.github.chhh.utils.swing.FileChooserUtils;
import com.github.chhh.utils.swing.FileChooserUtils.FcMode;
import com.github.chhh.utils.swing.FormEntry;
import com.github.chhh.utils.swing.JPanelWithEnablement;
import com.github.chhh.utils.swing.MigUtils;
import com.github.chhh.utils.swing.UiText;
import com.github.chhh.utils.swing.UiUtils;
import java.awt.event.ActionEvent;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.stream.Stream;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JEditorPane;
import javax.swing.JFileChooser;
import javax.swing.JPanel;
import javax.swing.filechooser.FileNameExtensionFilter;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.ThreadMode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import umich.msfragger.gui.FragpipeUtil;
import umich.msfragger.params.ThisAppProps;

public class TabDatabase extends JPanelWithEnablement {
  private static final Logger log = LoggerFactory.getLogger(TabDatabase.class);
  private static final MigUtils mu = MigUtils.get();
  public static final String TAB_PREFIX = "database.";
  public static final String TIP_DB_PATH = "tip.db.path";
  private static final String TIP_DB_DOWNLOAD = "tip.db.download";
  private UiText uiTextDbPath;
  private UiText uiTextDecoyTag;
  private JEditorPane epDbInfo;
  private JButton btnDownload;

  public TabDatabase() {
    init();
    initMore();
  }

  private void initMore() {
    Bus.register(this);
  }

  private void init() {
    this.setLayout(new MigLayout(new LC().fillX()));

    mu.add(this, createPanelDbSelection()).growX().wrap();

  }

  private FormEntry.Builder fe(JComponent comp, String name) {
    return Fragpipe.fe(comp, name, TAB_PREFIX);
  }

  private JPanel createPanelDbSelection() {
    JPanel p = mu.panel(false, "FASTA sequence database");
    uiTextDbPath = UiUtils.uiTextBuilder().filter("[\"'|<>]").cols(5).create();
    uiTextDbPath.addFocusListener(new ContentChangedFocusAdapter(uiTextDbPath, (s, s2) -> {
      Bus.post(new MessageDbNewPath(s2));
    }));
    FormEntry feDbPath = fe(uiTextDbPath, "db-path").label("Fasta file path").create();
    JButton btnBrowse = feDbPath.browseButton("Browse", "Select fasta file",
        () -> createFilechooserFasta(uiTextDbPath),
        paths -> Bus.post(new MessageDbNewPath(paths.get(0).toString())));
    btnDownload = UiUtils.createButton("Download", this::actionDbDownload);
    btnDownload.setEnabled(false);

    String defaultTag = Fragpipe.props().getProperty(ThisAppProps.PROP_TEXTFIELD_DECOY_TAG);
    uiTextDecoyTag = UiUtils.uiTextBuilder().cols(12).text(defaultTag).create();
    FormEntry feDecoyTag = fe(uiTextDecoyTag, "decoy-tag").label("Decoy protein prefix")
        .tooltip( "Decoys are used for FDR estimation.\n"
            + "Decoy proteins in the database are identified by this prefix\n"
            + "in their protein ID. If you're unsure what tag is used in your protein\n"
            + "database, use the Try Auto-Detect button to get a statistic about\n"
            + "the usage of prefixes in the give file.").create();
    JButton btnDecoyDetect = feDecoyTag.button("Try atuo-detect tag", "Input decoy tag",
        this::actionDetectDecoys);

    epDbInfo = SwingUtils.createClickableHtml(true, "");

    mu.add(p, feDbPath.label()).split();
    mu.add(p, feDbPath.comp).growX();
    mu.add(p, btnBrowse);
    mu.add(p, btnDownload).wrap();

    mu.add(p, feDecoyTag.label()).split();
    mu.add(p, feDecoyTag.comp);
    uiTextDecoyTag.addFocusListener(new ContentChangedFocusAdapter(uiTextDecoyTag, (s, s2) -> {
      validateFasta(getFastaPath());
    }));
    mu.add(p, btnDecoyDetect);
    mu.add(p, epDbInfo);
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
        .create("Select fasta file", false, FcMode.FILES_ONLY, exts);
    FileChooserUtils.setPath(fc, Stream.of(uiTextDbPath.getNonGhostText(), ThisAppProps.load(ThisAppProps.PROP_DB_FILE_IN)));
    return fc;
  }

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void onDecoyTag(MessageDecoyTag m) {
    log.debug("Updating decoy tag text field to: {}", m.tag);
    uiTextDecoyTag.setText(m.tag);
    Path fasta = PathUtils.existing(getFastaPath());
    if (fasta != null) {
      validateFasta(fasta.toString());
    }
  }

  @Subscribe
  public void onFastaNewPath(MessageDbNewPath m) {
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
      log.debug("Got bad fasta path: {}", path);
      Bus.postSticky(new NoteConfigDatabase());
    }
  }

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void onNoteConfigDatabase(NoteConfigDatabase m) {
    if (m.isValid) {
      uiTextDbPath.setText(m.path.toString());
      SwingUtils.setJEditorPaneContent(epDbInfo, true,
          String.format("File contains %d proteins (%d decoys: %.1f%%)", m.numEntries, m.decoysCnt, ((double)m.decoysCnt)/m.numEntries * 100.0));
    } else {
      SwingUtils.setJEditorPaneContent(epDbInfo, true, "");
    }

  }

  private void actionDetectDecoys(ActionEvent e) {
    Path path = PathUtils.existing(uiTextDbPath.getNonGhostText());
    if (path == null) {
      SwingUtils.showInfoDialog(this, "Select a valid fasta file first", "Fasta file missing");
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
  public void onNoteConfigPhilosopher(NoteConfigPhilosopher m) {
    btnDownload.setEnabled(m.isValid());
  }

  private void actionDbDownload(ActionEvent e) {
    NoteConfigPhilosopher conf = Bus.getStickyEvent(NoteConfigPhilosopher.class);
    if (conf == null || !conf.isValid()) {
      Notifications.showException(TIP_DB_DOWNLOAD, btnDownload, new ValidationException("Philosopher not configured"), false);
      return;
    }
    try {
      FragpipeUtil.downloadDb(this, conf.path, getFastaPath());
    } catch (Exception ex) {
      Notifications.showException(TIP_DB_DOWNLOAD, btnDownload, ex, true);
    }
  }

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void onUiRevalidate(MessageUiRevalidate m) {
    validateFasta(getFastaPath());
  }
}
