package com.dmtavt.fragpipe;

import com.dmtavt.fragpipe.api.Bus;
import com.dmtavt.fragpipe.exceptions.ValidationException;
import com.dmtavt.fragpipe.messages.MessageFastaNewPath;
import com.dmtavt.fragpipe.messages.MessageShowException;
import com.dmtavt.fragpipe.messages.NoteConfigDatabase;
import com.github.chhh.utils.swing.ContentChangedFocusAdapter;
import com.github.chhh.utils.swing.FileChooserUtils;
import com.github.chhh.utils.swing.FileChooserUtils.FcMode;
import com.github.chhh.utils.swing.FormEntry;
import com.github.chhh.utils.swing.JPanelWithEnablement;
import com.github.chhh.utils.swing.MigUtils;
import com.github.chhh.utils.swing.UiText;
import com.github.chhh.utils.swing.UiUtils;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.stream.Stream;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JPanel;
import javax.swing.filechooser.FileNameExtensionFilter;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.ThreadMode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import umich.msfragger.params.ThisAppProps;

public class TabDatabase extends JPanelWithEnablement {
  private static final Logger log = LoggerFactory.getLogger(TabDatabase.class);
  private static final MigUtils mu = MigUtils.get();
  public static final String TAB_PREFIX = "database.";
  public static final String TIP_DB_PATH = "tip.db.path";
  private UiText uiTextDbPath;

  public TabDatabase() {
    init();
    initMore();
  }

  private void initMore() {
    Bus.register(this);
  }

  private void init() {
    this.setLayout(new MigLayout(new LC().fillX()));

    JPanel p = mu.panel(false, "FASTA sequence database");
    uiTextDbPath = UiUtils.uiTextBuilder().filter("[\"'|<>]").cols(5).create();
    uiTextDbPath.addFocusListener(new ContentChangedFocusAdapter(uiTextDbPath, (s, s2) -> {
      Bus.post(new MessageFastaNewPath(s2));
    }));
    FormEntry feDbPath = fe(uiTextDbPath, "db-path").label("Fasta file path").create();
    JButton btnBrowse = feDbPath.browseButton("Browse", "Select fasta file",
        () -> createFilechooserFasta(uiTextDbPath),
        paths -> Bus.post(new MessageFastaNewPath(paths.get(0).toString())));

    mu.add(p, feDbPath.label()).split();
    mu.add(p, feDbPath.comp).growX();
    mu.add(p, btnBrowse).wrap();

    mu.add(this, p).growX().wrap();
  }

  private JFileChooser createFilechooserFasta(UiText uiTextDbPath) {
    FileNameExtensionFilter exts = new FileNameExtensionFilter("FASTA", "fa", "fas", "fasta");
    JFileChooser fc = FileChooserUtils
        .create("Select fasta file", false, FcMode.FILES_ONLY, exts);
    FileChooserUtils.setPath(fc, Stream.of(uiTextDbPath.getNonGhostText(), ThisAppProps.load(ThisAppProps.PROP_DB_FILE_IN)));
    return fc;
  }

  @Subscribe
  public void onFastaNewPath(MessageFastaNewPath m) {
    uiTextDbPath.setText(m.path);
    Path p;
    try {
      p = validateFastaPath(m.path);
    } catch (Exception e) {
      log.debug("Got bad fasta path: {}", m.path);
      Bus.post(new MessageShowException(TIP_DB_PATH, uiTextDbPath, e, true));
      return;
    }
    Bus.post(new NoteConfigDatabase(p));
  }

  private Path validateFastaPath(String path) throws Exception {
    Path p = Paths.get(path);
    if (!Files.exists(p)) {
      throw new ValidationException("Path does not exist");
    }
    return p;
  }

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void onNoteConfigDatabase(NoteConfigDatabase m) {
    uiTextDbPath.setText(m.path.toString());
  }

  private FormEntry.Builder fe(JComponent comp, String name) {
    return Fragpipe.fe(comp, name, TAB_PREFIX);
  }
}
