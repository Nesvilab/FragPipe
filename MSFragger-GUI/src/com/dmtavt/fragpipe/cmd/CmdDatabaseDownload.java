package com.dmtavt.fragpipe.cmd;

import java.awt.Component;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import com.dmtavt.fragpipe.tools.philosopher.PhilosopherProps;
import com.github.chhh.utils.StringUtils;
import com.github.chhh.utils.UsageTrigger;

public class CmdDatabaseDownload extends CmdBase {

  public static final String NAME = "DbDownload";

  public CmdDatabaseDownload(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  private List<String> add_multiple_fasta(final Path[] fastas) {
    final List<Path> plist = new ArrayList<>();
    final List<String> cmd = new ArrayList<>();
    for (final Path p : fastas)
      if (p != null)
        plist.add(p);
    if (plist.size() == 0)
      return Collections.emptyList();
    cmd.add("--add");
    if (plist.size() == 1) {
      cmd.add(plist.get(0).toAbsolutePath().normalize().toString());
      return cmd;
    }
    final Path combined_fasta = wd.resolve("add_fasta.fasta");
    try (final BufferedWriter bw = Files.newBufferedWriter(combined_fasta)) {
      for (final Path p : plist) {
        try (final BufferedReader br = Files.newBufferedReader(p)) {
          String line;
          while ((line = br.readLine()) != null)
            if (!StringUtils.isNullOrWhitespace(line)) {
              bw.write(line);
              bw.newLine();
            }
        }
      }
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    }
    cmd.add(combined_fasta.toString());
    return cmd;
  }


  public boolean configure(Component comp, UsageTrigger binPhilosopher, String uniprotId,
      boolean isReviewed, boolean isAddContaminants, boolean isAddIsoforms, boolean isAddDecoys,
                           Path addFastaPath,
                           final Path addSpikeInFasta) {

    initPreConfig();

    List<String> cmd = new ArrayList<>();
    cmd.add(binPhilosopher.useBin(this.wd));
    cmd.add(PhilosopherProps.CMD_DATABASE);
    if (isReviewed) {
      cmd.add("--reviewed");
    }
    if (isAddContaminants) {
      cmd.add("--contam");
    }
    if (isAddIsoforms) {
      cmd.add("--isoform");
    }
    if (!isAddDecoys) {
      cmd.add("--nodecoys");
    }
    cmd.add("--id");
    cmd.add(uniprotId);
    cmd.addAll(add_multiple_fasta(new Path[]{addFastaPath, addSpikeInFasta}));
    ProcessBuilder pb = new ProcessBuilder(cmd);
    pb.directory(this.wd.toFile());
    pbis.add(PbiBuilder.from(pb));

    isConfigured = true;
    return true;
  }

  @Override
  public boolean usesPhi() {
    return true;
  }
}
