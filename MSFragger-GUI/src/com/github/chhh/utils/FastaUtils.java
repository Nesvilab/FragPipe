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

package com.github.chhh.utils;

import com.github.chhh.utils.PrefixCounter.Mode;
import com.github.chhh.utils.PrefixCounter.Node;
import java.awt.Component;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import javax.swing.JOptionPane;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class FastaUtils {
  private static final Logger log = LoggerFactory.getLogger(FastaUtils.class);

  public static double getDecoysPct(List<String> fastaDescriptorLines, String decoyTag) {
    return getDecoysCnt(fastaDescriptorLines, decoyTag)/(double)getProtsTotal(fastaDescriptorLines);
  }

  public static long getDecoysCnt(List<String> fastaDescriptorLines, String decoyTag) {
    return fastaDescriptorLines.stream().filter(line -> line.startsWith(decoyTag)).count();
  }

  public static int getProtsTotal(List<String> fastaDescriptorLines) {
    return fastaDescriptorLines.size();
  }

  public static FastaContent readFasta(Path p) throws IOException {
    if (Files.isDirectory(p))
      throw new IOException("Must be an ordinary file, not a directory");
    List<String> descriptors = new ArrayList<>();
    List<List<String>> ordered = new ArrayList<>();
    try (BufferedReader br = new BufferedReader(new InputStreamReader(Files.newInputStream(p),
        StandardCharsets.UTF_8))) {
      String line;
      while ((line = br.readLine()) != null) {
        if (!line.startsWith(">")) {
          continue;
        }
        int pos = 1, next;
        int depth = 1;
        while ((next = line.indexOf('|', pos)) >= 0 || pos < line.length()) {
          if (next < 0) {
            next = line.length();
          }
          String desc = line.substring(pos, next).trim();
          descriptors.add(desc);
          if (ordered.size() < depth) {
            ordered.add(new ArrayList<String>());
          }
          ordered.get(depth - 1).add(desc);
          pos = next + 1;
          depth++;
        }
      }
    } catch (IOException ex) {
      log.error("Could not read fasta file: \"" + p + "\"", ex);
      throw ex;
    }
    return new FastaContent(descriptors, ordered);
  }

  public static class FastaContent {
    public final List<String> descriptors;
    public final List<List<String>> ordered;

    public FastaContent(List<String> descriptors,
        List<List<String>> ordered) {
      this.descriptors = descriptors;
      this.ordered = ordered;
    }
  }

  public static class InferFastaPrefixesAndSuffixes {

    private List<List<String>> ordered;
    private List<List<Tuple2<String, Double>>> prefixesByCol;
    private List<List<Tuple2<String, Double>>> suffixesByCol;

    public InferFastaPrefixesAndSuffixes(List<List<String>> ordered) {
      this.ordered = ordered;
    }

    public List<List<Tuple2<String, Double>>> getPrefixesByCol() {
      return prefixesByCol;
    }

    public List<List<Tuple2<String, Double>>> getSuffixesByCol() {
      return suffixesByCol;
    }

    public InferFastaPrefixesAndSuffixes invoke() {
      prefixesByCol = new ArrayList<>();
      suffixesByCol = new ArrayList<>();

      for (int descCol = 0; descCol < ordered.size(); descCol++) {

        List<String> descriptorCol = ordered.get(descCol);
        final int maxDepth = 16;
        PrefixCounter cntFwd = new PrefixCounter(Mode.FWD, maxDepth);
        PrefixCounter cntRev = new PrefixCounter(Mode.REV, maxDepth);

        for (int i = 0; i < descriptorCol.size(); i++) {
          String descriptor = descriptorCol.get(i);
          cntFwd.add(descriptor);
          cntRev.add(descriptor);
        }
        final long total = descriptorCol.size();
        final StringBuilder sb = new StringBuilder();

        final double pctMin = 0.3;
        final double pctMax = 0.7;

        { // prefixes
          final List<Tuple2<String, Double>> result = new ArrayList<>();
          Proc2<Node, Mode> action = (n, mode) -> {

            Node cur = n;
            if (cur.getTerminals() > 0)
              return; // no prefix or a suffix can be a whole protein id
            double pct = cur.getHits() / (double) total;
            if (pct < pctMin || pct > pctMax) {
              return;
            }
            sb.setLength(0);
            while (cur != null) {
              if (cur.parent != null) {
                sb.append(cur.ch);
              }
              cur = cur.parent;
            }

            if (sb.length() < 2) {
              return; // no prefixes smaller than 2 characters
            }

            StringBuilder sbPrint = sb
                .reverse();// mode == PrefixCounter.Mode.REV ? sb.reverse() : sb;
            result.add(new Tuple2<>(sbPrint.toString(), pct));
          };
          cntFwd.iterPrefixCounts(maxDepth, action);
          prefixesByCol.add(cleanUpDecoyTagCandidates(result));
        }

        { // suffixes
          final List<Tuple2<String, Double>> result = new ArrayList<>();
          Proc2<Node, Mode> action = new Proc2<Node, Mode>() {
            @Override
            public void call(Node n, Mode mode) {

              Node cur = n;
              if (cur.getTerminals() > 0)
                return; // a prefix or a suffix can never be the whole protein id
              double pct = cur.getHits() / (double) total;
              if (pct < pctMin || pct > pctMax) {
                return;
              }
              sb.setLength(0);
              while (cur != null) {
                if (cur.parent != null) {
                  sb.append(cur.ch);
                }
                cur = cur.parent;
              }

              if (sb.length() < 2) {
                return; // no suffixes smaller than 2 chars
              }

              StringBuilder sbPrint = sb;// mode == PrefixCounter.Mode.REV ? sb.reverse() : sb;
              result.add(new Tuple2<>(sbPrint.toString(), pct));
            }
          };
          cntRev.iterPrefixCounts(maxDepth, action);
          suffixesByCol.add(cleanUpDecoyTagCandidates(result));
        }
      }
      return this;
    }

    private List<Tuple2<String, Double>> cleanUpDecoyTagCandidates(
        List<Tuple2<String, Double>> candidates) {
      List<Tuple2<String, Double>> result = new ArrayList<>();

      Collections.sort(candidates, (t1, t2) -> {
        int cmp0 = Double.compare(Math.abs(t1.item2 - 0.5), Math.abs(t2.item2 - 0.5));
        if (cmp0 == 0) {
          cmp0 = Integer.compare(t1.item1.length(), t2.item1.length());
        }
        return cmp0;
      });

      for (int i = 0; i < candidates.size(); i++) {
        Tuple2<String, Double> cur = candidates.get(i);
        String prefix = cur.item1;
        double pct = cur.item2;

        if (prefix.endsWith("-") || prefix.endsWith("_")) {
          result.add(cur);
          continue;
        }

        if (i + 1 < candidates.size()) {
          Tuple2<String, Double> next = candidates.get(i + 1);
          if (!next.item1.startsWith(prefix) || pct != next.item2) {
            result.add(cur);
            continue;
          }
        } else if ( i == candidates.size() - 1 && result.isEmpty()) {
          result.add(cur);
          continue;
        }
      }

//    for (Tuple2<String, Double> cur : candidates) {
//      boolean isBest = true;
//      for (Tuple2<String, Double> other : candidates) {
//        if (Double.compare(other.item2, cur.item2) == 0) {
//          String curStr = cur.item1;
//          String othStr = other.item1;
//          if (othStr.length() > curStr.length()) {
//            if (othStr.startsWith(curStr) || othStr.endsWith(curStr)) {
//              isBest = false;
//              break;
//            }
//          }
//        }
//      }
//      if (isBest) {
//        result.add(cur);
//      }
//    }
      return result;
    }
  }

  public static class FastaDecoyPrefixSearchResult {

    private boolean isError = false;
    private final Path p;
    private String selectedPrefix;
    private Component comp;

    public FastaDecoyPrefixSearchResult(Path p, Component comp) {
      this.p = p;
      this.comp = comp;
    }

    public boolean isError() {
      return isError;
    }

    public String getSelectedPrefix() {
      return selectedPrefix;
    }

    public FastaDecoyPrefixSearchResult invoke() {
      FastaContent fastaContent;
      try {
        fastaContent = readFasta(p);
      } catch (IOException e) {
        SwingUtils.showErrorDialogWithStacktrace(e, comp);
        isError = true;
        return this;
      }

      List<String> descriptors = fastaContent.descriptors;
      List<List<String>> ordered = fastaContent.ordered;

      InferFastaPrefixesAndSuffixes inferFastaPrefixesAndSuffixes = new InferFastaPrefixesAndSuffixes(
          ordered).invoke();
      List<List<Tuple2<String, Double>>> prefixesByCol = inferFastaPrefixesAndSuffixes
          .getPrefixesByCol();
      List<List<Tuple2<String, Double>>> suffixesByCol = inferFastaPrefixesAndSuffixes
          .getSuffixesByCol();

      int totalCandidates = 0;
      int supportedPrefixes = 0;
      int totalPrefixes = 0;
      int totalSuffixes = 0;
      for (int i = 0; i < prefixesByCol.size(); i++) {
        List<Tuple2<String, Double>> list = prefixesByCol.get(i);
        totalCandidates += list.size();
        totalPrefixes += list.size();
        if (i == 0) {
          supportedPrefixes = list.size();
        }
      }
      for (List<Tuple2<String, Double>> list : suffixesByCol) {
        totalCandidates += list.size();
        totalSuffixes += list.size();
      }

      selectedPrefix = null;
      if (totalCandidates == 0) {
        String msg = "No candidates for decoy tags found";
        String[] options = {"Ok"};
        JOptionPane.showOptionDialog(comp, msg, "Nothing found",
            JOptionPane.DEFAULT_OPTION, JOptionPane.WARNING_MESSAGE, null, options, options[0]);

      } else if (supportedPrefixes == 1) {
        // good, we've found the one good decoy prefix
        Tuple2<String, Double> prefix = prefixesByCol.get(0).get(0);
        StringBuilder sb = new StringBuilder();
        sb.append(String.format(Locale.ROOT,
            "Found candidate decoy tag: \n\"%s\" in % 3.1f%% entries", prefix.item1,
            prefix.item2 * 100d));
        sb.append("\n\nAll found candidates:");
        appendFoundPrefixes(sb, prefixesByCol, suffixesByCol);
        String[] options = {"Set \"" + prefix.item1 + "\" as decoy tag", "Cancel"};
        int result = JOptionPane.showOptionDialog(comp, sb.toString(), "Found prefix",
            JOptionPane.DEFAULT_OPTION, JOptionPane.QUESTION_MESSAGE, null, options, options[0]);
        if (result == 0) {
          selectedPrefix = prefix.item1;
        }

      } else if (supportedPrefixes > 1) {
        // several possible prefixes found
        StringBuilder sb = new StringBuilder();
        sb.append("Found several possible supported decoy tag prefixes.\n")
            .append("Note: only prefixes in the 1st column are supported by downstream tools.\n");
        appendFoundPrefixes(sb, prefixesByCol, suffixesByCol);
        sb.append("\nOnly supported variants are lsited on buttons below.\n");

        List<Tuple2<String, Double>> supported = prefixesByCol.get(0);
        String[] options = new String[supported.size() + 1];
        options[options.length - 1] = "Cancel";
        for (int i = 0; i < supported.size(); i++) {
          options[i] = String.format("Set \"%s\"", supported.get(i).item1);
        }
        int result = JOptionPane
            .showOptionDialog(comp, sb.toString(), "Found several possible prefixes",
                JOptionPane.DEFAULT_OPTION, JOptionPane.QUESTION_MESSAGE, null, options,
                options[0]);
        if (result >= 0 && result < options.length - 1) {
          selectedPrefix = supported.get(result).item1;
        }

      } else if (supportedPrefixes == 0) {
        // no prefixes found - this is not supported by downstream tools
        StringBuilder sb = new StringBuilder();
        sb.append("No supported decoy tag prefixes found.\n")
            .append("However found other possible decoy markers, listed below.\n")
            .append("Note: only prefixes in the 1st column are supported by downstream tools.\n");
        appendFoundPrefixes(sb, prefixesByCol, suffixesByCol);
        String[] options = {"Ok"};
        JOptionPane.showOptionDialog(comp, sb.toString(),
            "Found incompatible decoy marker candidates",
            JOptionPane.DEFAULT_OPTION, JOptionPane.WARNING_MESSAGE, null, options, options[0]);
      }
      isError = false;
      return this;
    }

    private void appendFoundPrefixes(StringBuilder sb,
        List<List<Tuple2<String, Double>>> prefixesByCol,
        List<List<Tuple2<String, Double>>> suffixesByCol) {

      int totalPrefixes = 0;
      int totalSuffixes = 0;
      for (int i = 0; i < prefixesByCol.size(); i++) {
        List<Tuple2<String, Double>> list = prefixesByCol.get(i);
        totalPrefixes += list.size();
      }
      for (List<Tuple2<String, Double>> list : suffixesByCol) {
        totalSuffixes += list.size();
      }

      final String tab1 = "  ";
      final String tab2 = tab1 + tab1;

      if (totalPrefixes > 0) {
        sb.append(tab1).append("\nPrefixes:\n");
        for (int i = 0; i < prefixesByCol.size(); i++) {
          for (Tuple2<String, Double> tuple2 : prefixesByCol.get(i)) {
            sb.append(tab2).append(String.format("\tColumn #%d: \"%s\" in % 3.1f%% entries\n",
                i + 1, tuple2.item1, tuple2.item2 * 100d));
          }
        }
      }

      if (totalSuffixes > 0) {
        sb.append(tab1).append("\nSuffixes:\n");
        for (int i = 0; i < suffixesByCol.size(); i++) {
          for (Tuple2<String, Double> tuple2 : suffixesByCol.get(i)) {
            sb.append(tab2).append(String.format("\tColumn #%d: \"%s\" in % 3.1f%% entries\n",
                i + 1, tuple2.item1, tuple2.item2 * 100d));
          }
        }
      }

    }
  }
}
