package umich.msfragger.util;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import umich.msfragger.util.PrefixCounter.Mode;
import umich.msfragger.util.PrefixCounter.Node;

public class FastaUtils {
  private static final Logger log = LoggerFactory.getLogger(FastaUtils.class);

  public static double checkDecoysPercentage(List<String> fastaDescriptorLines, String decoyTag) {
    double total = fastaDescriptorLines.size();
    long countDecoys = fastaDescriptorLines.stream().filter(line -> line.startsWith(decoyTag)).count();

    return countDecoys/total;
  }

  public static FastaContent readFasta(Path p)
      throws IOException {
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
        while ((next = line.indexOf('|', pos)) >= 0 || pos < line.length() - 1) {
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
      log.error("Could not read fasta file", ex);
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
}
