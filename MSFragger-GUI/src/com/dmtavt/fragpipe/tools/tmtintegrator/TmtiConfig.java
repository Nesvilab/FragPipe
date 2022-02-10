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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with FragPipe.  If not, see <https://www.gnu.org/licenses/>.
 */

package com.dmtavt.fragpipe.tools.tmtintegrator;

import com.github.chhh.utils.StringUtils;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Collectors;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class TmtiConfig {
  private static final Logger log = LoggerFactory.getLogger(TmtiConfig.class);
  private Props tmtintegrator;

  public Props getTmtintegrator() {
    return tmtintegrator;
  }

  public void setTmtintegrator(Props props) {
    this.tmtintegrator = props;
  }

  public static class Props {

    private String path;
    private int memory;
    private String protein_database;
    private String output;
    private int channel_num;
    private String ref_tag;
    private double min_pep_prob;
    private double min_purity;
    private double min_percent;
    private double min_site_prob;
    private int unique_gene;
    private String prot_exclude;
    private String mod_tag;
    private int groupby;
    private int prot_norm;
    private int add_Ref;
    private boolean psm_norm;
    private boolean unique_pep;
    private boolean outlier_removal;
    private boolean best_psm;
    private boolean allow_overlabel;
    private boolean allow_unlabeled;
    private boolean ms1_int;
    private boolean top3_pep;
    private boolean print_RefInt;
    private double max_pep_prob_thres;
    private int min_ntt;
    private int aggregation_method;
    private boolean use_glycan_composition;
    private double glyco_qval;

    public boolean getUse_glycan_composition() {return use_glycan_composition; }

    public void setUse_glycan_composition(boolean use_glycan_composition) {this.use_glycan_composition = use_glycan_composition; }

    public double getGlyco_qval() {return glyco_qval; }

    public void setGlyco_qval(double glyco_qval) {this.glyco_qval = glyco_qval; }

    public double getMax_pep_prob_thres() {
      return max_pep_prob_thres;
    }

    public int getMin_ntt() {
      return min_ntt;
    }

    public int getAggregation_method() {
      return aggregation_method;
    }

    public void setMax_pep_prob_thres(double max_pep_prob_thres) {
      this.max_pep_prob_thres = max_pep_prob_thres;
    }

    public void setMin_ntt(int min_ntt) {
      this.min_ntt = min_ntt;
    }

    public void setAggregation_method(int aggregation_method) {
      this.aggregation_method = aggregation_method;
    }

    public String getPath() {
      return path;
    }

    public void setPath(String path) {
      this.path = path;
    }

    public int getMemory() {
      return memory;
    }

    public void setMemory(int memory) {
      this.memory = memory;
    }

    public String getProtein_database() {
      return protein_database;
    }

    public void setProtein_database(String protein_database) {
      this.protein_database = protein_database;
    }

    public String getOutput() {
      return output;
    }

    public void setOutput(String output) {
      this.output = output;
    }

    public int getChannel_num() {
      return channel_num;
    }

    public void setChannel_num(int channel_num) {
      this.channel_num = channel_num;
    }

    public String getRef_tag() {
      return ref_tag;
    }

    public void setRef_tag(String ref_tag) {
      this.ref_tag = ref_tag;
    }

    public double getMin_pep_prob() {
      return min_pep_prob;
    }

    public void setMin_pep_prob(double min_pep_prob) {
      this.min_pep_prob = min_pep_prob;
    }

    public double getMin_purity() {
      return min_purity;
    }

    public void setMin_purity(double min_purity) {
      this.min_purity = min_purity;
    }

    public double getMin_percent() {
      return min_percent;
    }

    public void setMin_percent(double min_percent) {
      this.min_percent = min_percent;
    }

    public int getUnique_gene() {
      return unique_gene;
    }

    public void setUnique_gene(int unique_gene) {
      this.unique_gene = unique_gene;
    }

    public String getProt_exclude() {
      return prot_exclude;
    }

    public void setProt_exclude(String prot_exclude) {
      this.prot_exclude = prot_exclude;
    }

    public String getMod_tag() {
      return mod_tag;
    }

    public void setMod_tag(String mod_tag) {
      this.mod_tag = mod_tag;
    }

    public int getGroupby() {
      return groupby;
    }

    public void setGroupby(int groupby) {
      this.groupby = groupby;
    }

    public int getProt_norm() {
      return prot_norm;
    }

    public void setProt_norm(int prot_norm) {
      this.prot_norm = prot_norm;
    }

    public int getAdd_Ref() {
      return add_Ref;
    }

    public void setAdd_Ref(int add_Ref) {
      this.add_Ref = add_Ref;
    }

    public double getMin_site_prob() {
      return min_site_prob;
    }

    public void setMin_site_prob(double min_site_prob) {
      this.min_site_prob = min_site_prob;
    }

    public boolean isPsm_norm() {
      return psm_norm;
    }

    public void setPsm_norm(boolean psm_norm) {
      this.psm_norm = psm_norm;
    }

    public boolean isUnique_pep() {
      return unique_pep;
    }

    public void setUnique_pep(boolean unique_pep) {
      this.unique_pep = unique_pep;
    }

    public boolean isOutlier_removal() {
      return outlier_removal;
    }

    public void setOutlier_removal(boolean outlier_removal) {
      this.outlier_removal = outlier_removal;
    }

    public boolean isBest_psm() {
      return best_psm;
    }

    public void setBest_psm(boolean best_psm) {
      this.best_psm = best_psm;
    }

    public boolean isAllow_overlabel() {
      return allow_overlabel;
    }

    public void setAllow_overlabel(boolean allow_overlabel) {
      this.allow_overlabel = allow_overlabel;
    }

    public boolean isAllow_unlabeled() {
      return allow_unlabeled;
    }

    public void setAllow_unlabeled(boolean allow_unlabeled) {
      this.allow_unlabeled = allow_unlabeled;
    }

    public boolean isMs1_int() {
      return ms1_int;
    }

    public void setMs1_int(boolean ms1_int) {
      this.ms1_int = ms1_int;
    }

    public boolean isTop3_pep() {
      return top3_pep;
    }

    public void setTop3_pep(boolean top3_pep) {
      this.top3_pep = top3_pep;
    }

    public boolean isPrint_RefInt() {
      return print_RefInt;
    }

    public void setPrint_RefInt(boolean print_RefInt) {
      this.print_RefInt = print_RefInt;
    }
  }

  public static void write(Map<String, String> map, Writer w) throws IOException {
    final String space = "  ";
    w.write("tmtintegrator:\n");
    for (Entry<String, String> e : map.entrySet()) {
      w.write(String.format("%s%s: %s\n", space, e.getKey(), e.getValue()));
    }
  }

  public static Map<String, String> getDefaultAsMap() {
    final String fn = "tmt-i_param_default.yml";
    final String resoucePath = "com/dmtavt/fragpipe/tools/tmtintegrator/" + fn;
    final Map<String, String> map = new LinkedHashMap<>();
    try (InputStream is = TmtiConfig.class.getClassLoader().getResourceAsStream(resoucePath)) {
      if (is == null)
        throw new IllegalStateException("Resource missing: " + resoucePath);
      List<String> lines = new BufferedReader(new InputStreamReader(is, StandardCharsets.UTF_8))
          .lines().filter(s -> !StringUtils.isNullOrWhitespace(s)).collect(Collectors.toList());
      lines.stream().forEach(l -> {
        String[] s = l.split(":", 2);
        if (s.length != 2 || Arrays.stream(s).anyMatch(StringUtils::isNullOrWhitespace))
          return;
        map.put(s[0].trim(), StringUtils.upToLastChar(s[1], '#', false).trim());
      });

    } catch (IOException e) {
      throw new IllegalStateException(e);
    }
    return map;
  }
}
