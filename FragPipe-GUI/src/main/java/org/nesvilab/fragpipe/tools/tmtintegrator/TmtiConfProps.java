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

package org.nesvilab.fragpipe.tools.tmtintegrator;

import java.util.Arrays;
import java.util.List;

public class TmtiConfProps {

  public static final String PROP_path = "path";
  public static final String PROP_memory = "memory";
  public static final String PROP_output = "output";
  public static final String PROP_channel_num = "channel_num";
  public static final String PROP_ref_tag = "ref_tag";
  public static final String PROP_ref_d_tag = "ref_d_tag";
  public static final String PROP_min_pep_prob = "min_pep_prob";
  public static final String PROP_min_purity = "min_purity";
  public static final String PROP_min_percent = "min_percent";
  public static final String PROP_min_resolution = "min_resolution";
  public static final String PROP_min_snr = "min_snr";
  public static final String PROP_min_ntt = "min_ntt";
  public static final String PROP_min_site_prob = "min_site_prob";
  public static final String PROP_unique_gene = "unique_gene";
  public static final String PROP_prot_exclude = "prot_exclude";
  public static final String PROP_mod_tag = "mod_tag";
  public static final String PROP_groupby = "groupby";
  public static final String PROP_prot_norm = "prot_norm";
  public static final String PROP_add_Ref = "add_Ref";
  public static final String PROP_psm_norm = "psm_norm";
  public static final String PROP_unique_pep = "unique_pep";
  public static final String PROP_aggregation_method = "aggregation_method";
  public static final String PROP_outlier_removal = "outlier_removal";
  public static final String PROP_glyco_qval = "glyco_qval";
  public static final String PROP_use_glycan_composition = "use_glycan_composition";
  public static final String PROP_best_psm = "best_psm";
  public static final String PROP_allow_overlabel = "allow_overlabel";
  public static final String PROP_allow_unlabeled = "allow_unlabeled";
  public static final String PROP_ms1_int = "ms1_int";
  public static final String PROP_top3_pep = "top3_pep";
  public static final String PROP_print_RefInt = "print_RefInt";
  public static final String PROP_max_pep_prob_thres = "max_pep_prob_thres";
  public static final String PROP_log2transformed = "log2transformed";
  public static final String PROP_abundance_type = "abn_type";
  public static final String PROP_label_masses = "label_masses";

  public static final List<String> PROPS = Arrays
      .asList(PROP_path, PROP_memory, PROP_output, PROP_channel_num, PROP_label_masses,
          PROP_ref_tag, PROP_ref_d_tag, PROP_min_pep_prob, PROP_min_purity, PROP_min_percent, PROP_min_resolution, PROP_min_snr, PROP_min_ntt, PROP_min_site_prob,
          PROP_unique_gene, PROP_prot_exclude, PROP_mod_tag, PROP_groupby, PROP_prot_norm,
          PROP_add_Ref, PROP_psm_norm, PROP_unique_pep, PROP_outlier_removal, PROP_best_psm,
          PROP_allow_overlabel, PROP_allow_unlabeled, PROP_ms1_int, PROP_top3_pep,
          PROP_print_RefInt, PROP_max_pep_prob_thres, PROP_aggregation_method, PROP_glyco_qval, PROP_use_glycan_composition,
          PROP_log2transformed, PROP_abundance_type);

  public static List<ComboValue> COMBO_GROUP_BY = Arrays.asList(
      new ComboValue("0", "Gene level", "PSM aggregation to the gene level"),
      new ComboValue("1", "Protein", ""),
      new ComboValue("2", "Peptide sequence", ""),
      new ComboValue("3", "Multiple PTM sites", ""),
      new ComboValue("4", "Single PTM site", ""),
      new ComboValue("-1", "All", "generate reports at all levels")
      );

  public static List<ComboValue> COMBO_NORM = Arrays.asList(
      new ComboValue("0", "None", ""),
      new ComboValue("1", "MD (median centering)", ""),
      new ComboValue("2", "GN (median centering + variance scaling)", ""),
      new ComboValue("-1", "All", "generate reports with all normalization options")
  );

  public static List<ComboValue> COMBO_UNIQUE_GENE = Arrays.asList(
      new ComboValue("0", "Keep all PSMs", "Allow all PSMs"),
      new ComboValue("1", "No PSMs mapping to multiple genes in dataset", "Remove PSMs mapping to more than one GENE with evidence of expression in the dataset"),
      new ComboValue("2", "No PSMs mapping to multiple genes in FASTA", "Remove all PSMs mapping to more than one GENE in the fasta file")
  );

  public static List<ComboValue> COMBO_PEPTIDE_PROTEIN_UNIQUENESS = Arrays.asList(
      new ComboValue("false", "Unique+Razor", "Unique+Razor"),
      new ComboValue("true", "Unique only", "Unique only")
  );

  public static List<ComboValue> COMBO_AGGREGATION_METHOD = Arrays.asList(
      new ComboValue("0", "Median", "Median"),
      new ComboValue("1", "Weighted", "Weighted")
  );

  public static List<ComboValue> COMBO_ABUNDANCE_TYPE = Arrays.asList(
          new ComboValue("0", "Ratio", "Ratio"),
          new ComboValue("1", "Abundance", "Abundance")
  );


  public static String COMBO_ADD_REF_CHANNEL = "Reference sample";
  public static List<ComboValue> COMBO_ADD_REF = Arrays.asList(
      new ComboValue("-1","Reference sample", ""),
//      new ComboValue("0", "Sum", ""),
      new ComboValue("1", "Virtual", "")
      //new ComboValue("2", "Virtual - Median", "")
  );

}
