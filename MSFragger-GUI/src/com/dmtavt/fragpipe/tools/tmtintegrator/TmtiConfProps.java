package com.dmtavt.fragpipe.tools.tmtintegrator;

import java.util.Arrays;
import java.util.List;

public class TmtiConfProps {
  /*
  path: D:\test\TMTIntegrator.jar                 # path to TMT-Integrator jar
  memory: 30                                      # memory allocation, in Gb
  protein_database: D:\test\test.fasta            # protein fasta file
  output: D:\test\reports                         # the location of output files
  channel_num: 10                              # number of channels in the multiplex (e.g. 10, 11)
  ref_tag: Bridge                              # unique tag for identifying the reference channel (Bridge sample added to each multiplex)
  min_pep_prob: 0.9                            # minimum PSM probability threshold (in addition to FDR-based filtering by Philosopher)
  min_purity: 0.5                              # ion purity score threshold
  min_percent: 0.05                            # remove low intensity PSMs (e.g. value of 0.05 indicates removal of PSMs with the summed TMT reporter ions intensity in the lowest 5% of all PSMs)
  min_site_prob: -1                            # site localization confidence threshold (-1: for Global; 0: as determined by the search engine; above 0 (e.g. 0.75): PTMProphet probability, to be used with phosphorylation only)
  unique_gene: 0                               # additional, gene-level uniqueness filter (0: allow all PSMs; 1: remove PSMs mapping to more than one GENE with evidence of expression in the dataset; 2:remove all PSMs mapping to more than one GENE in the fasta file)
  prot_exclude: none                           # exclude proteins with specified tags at the beginning of the accession number (e.g. none: no exclusion; sp|,tr| : exclude protein with sp| or tr|)
  mod_tag: none                                # PTM info for generation of PTM-specific reports (none: for Global data; S[167],T[181],Y[243]: for Phospho; K[170]: for K-Acetyl)
  groupby: -1                                  # level of data summarization(0: PSM aggregation to the gene level; 1: protein; 2: peptide sequence; 3: PTM site; -1: generate reports at all levels)
  prot_norm: -1                                # normalization (0: None; 1: MD (median centering); 2: GN (median centering + variance scaling); -1: generate reports with all normalization options)
  add_Ref: -1                                  # add an artificial reference channel if there is no reference channel (-1: don't add the reference; 0: use summation as the reference; 1: use average as the reference; 2: use median as the reference)
  psm_norm: false                              # perform additional retention time-based normalization at the PSM level
  unique_pep: false                            # allow PSMs with unique peptides only (if true) or unique plus razor peptides (if false), as classified by Philosopher and defined in PSM.tsv files
  outlier_removal: true                        # perform outlier removal
  best_psm: true                               # keep the best PSM only (highest summed TMT intensity) among all redundant PSMs within the same LC-MS run
  allow_overlabel: true                        # allow PSMs with TMT on S (when overlabeling on S was allowed in the database search)
  allow_unlabeled: true                        # allow PSMs without TMT tag or acetylation on the peptide n-terminus
  ms1_int: true                                # use MS1 precursor ion intensity (if true) or MS2 summed TMT reporter ion intensity (if false) as part of the reference sample abundance estimation
  top3_pep: true                               # use top 3 most intense peptide ions as part of the reference sample abundance estimation
  print_RefInt: false                          # print individual reference sample abundance estimates for each multiplex in the final reports (in addition to the combined reference sample abundance estimate)
   */

  public static final String PROP_path = "path";
  public static final String PROP_memory = "memory";
  public static final String PROP_protein_database = "protein_database";
  public static final String PROP_output = "output";
  public static final String PROP_channel_num = "channel_num";
  public static final String PROP_ref_tag = "ref_tag";
  public static final String PROP_min_pep_prob = "min_pep_prob";
  public static final String PROP_min_purity = "min_purity";
  public static final String PROP_min_percent = "min_percent";
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
  public static final String PROP_outlier_removal = "outlier_removal";
  public static final String PROP_best_psm = "best_psm";
  public static final String PROP_allow_overlabel = "allow_overlabel";
  public static final String PROP_allow_unlabeled = "allow_unlabeled";
  public static final String PROP_ms1_int = "ms1_int";
  public static final String PROP_top3_pep = "top3_pep";
  public static final String PROP_print_RefInt = "print_RefInt";
  public static final String PROP_max_pep_prob_thres = "max_pep_prob_thres";

  public static final List<String> PROPS = Arrays
      .asList(PROP_path, PROP_memory, PROP_protein_database, PROP_output, PROP_channel_num,
          PROP_ref_tag, PROP_min_pep_prob, PROP_min_purity, PROP_min_percent, PROP_min_ntt, PROP_min_site_prob,
          PROP_unique_gene, PROP_prot_exclude, PROP_mod_tag, PROP_groupby, PROP_prot_norm,
          PROP_add_Ref, PROP_psm_norm, PROP_unique_pep, PROP_outlier_removal, PROP_best_psm,
          PROP_allow_overlabel, PROP_allow_unlabeled, PROP_ms1_int, PROP_top3_pep,
          PROP_print_RefInt, PROP_max_pep_prob_thres);

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


  public static String COMBO_ADD_REF_CHANNEL = "Reference sample";
  public static List<ComboValue> COMBO_ADD_REF = Arrays.asList(
      new ComboValue("-1","Reference sample", ""),
//      new ComboValue("0", "Sum", ""),
      new ComboValue("1", "Virtual - Average", ""),
      new ComboValue("2", "Virtual - Median", "")
  );

}
