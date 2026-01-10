# FragPipe in Chemoproteomics — Literature Collection

Community-maintained bibliography of peer-reviewed papers and preprints that explicitly report using **FragPipe/MSFragger** (and related FragPipe components) for chemoproteomics and chemistry-driven proteomics workflows.

## Annotated literature (condensed, with workflow notes)

### ABPP / Covalent profiling

- **Montaño, J.; Ivanova, N.; Finkelstein, Z. A.; Song, A.; Tsai, C. C.; Scheffler, W. J.; Zhang, M.; Li, M. M.; Lee, J. E.; Fong, O. A.; Brown, A. L.; Chamberlain, E. S.; Giacomini, K. D.; Williams, E. K.; Smith, J. L. (2026)** — *ProxiCapture: A chemoproteomics resource for covalent chemical probe discovery across the NCI-60*. **Cell Chemical Biology**. https://doi.org/10.1016/j.chembiol.2025.12.003
  - FragPipe (MSFragger/IonQuant) used for identification and site-resolved quantification in isoDTB-ABPP workflows; MBR enabled for LFQ
  - *Workflow note: FragPipe enables modification-centric identification (open/mass-offset searches) and IonQuant-based quantification to localize and quantify covalent probe engagement at the residue level.*

- **Nuber, C.M.; Milton, A.V.; Nissl, B.; Alvarez, M.C.I.; Bissinger, B.R.G.; Sathian, M.B.; Pignot, C.D.; Haberhauer, A.; Wu, D.; Douat, C.; Schneider, S.; Hacker, S.M.; Kielkowski, P.; Konrad, D.B. (2025)** — *A Highly Reactive Cysteine-Targeted Acrylophenone Chemical Probe That Enables Peptide-Protein Bioconjugation and Chemoproteomic Profiling*. **JACS Au**. https://doi.org/10.1021/jacsau.5c00692
  - FragPipe used as the primary search/quantification environment for probe-modified peptides; configured with variable modifications matching acrylophenone adducts
  - *Workflow note: FragPipe enables modification-centric identification (open/mass-offset searches) and IonQuant-based quantification to localize and quantify covalent probe engagement at the residue level.*

- **Sajic, T.; Vizovišek, M.; Arni, S.; Ciuffa, R.; Mehnert, M.; Lenglet, S.; Weder, W.; Gallart-Ayala, H.; Ivanisevic, J.; Buljan, M.; Thomas, A.; Hillinger, S.; Aebersold, R. (2025)** — *Depletion-dependent activity-based protein profiling using SWATH/DIA-MS detects serine hydrolase lipid remodeling in lung adenocarcinoma progression*. **Nature Communications**. https://doi.org/10.1038/s41467-025-59564-x
  - FragPipe/MSFragger used to search and validate DIA-derived identifications in the ABPP context | Outputs used to quantify serine hydrolase activity profiles across samples
  - *Workflow note: FragPipe enables modification-centric identification (open/mass-offset searches) and IonQuant-based quantification to localize and quantify covalent probe engagement at the residue level.*

- **Upadhyay, T.; Woods, E. C.; Ahator, S. D.; Julin, K.; Faucher, F. F.; Uddin, M. J.; Hollander, M. J.; Pedowitz, N. J.; Abegg, D.; Hammond, I.; Eke, I. E.; Wang, S.; Chen, S.; Bennett, J. M.; Jo, J.; Lentz, C. S.; Adibekian, A.; Fellner, M.; Bogyo, M. (2025)** — *Identification of covalent inhibitors of Staphylococcus aureus serine hydrolases important for virulence and biofilm formation*. **Nature Communications**. https://doi.org/10.1038/s41467-025-60367-3
  - FragPipe used for database search + PSM validation (PeptideProphet) and quantification with IonQuant | Variable modifications used to model probe-derived adduct masses; multi-protease settings supported
  - *Workflow note: FragPipe enables modification-centric identification (open/mass-offset searches) and IonQuant-based quantification to localize and quantify covalent probe engagement at the residue level.*

- **Zanon, P.R.A.; Yu, F.; Musacchio, P.Z.; Lewald, L.; Zollo, M.; Krauskopf, K.; Mrdović, D.; Raunft, P.; Maher, T.E.; Cigler, M.; Chang, C.J.; Lang, K.; Toste, F.D.; Nesvizhskii, A.I.; Hacker, S.M. (2025)** — *Profiling the proteome-wide selectivity of diverse electrophiles*. **Nature Chemistry**. https://doi.org/10.1038/s41557-025-01902-z
  - Used MSFragger open search to determine masses of modification, mass-offset search to localize probe-derived modifications across residues, and MSFragger closed searches with IonQuant labeling-enabled quantification to quantify residue engagement and probe selectivity across diverse electrophiles.
  - *Workflow note: FragPipe enables modification-centric identification (open/mass-offset searches) and IonQuant-based quantification to localize and quantify covalent probe engagement at the residue level.*

- **Tian, C.; Sun, L.; Liu, K.; Fu, L.; Zhang, Y.; Chen, W.; He, F.; Yang, J. (2025)** — *Proteome-wide ligandability maps of drugs with diverse cysteine-reactive chemotypes*. **Nature Communications**. https://doi.org/10.1038/s41467-025-60068-x
  - FragPipe integrated with MSFragger/IonQuant/Philosopher; mass-offset strategy used to target selected adduct masses (Mass-Offset-CommonPTMs workflow) | Restricted offset search via mass offsets box; IonQuant used for quantification
  - *Workflow note: FragPipe enables modification-centric identification (open/mass-offset searches) and IonQuant-based quantification to localize and quantify covalent probe engagement at the residue level.*


### Redox and cysteine reactivity profiling

- **Kobayashi, D.; Takami, T.; Matsumoto, M. (2025)** — *Data-independent acquisition (DIA)-based label-free redox proteomics (DIALRP) identifies prominent cysteine oxidations*. **Journal of Proteome Research**. https://doi.org/10.1021/acs.jproteome.5c00339
  - FragPipe used for DIA/LFQ-style analysis with MSFragger/IonQuant; focused on cysteine oxidation readouts
  - *Workflow note: FragPipe supports modification-aware identification and MS1-level quantification to enable site-resolved comparisons of chemically modified versus unmodified cysteine peptides.*

- **Tomin, T.; Honeder, S. E.; Liesinger, L.; Gremel, D.; Retzl, B.; Lindenmann, J.; Brcic, L.; Schittmayer, M.; Birner-Gruenberger, R. (2025)** — *Increased antioxidative defense and reduced advanced glycation end-product formation by metabolic adaptation in non-small-cell-lung-cancer patients*. **Nature Communications**. https://doi.org/10.1038/s41467-025-60326-y
  - FragPipe used for label-free processing and quantification with IonQuant | Placed in DIA/LFQ section because the workflow centers on LFQ-style quantification and across-run normalization/MBR
  - *Workflow note: FragPipe supports modification-aware identification and MS1-level quantification to enable site-resolved comparisons of chemically modified versus unmodified cysteine peptides.*


### DIA-enabled chemoproteomics

- **He, Y.; Yang, K.; Li, S.; Zelisko, M.; Zhu, Y.; Gurdal, S.; Li, L. (2025)** — *TMT-based multiplexed (chemo)proteomics on the Orbitrap Astral Mass Spectrometer*. **Molecular & Cellular Proteomics**. https://doi.org/10.1016/j.mcpro.2025.100968
  - FragPipe used for DIA data processing (e.g., diaTracer/DIA-NN components within FragPipe workflows) and quantification reporting
  - *Workflow note: FragPipe acts as the orchestration layer for DIA processing (e.g., MSFragger-DIA, diaTracer, and/or DIA-NN integration) with standardized quantitative reporting suitable for DIA/LFQ chemoproteomics.*


### Photoproximity and proximity labeling

- **Crocker, L. B.; Arafiles, J. V. V.; Müchler, J. M.; Ruwolt, M.; Kemnitz-Hassanin, K.; Roßmann, K.; Stieger, C. E.; Liu, F.; Archipowa, N.; Kutta, R. J.; Hackenberger, C. P. R. (2025)** — *Energy-transfer photoproximity labelling in live cells using an organic cofactor*. **Nature Chemistry**. https://doi.org/10.1038/s41557-025-01931-8
  - FragPipe used for identification/quantification of proximity-labeled proteins/peptides from LC–MS/MS datasets
  - *Workflow note: FragPipe provides peptide identification and LFQ-based quantification for proximity-labeled samples, supporting statistically robust interactome comparisons.*

- **Wang, W.; Guo, H.; Yan, X.; Pan, X.; Wang, X.; Rong, Y.; Bai, Z.; Zhang, L.; Wu, Z.; Zhao, X.; Huang, W.; Qin, W.; Chu, L. (2025)** — *Silicon-rhodamine-enabled identification for near-infrared light controlled proximity labeling in vitro and in vivo*. **Nature Communications**. https://doi.org/10.1038/s41467-025-63496-x
  - FragPipe used for peptide/protein identification and downstream quantification of proximity-labeled proteomes | Configured to accommodate probe-derived mass shifts
  - *Workflow note: FragPipe provides peptide identification and LFQ-based quantification for proximity-labeled samples, supporting statistically robust interactome comparisons.*

- **Zhang, Z.; Wang, Y.; Lu, W.; Wang, X.; Guo, H.; Pan, X.; Liu, Z.; Wu, Z.; Qin, W. (2025)** — *Spatiotemporally resolved photocatalytic proximity labeling using a ligand-directed iridium complex*. **Nature Communications**. https://doi.org/10.1038/s41467-025-57767-w
  - FragPipe used as primary database-search/quantification pipeline for labeled proteomes (per Methods in PDF)
  - *Workflow note: FragPipe provides peptide identification and LFQ-based quantification for proximity-labeled samples, supporting statistically robust interactome comparisons.*


### Affinity-based and molecular glue interactomics

- **Kazi, R.; Bailey, H.J.; Gerhartz, J.; Shah, V.J.; Toker, B.; Bein, J.; Wild, P.; Nowak, R.P.; Mosler, T.; Dikic, I. (2026)** — *ProxiCapture Reveals Context-Dependent CRBN Interactome Landscape of Molecular Glue Degraders*. **bioRxiv** (Preprint). https://doi.org/10.64898/2026.01.05.697692
  - FragPipe used for LFQ-MBR-style quantitative interactome profiling (MSFragger/IonQuant) to identify molecular glue–dependent CRBN interactors across conditions.
  - *Workflow note: FragPipe LFQ-MBR workflows provide consistent quantitative interactome profiling across conditions to identify context-dependent binding partners.*

- **Bailey, H.J.; Eisert, J.; Kazi, R.; Gerhartz, J.; Pieńkowska, D.E.; Dressel, I.; Vollrath, J.; Kondratov, I.; Matviyuk, T.; Tolmachova, N.; Shah, V.J.; Giuliani, G.; Mosler, T.; Geiger, T.M.; Esteves, A.M.; Santos, S.P.; Sousa, R.L.; Bandeiras, T.M.; Leibrock, E.; Bauer, U. (2025)** — *An engineered cereblon optimized for high-throughput screening and molecular glue discovery*. **Cell Chemical Biology**. https://doi.org/10.1016/j.chembiol.2024.11.002
  - FragPipe used for database searching and quantification of chemoproteomics experiments supporting molecular glue discovery
  - *Workflow note: FragPipe LFQ-MBR workflows provide consistent quantitative interactome profiling across conditions to identify context-dependent binding partners.*


### Chemoenzymatic and enzyme-driven PTMs

- **Kong, S.; Peters-Clarke, T.M.; Delaveris, C.S.; Phojanakong, P.; Steri, V.; Wells, J.A. (2026)** — *Cellular consequences, citrullination substrates, and antigenicity resulting from wild-type and targeted PAD4 on cell surfaces*. **bioRxiv** (Preprint). https://doi.org/10.64898/2026.01.05.696859
  - FragPipe/MSFragger used for modification-centric identification (Arg→Cit) and IonQuant-based quantification supporting site-resolved citrullination mapping/occupancy-style analyses.
  - *Workflow note: FragPipe performs modification-centric searches for chemically installed PTMs with residue-level localization and quantitative reporting.*

- **Farhi, J.; Emenike, B.; Lee, R. S.; Sad, K.; Fawwal, D. V.; Beusch, C. M.; Jones, R. B.; Verma, A. K.; Jones, C. Y.; Foroozani, M.; Reeves, M.; Parwani, K. K.; Bagchi, P.; Deal, R. B.; Katz, D. J.; Corbett, A. H.; Gordon, D. E.; Raj, M.; Spangle, J. M. (2025)** — *Dynamic In Vivo Mapping of the Methylproteome Using a Chemoenzymatic Approach*. **Journal of the American Chemical Society**. https://doi.org/10.1021/jacs.4c08175
  - FragPipe/MSFragger used for database search with variable mass shifts for the propargyl handle (multiple occurrences permitted) | Percolator/Philosopher used for 1% FDR; LFQ with match-between-runs enabled
  - *Workflow note: FragPipe performs modification-centric searches for chemically installed PTMs with residue-level localization and quantitative reporting.*


### Other

- **Qiu, N.; Tan, H.; Pechalrieu, D.; Abegg, D.; Fnu, D.; Mukherjee, P.; Gomez, A.R.; Gutierrez, O.; Powers, D.C.; Adibekian, A. (2025)** — *Proteome-wide covalent targeting of acidic residues with tunable N-aryl aziridines*. **ChemRxiv** (Preprint). https://chemrxiv.org/engage/chemrxiv/article-details/675b3fb6e8a6cd6f5422b8f7
  - FragPipe mass-offset / variable-mod search used to identify and localize aziridine-derived adducts on acidic residues
  - *Workflow note: FragPipe is used as the primary platform for identification and quantitative analysis in chemistry-driven proteomics.*

- **van Bergen, W.; Nederstigt, A. E.; Dhondt, I.; van der Fliert, S.; van der Zwan, G.; Mohammed, S. (2025)** — *Site-specific competitive kinase inhibitor profiling using phosphonate affinity tags (PhosID)*. **Molecular & Cellular Proteomics**. https://doi.org/10.1016/j.mcpro.2025.100906
  - FragPipe used for database searching and quantification; configured for probe-derived modifications and site-resolved reporting
  - *Workflow note: FragPipe is used as the primary platform for identification and quantitative analysis in chemistry-driven proteomics.*

- **Shikwana, F.; Heydari, B.S.; Ofori, S.; Truong, C.; Turmon, A.C.; Darrouj, J.; Holoidovsky, L.; Gustafson, J.L.; Backus, K.M. (2024)** — *CySP3-96 Enables Scalable, Streamlined, and Low-Cost Sample Preparation for Cysteine Chemoproteomic Applications*. **Molecular & Cellular Proteomics**. https://doi.org/10.1016/j.mcpro.2024.100898
  - FragPipe used for searching and quantifying cysteine-reactive probe datasets; configured to detect probe adduct mass shifts
  - *Workflow note: FragPipe is used as the primary platform for identification and quantitative analysis in chemistry-driven proteomics.*

