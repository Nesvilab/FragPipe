# FragPipe in Chemoproteomics: Curated Recent Literature

Last updated: 2026-01-10 (America/Detroit)

This repository curates recent peer-reviewed articles and preprints that explicitly report using **FragPipe** (and/or **MSFragger**, **IonQuant**, **diaTracer**, and associated Philosopher validation) in **chemoproteomics** workflows (broadly defined: ABPP, covalent ligandability, proximity labeling, redox and cysteine oxidation profiling, chemoenzymatic PTM mapping, etc.).

## How to use this list
- Use it as a starting point for **method replication**: each entry highlights what FragPipe components were used and (when stated) the key configuration choices (e.g., mass-offset / probe-adduct handling, MBR/LFQ, DIA components).
- If you use FragPipe in chemoproteomics and want your paper included, open an issue or PR.

## ABPP / covalent inhibitor profiling (DDA)

**Upadhyay, T.; Woods, E. C.; Ahator, S. D.; Julin, K.; Faucher, F. F.; Uddin, M. J.; Hollander, M. J.; Pedowitz, N. J.; Abegg, D.; Hammond, I.; Eke, I. E.; Wang, S.; Chen, S.; Bennett, J. M.; Jo, J.; Lentz, C. S.; Adibekian, A.; Fellner, M.; Bogyo, M.** (2025). *Identification of covalent inhibitors of Staphylococcus aureus serine hydrolases important for virulence and biofilm formation*. *Nature Communications*. DOI: [10.1038/s41467-025-60367-3](https://doi.org/10.1038/s41467-025-60367-3)

- FragPipe used for database search + PSM validation (PeptideProphet) and quantification with IonQuant
- Variable modifications used to model probe-derived adduct masses; multi-protease settings supported

## ABPP resource (cysteine, isoDTB; DDA/LFQ)

**Montaño, J.; Ivanova, N.; Finkelstein, Z. A.; Song, A.; Tsai, C. C.; Scheffler, W. J.; Zhang, M.; Li, M. M.; Lee, J. E.; Fong, O. A.; Brown, A. L.; Chamberlain, E. S.; Giacomini, K. D.; Williams, E. K.; Smith, J. L.** (2026). *ProxiCapture: A chemoproteomics resource for covalent chemical probe discovery across the NCI-60*. *Cell Chemical Biology*. DOI: [10.1016/j.chembiol.2025.12.003](https://doi.org/10.1016/j.chembiol.2025.12.003)

- FragPipe (MSFragger/IonQuant) used for identification and site-resolved quantification in isoDTB-ABPP workflows; MBR enabled for LFQ

## Chemoproteomics (cysteine)

**Shikwana, F.; Heydari, B.S.; Ofori, S.; Truong, C.; Turmon, A.C.; Darrouj, J.; Holoidovsky, L.; Gustafson, J.L.; Backus, K.M.** (2024). *CySP3-96 Enables Scalable, Streamlined, and Low-Cost Sample Preparation for Cysteine Chemoproteomic Applications*. *Molecular & Cellular Proteomics*. DOI: [10.1016/j.mcpro.2024.100898](https://doi.org/10.1016/j.mcpro.2024.100898)

- FragPipe used for searching and quantifying cysteine-reactive probe datasets; configured to detect probe adduct mass shifts

## Chemoproteomics (acidic residues, Asp/Glu)

**Qiu, N.; Tan, H.; Pechalrieu, D.; Abegg, D.; Fnu, D.; Mukherjee, P.; Gomez, A.R.; Gutierrez, O.; Powers, D.C.; Adibekian, A.** (2025). *Proteome-wide covalent targeting of acidic residues with tunable N-aryl aziridines*. *ChemRxiv*. DOI: [10.26434/chemrxiv-2025-clgct](https://chemrxiv.org/engage/chemrxiv/article-details/675b3fb6e8a6cd6f5422b8f7)

- FragPipe mass-offset / variable-mod search used to identify and localize aziridine-derived adducts on acidic residues

## Chemoproteomics (kinase inhibitor profiling)

**van Bergen, W.; Nederstigt, A. E.; Dhondt, I.; van der Fliert, S.; van der Zwan, G.; Mohammed, S.** (2025). *Site-specific competitive kinase inhibitor profiling using phosphonate affinity tags (PhosID)*. *Molecular & Cellular Proteomics*. DOI: [10.1016/j.mcpro.2025.100906](https://doi.org/10.1016/j.mcpro.2025.100906)

- FragPipe used for database searching and quantification; configured for probe-derived modifications and site-resolved reporting

## Chemoproteomics / proximity labeling

**Crocker, L. B.; Arafiles, J. V. V.; Müchler, J. M.; Ruwolt, M.; Kemnitz-Hassanin, K.; Roßmann, K.; Stieger, C. E.; Liu, F.; Archipowa, N.; Kutta, R. J.; Hackenberger, C. P. R.** (2025). *Energy-transfer photoproximity labelling in live cells using an organic cofactor*. *Nature Chemistry*. DOI: [10.1038/s41557-025-01931-8](https://doi.org/10.1038/s41557-025-01931-8)

- FragPipe used for identification/quantification of proximity-labeled proteins/peptides from LC–MS/MS datasets

**Wang, W.; Guo, H.; Yan, X.; Pan, X.; Wang, X.; Rong, Y.; Bai, Z.; Zhang, L.; Wu, Z.; Zhao, X.; Huang, W.; Qin, W.; Chu, L.** (2025). *Silicon-rhodamine-enabled identification for near-infrared light controlled proximity labeling in vitro and in vivo*. *Nature Communications*. DOI: [10.1038/s41467-025-63496-x](https://doi.org/10.1038/s41467-025-63496-x)

- FragPipe used for peptide/protein identification and downstream quantification of proximity-labeled proteomes
- Configured to accommodate probe-derived mass shifts

**Zhang, Z.; Wang, Y.; Lu, W.; Wang, X.; Guo, H.; Pan, X.; Liu, Z.; Wu, Z.; Qin, W.** (2025). *Spatiotemporally resolved photocatalytic proximity labeling using a ligand-directed iridium complex*. *Nature Communications*. DOI: [10.1038/s41467-025-57767-w](https://doi.org/10.1038/s41467-025-57767-w)

- FragPipe used as primary database-search/quantification pipeline for labeled proteomes (per Methods in PDF)

## DIA-ABPP (SWATH/DIA)

**Sajic, T.; Vizovišek, M.; Arni, S.; Ciuffa, R.; Mehnert, M.; Lenglet, S.; Weder, W.; Gallart-Ayala, H.; Ivanisevic, J.; Buljan, M.; Thomas, A.; Hillinger, S.; Aebersold, R.** (2025). *Depletion-dependent activity-based protein profiling using SWATH/DIA-MS detects serine hydrolase lipid remodeling in lung adenocarcinoma progression*. *Nature Communications*. DOI: [10.1038/s41467-025-59564-x](https://doi.org/10.1038/s41467-025-59564-x)

- FragPipe/MSFragger used to search and validate DIA-derived identifications in the ABPP context
- Outputs used to quantify serine hydrolase activity profiles across samples

## DIA / LFQ (redox proteomics)

**Kobayashi, D.; Takami, T.; Matsumoto, M.** (2025). *Data-independent acquisition (DIA)-based label-free redox proteomics (DIALRP) identifies prominent cysteine oxidations*. *Journal of Proteome Research*. DOI: [10.1021/acs.jproteome.5c00339](https://doi.org/10.1021/acs.jproteome.5c00339)

- FragPipe used for DIA/LFQ-style analysis with MSFragger/IonQuant; focused on cysteine oxidation readouts

## DIA / LFQ (IonQuant-driven; redox proteomics)

**Tomin, T.; Honeder, S. E.; Liesinger, L.; Gremel, D.; Retzl, B.; Lindenmann, J.; Brcic, L.; Schittmayer, M.; Birner-Gruenberger, R.** (2025). *Increased antioxidative defense and reduced advanced glycation end-product formation by metabolic adaptation in non-small-cell-lung-cancer patients*. *Nature Communications*. DOI: [10.1038/s41467-025-60326-y](https://doi.org/10.1038/s41467-025-60326-y)

- FragPipe used for label-free processing and quantification with IonQuant
- Placed in DIA/LFQ section because the workflow centers on LFQ-style quantification and across-run normalization/MBR

## DIA / LFQ (DIA-focused note)

**He, Y.; Yang, K.; Li, S.; Zelisko, M.; Zhu, Y.; Gurdal, S.; Li, L.** (2025). *TMT-based multiplexed (chemo)proteomics on the Orbitrap Astral Mass Spectrometer*. *Molecular & Cellular Proteomics*. DOI: [10.1016/j.mcpro.2025.100968](https://doi.org/10.1016/j.mcpro.2025.100968)

- FragPipe used for DIA data processing (e.g., diaTracer/DIA-NN components within FragPipe workflows) and quantification reporting

## Chemoenzymatic PTM profiling (DDA/LFQ)

**Farhi, J.; Emenike, B.; Lee, R. S.; Sad, K.; Fawwal, D. V.; Beusch, C. M.; Jones, R. B.; Verma, A. K.; Jones, C. Y.; Foroozani, M.; Reeves, M.; Parwani, K. K.; Bagchi, P.; Deal, R. B.; Katz, D. J.; Corbett, A. H.; Gordon, D. E.; Raj, M.; Spangle, J. M.** (2025). *Dynamic In Vivo Mapping of the Methylproteome Using a Chemoenzymatic Approach*. *Journal of the American Chemical Society*. DOI: [10.1021/jacs.4c08175](https://doi.org/10.1021/jacs.4c08175)

- FragPipe/MSFragger used for database search with variable mass shifts for the propargyl handle (multiple occurrences permitted)
- Percolator/Philosopher used for 1% FDR; LFQ with match-between-runs enabled
