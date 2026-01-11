# Glycoproteomics Collection

Curated collection of recent manuscripts (last 12 months; compiled in January 2026) that used the MSFragger/FragPipe computational platform for glycoproteomics and related applications.

---

**Uncovering protein glycosylation dynamics and heterogeneity using deep quantitative glycoprofiling (DQGlyco)**  
Potel, C.M.; Burtscher, M.L.; Garrido-Rodriguez, M.; Brauer-Nikonow, A.; Becher, I.; Le Sueur, C.; Typas, A.; Zimmermann, M.; Savitski, M.M.; *Nature Structural & Molecular Biology*. 2025.  
[https://doi.org/10.1038/s41594-025-01485-w](https://doi.org/10.1038/s41594-025-01485-w)

Manuscript describing the DQGlyco workflow for large-scale, quantitative analysis of protein glycosylation. Intact N- and O-glycopeptide LC–MS/MS data were searched using MSFragger within FragPipe to enable high-throughput identification with peptide- and glycan-level FDR control. MSFragger-based searches were applied to single-shot and PGC-prefractionated datasets, supporting confident identification of large numbers of N-glycopeptides and enabling relative quantification across multiplexed TMT experiments. FragPipe outputs were used to assess site-specific microheterogeneity, glycoform distributions across tissues and perturbations, and to support downstream analyses of glycosylation dynamics without relying on extensive prefractionation-specific tuning.

---

**The molecular basis of immunosuppression by soluble CD52 is defined by interactions of N-linked and O-linked glycans with HMGB1 box B**  
DeBono, N.J.; D’Andrea, S.; Bandala-Sanchez, E.; Goddard-Borger, E.; Zenaidee, M.A.; Moh, E.S.X.; Fadda, E.; Harrison, L.C.; Packer, N.H.; *Journal of Biological Chemistry*. 2025.  
[https://doi.org/10.1016/j.jbc.2025.108350](https://doi.org/10.1016/j.jbc.2025.108350)

Structural glycoproteomics study defining how site-specific N- and O-glycosylation of soluble CD52 governs immunosuppressive activity via selective binding to the HMGB1 Box B domain. High-resolution LC–MS/MS with MSFragger/FragPipe glycoproteomics workflows enabled confident localization of a low-occupancy but functionally critical O-glycosylation site at Thr8 and detailed characterization of highly sialylated N-glycans. These experimentally defined glycoforms informed molecular dynamics simulations revealing atomistic glycan-mediated interactions underlying HMGB1 Box B specificity and downstream SIGLEC-10 engagement.

---


**Glycoproteomic and single-protein glycomic analyses reveal zwitterionic N-glycans on natural and recombinant proteins derived from insect cells**  
Yan, S.; Vanbeselaere, J.; Ives, C.; Stenitzer, D.; Nuschy, L.; Wöls, F.; Paschinger, K.; Fadda, E.; Stadlmann, J.; Wilson, I.B.H.; *Molecular & Cellular Proteomics*. 2025.  
[https://doi.org/10.1016/j.mcpro.2025.100981](https://doi.org/10.1016/j.mcpro.2025.100981)

Comprehensive glycoproteomics and glycomics analysis of Sf9 and High Five insect cells revealing unexpectedly complex, zwitterionic N-glycans carrying phosphorylcholine, glucuronic acid, fucose, and pentose modifications. Open-search LC–MS/MS workflows (including FragPipe/MSFragger glyco-N-HCD) enabled detection of non-canonical N-glycan masses on endogenous proteins and insect cell–derived recombinant hemagglutinin and SARS-CoV-2 spike, highlighting the need for glycan-agnostic search strategies for non-mammalian expression systems.

---

**Extracting informative glycan-specific ions from glycopeptide MS/MS spectra with GlyCounter**  
Kothlow, K.; Schramm, H.M.; Markuson, K.A.; Russell, J.H.; Sutherland, E.; Veth, T.S.; Zhang, R.; Duboff, A.G.; Tejus, V.R.; McDermott, L.E.; Dräger, L.S.; Riley, N.M.; *Molecular & Cellular Proteomics*. 2025.  
[https://doi.org/10.1016/j.mcpro.2025.101085](https://doi.org/10.1016/j.mcpro.2025.101085)

Introduces GlyCounter, a fast, open-source tool for extracting oxonium, Y-type, and user-defined glycan-specific fragment ions directly from Thermo `.raw` and `.mzML` files without requiring prior glycopeptide identification. GlyCounter enables rapid assessment of glycopeptide content, fragmentation behavior, enzyme efficiency, and enrichment strategies across diverse glycoproteomics datasets, and includes the *Ynaught* module for targeted extraction of Y-type ions from MSFragger-Glyco search results.

---

**Ultradeep N-glycoproteome atlas of mouse reveals spatiotemporal signatures of brain aging and neurodegenerative diseases**  
Fang, P.; Yu, X.; Ding, M.Y.; Cong, Q.; Jiang, H.; Shi, Q.; Zhao, W.; Zheng, W.; Li, Y.; Ling, Z.; Kong, W.-J.; Yang, P.; Shen, H.; *bioRxiv*. 2025.  
[https://doi.org/10.1101/2025.02.15.638397](https://doi.org/10.1101/2025.02.15.638397)

Large-scale, multi-engine N-glycoproteomics study establishing the deepest mouse N-glycoproteome atlas to date, comprising ~92k precursor glycopeptides, ~62k glycoforms, ~8.9k glycosites, and ~4.6k glycoproteins across five tissues. Integration of complementary enzymes, enrichment strategies, and multiple search engines (including MSFragger-Glyco) enabled confidence-tiered identifications and revealed pronounced tissue-specific microheterogeneity. Spatially resolved brain analyses uncovered age- and disease-associated N-glycosylation signatures in Alzheimer’s and Parkinson’s models, and the resulting data were consolidated into the N-GlycoMiner public resource for site-specific glycoproteome exploration.

---

**Evaluating the performance of photon- and electron-based fragmentation methods in Omnitrap-LCMS analysis of N-glycopeptides**  
Levin, N.; Mohammed, S.; *bioRxiv*. 2025.  
[https://doi.org/10.64898/2025.12.10.693381](https://doi.org/10.64898/2025.12.10.693381)

Systematic evaluation of ultraviolet photodissociation (UVPD), electron ionization dissociation (EID), electron capture dissociation (ECD), and activated-ion ECD (AI-ECD) for LC–MS analysis of complex N-glycopeptides using an Orbitrap–Omnitrap platform. Using MSFragger-Glyco within FragPipe with peptide- and glycan-level FDR control, the study shows that UVPD and EID achieve glycopeptide identification efficiencies comparable to stepped HCD while providing richer glycosidic and cross-ring fragmentation, whereas AI-ECD substantially improves ECD performance via supplemental IR activation. The work highlights both the structural advantages of high-energy fragmentation and the need for enhanced computational support for UVPD/EID glycoproteomics.

---

**In-depth plasma N-glycoproteome profiling using narrow-window data-independent acquisition on the Orbitrap Astral mass spectrometer**  
Jager, S.; Zeller, M.; Pashkova, A.; Schulte, D.; Damoc, E.; Reiding, K.R.; Makarov, A.A.; Heck, A.J.R.; *Nature Communications*. 2025.  
[https://doi.org/10.1038/s41467-025-57916-1](https://doi.org/10.1038/s41467-025-57916-1)

Demonstrates a dedicated narrow-window DIA strategy (nGlycoDIA) for plasma N-glycoproteomics and evaluates its compatibility with MSFragger/FragPipe workflows. DIA data were analyzed using FragPipe’s glyco-N-DIA capabilities via two complementary approaches: DIA-Umpire–based pseudo-DDA conversion followed by MSFragger-Glyco searches, and library-based glyco-DIA searches using MSFragger-generated spectral libraries. MSFragger/FragPipe enabled confident identification and quantification of hundreds to thousands of N-glycopeptides across short LC gradients, showed high overlap with Byonic-based results, and supported robust DIA-NN quantification with low CVs. The study highlights FragPipe as a scalable, open framework for high-throughput DIA glycoproteomics on next-generation high-speed instruments.

---

**AMPylation regulates 5′–3′ exonuclease PLD3 processing**  
Hoffmann, L.; Eckl, E.-M.; Berouti, M.; Pries, M.; Koller, A.; Guhl, C.; Hellmich, U.A.; Hornung, V.; Xiang, W.; Jae, L.T.; Kielkowski, P.; *Molecular & Cellular Proteomics*. 2025.  
[https://doi.org/10.1016/j.mcpro.2025.101051](https://doi.org/10.1016/j.mcpro.2025.101051)

Uses MSFragger/FragPipe to enable site-resolved identification of low-abundance AMPylation on PLD3 from DIA data. DIA-MS datasets were processed through FragPipe workflows (including DIA-Umpire–style pseudo-DDA and open/variable modification searches) to detect AMP-modified peptides despite labile phosphate fragmentation. MSFragger-based identification localized AMPylation sites within the soluble PLD3 domain, supporting downstream functional validation of AMPylation-dependent effects on PLD3 proteolytic processing, dimerization, and exonuclease activity in immune and neuronal contexts.

---

**Glycoproteomics analysis of complement factor H and its complement-regulatory function during *Streptococcus pneumoniae*–associated hemolytic uremic syndrome**  
Baas, L.M.; Wijnsma, K.L.; Zijlstra, F.; van de Kar, N.C.A.J.; ter Steeg, L.; Bouts, A.H.M.; Michels, M.A.H.M.; Langereis, J.D.; Lefeber, D.; Wessels, H.J.C.T.; van den Heuvel, L.P.; *Frontiers in Immunology*. 2025.  
[https://doi.org/10.3389/fimmu.2025.1645196](https://doi.org/10.3389/fimmu.2025.1645196)

Targeted plasma N-glycoproteomics study of complement factor H (FH) in pediatric SP-HUS patients, with MSFragger-Glyco and FragPipe as the core identification framework. LC–MS/MS DDA-PASEF data were searched using MSFragger-Glyco (FragPipe v15) against a curated secreted-protein database and a comprehensive GlyGen-derived N-glycan mass list, with peptide-, glycan-, and PSM-level FDR control at 1%. FragPipe-enabled glycopeptide identification resolved site-specific microheterogeneity across three FH N-glycosites and revealed extensive disease-associated truncation beyond simple desialylation. MSFragger-derived identifications were subsequently used to drive targeted LFQ of FH glycoforms, directly linking FragPipe-based glycoproteomics readouts to downstream functional complement assays.

---

**Glycoprotein structure and function in mammalian immune systems: molecular architecture and regulatory networks**  
Montgomery, R.M.; *Scottish Science Society*. 2025.  

Comprehensive review of glycoprotein-mediated regulation in innate and adaptive immunity, with explicit discussion of modern mass spectrometry–based glycoproteomics workflows. The review highlights the use of MSFragger-Glyco within FragPipe for localization-aware open searches of N- and O-linked glycopeptides, enabling site-specific characterization of immune glycoproteins such as antibodies and complement components. MSFragger-based strategies are described in the context of matched glycan database construction, glycoform-level interpretation, and integration with functional immune assays (e.g., ADCC, complement activation) to link glycosylation heterogeneity with immune regulation and disease phenotypes.

---

**Integrated single-tip IMAC–HILIC enables simultaneous analysis of plant phosphoproteomics and N-glycoproteomics**  
Chen, C.-W.; Chen, T.-A.; Lin, P.-Y.; Lin, S.-Y.; Hsu, C.-C.; *Journal of Proteome Research*. 2025.  
[https://doi.org/10.1021/acs.jproteome.5c00185](https://doi.org/10.1021/acs.jproteome.5c00185)

Introduces a single-tip IMAC–HILIC enrichment strategy for concurrent phosphopeptide and N-glycopeptide analysis in plants, coupled to DDA LC–MS/MS. N-glycoproteomics data were processed using MSFragger within FragPipe (glyco-N-LFQ workflow), enabling database-driven identification of intact N-glycopeptides against a curated plant N-glycan mass list with FDR control and reproducible MS1-based quantification. FragPipe outputs (combined modified peptide tables and XIC areas) were used to benchmark enrichment selectivity, quantify glycopeptides across replicates, and assess PTM remodeling under calcium deprivation, highlighting FragPipe’s role as the core analysis engine for integrated PTM workflows.

---

**Improving glycoproteomic analysis workflow by systematic evaluation of glycopeptide enrichment, quantification, mass spectrometry approach, and data analysis strategies**  
Sun, Z.; Lih, T.M.; Woo, J.; Jiao, L.; Hu, Y.; Wang, Y.; Liu, H.; Zhang, H.; *Analytical Chemistry*. 2024.  
https://doi.org/10.1021/acs.analchem.4c04466

Comprehensive benchmarking study of intact N-glycoproteomics workflows that explicitly evaluates MSFragger-Glyco within FragPipe alongside pGlyco 3.0 and MS-PyCloud. MSFragger-Glyco was applied to ZIC-HILIC–enriched, TMTpro-labeled intact glycopeptides using stepped-HCD fragmentation (25/35/45), enabling localization-aware identification of glycopeptides with peptide-, glycan-, and site-level resolution. The study demonstrates that MSFragger-Glyco achieves competitive coverage of glycopeptides, glycosites, and glycan compositions, and supports robust quantitative analysis when integrated into optimized enrichment and acquisition strategies. FragPipe-based analysis was further used to characterize subtype-specific N-glycosylation differences in breast cancer PDX models, highlighting its utility for large-scale, clinically oriented glycoproteomics.

---

**The molecular basis of immunosuppression by soluble CD52 is defined by interactions of N-linked and O-linked glycans with HMGB1 box B**  
DeBono, N.J.; D’Andrea, S.; Bandala-Sanchez, E.; Goddard-Borger, E.; Zenaidee, M.A.; Moh, E.S.X.; Fadda, E.; Harrison, L.C.; Packer, N.H.; *Journal of Biological Chemistry*. 2025.  
[https://doi.org/10.1016/j.jbc.2025.108350](https://doi.org/10.1016/j.jbc.2025.108350)

Detailed glycoproteomics analysis of recombinant soluble CD52 defining the structural basis of its immunosuppressive activity. Intact N- and O-glycopeptides were analyzed using MSFragger within FragPipe, applying the glyco-O-Hybrid / O-Pair workflow with stringent confidence filtering to localize O-glycosylation. FragPipe-enabled searches confidently localized low-occupancy O-glycosylation to Thr8 using paired EThcD spectra and composition-restricted glycan lists derived from released glycomics. MSFragger-based glycopeptide identification and site-occupancy estimation informed downstream molecular dynamics simulations, linking specific sialylated N- and O-glycoforms to selective HMGB1 Box B binding and SIGLEC-10–mediated immune suppression.

---

**Improving the depth and reliability of glycopeptide identification using Protein Prospector**  
Chalkley, R.J.; Baker, P.R.; *Molecular & Cellular Proteomics*. 2025.  
[https://doi.org/10.1016/j.mcpro.2025.100903](https://doi.org/10.1016/j.mcpro.2025.100903)

Methodological benchmarking study comparing Protein Prospector to several leading intact N-glycopeptide search engines, including MSFragger-Glyco within FragPipe, using a complex mouse liver ZIC-HILIC–enriched dataset. MSFragger was applied in a peptide-first glyco-N-HCD workflow with a large mammalian N-glycan search space and 1% FDR filtering, and was explicitly evaluated for its handling of ammonium-adducted glycopeptides. The study shows that MSFragger robustly recovers peptide backbones and corrects glycan assignments when adducts are included, with substantial overlap with Protein Prospector identifications and complementary strengths relative to glycan-first tools. Results highlight the importance of expansive glycan search spaces, adduct-aware searching, and peptide-first strategies—areas where MSFragger/FragPipe provides competitive and scalable performance for large-scale glycoproteomics. 

---

**Ultradeep N-glycoproteome atlas of mouse reveals spatiotemporal signatures of brain aging and neurodegenerative diseases**  
Fang, P.; Yu, X.; Ding, M.Y.; Cong, Q.; Jiang, H.; Shi, Q.; Zhao, W.; Zheng, W.; Li, Y.; Ling, Z.; Kong, W.-J.; Yang, P.; Shen, H.; *Nature Communications*. 2025.  
[https://doi.org/10.1038/s41467-025-60437-6](https://doi.org/10.1038/s41467-025-60437-6)

Ultra-deep, multi-tissue N-glycoproteomics study integrating complementary enzymes, enrichment strategies, and extensive LC–MS/MS acquisition, with explicit multi-engine data analysis that includes MSFragger-Glyco within FragPipe. MSFragger-Glyco was used in parallel with pGlyco3, StrucGP, and Glyco-Decipher to identify and benchmark >1 million glycopeptide-spectrum matches from ~6.9 million glyco-spectra, contributing substantially to precursor-, glycoform-, glycosite-, and glycoprotein-level coverage. MSFragger-Glyco showed high sensitivity, a preference for longer peptides and sialylated glycans, and strong complementarity to other engines; consensus identifications involving MSFragger were used to define high- and moderate-confidence glycopeptide sets. These FragPipe-enabled results fed into downstream tissue-, region-, aging-, and disease-resolved analyses and underpinned construction of the N-GlycoMiner resource, highlighting MSFragger’s role as a scalable engine for ultra-large glycoproteomics datasets.

---

**Dysregulated inflammation in solid tumor malignancy patients shapes polyfunctional antibody responses to COVID-19 vaccination**  
Purcell, R.A.; Koutsakos, M.; Kedzierski, K.; Chung, A.W.; et al.; *npj Vaccines*. 2025.  
[https://doi.org/10.1038/s41541-025-01268-w](https://doi.org/10.1038/s41541-025-01268-w)

Study of vaccine-induced antibody responses in solid tumor malignancy patients combining functional Fc assays with targeted IgG Fc glycosylation analysis. Antigen-specific IgG1 Fc N-glycopeptides were quantified by LC–MS/MS, with raw data processed using MSFragger in FragPipe to identify defined galactosylated and fucosylated Fc glycoforms. MSFragger-derived glycopeptide intensities were used to calculate relative IgG Fc glycosylation features, which were evaluated alongside FcγR binding, ADCP/ADCC measurements, cytokine profiles, and neutralization assays to assess associations between inflammation, antibody glycosylation, and vaccine responses. 

---

**A multivalent capsule vaccine protects against *Klebsiella pneumoniae* bloodstream infections in healthy and immunocompromised mice**  
Wantuch, P.L.; Robinson, L.S.; Knoot, C.J.; Darwech, I.; Matsuguma, A.M.; Vinogradov, E.; Scott, N.E.; Harding, C.M.; Rosen, D.A.; *npj Vaccines*. 2025.  
[https://doi.org/10.1038/s41541-025-01314-7](https://doi.org/10.1038/s41541-025-01314-7)

Development and preclinical evaluation of a tetravalent *K. pneumoniae* capsule bioconjugate vaccine (K1, K2, KL102, KL107) assessed in immunocompetent and immunocompromised murine bacteremia models. Mass spectrometry was used to characterize vaccine glycoproteins, with intact glycopeptide LC–MS/MS data analyzed using MSFragger in open-search mode to confirm glycan masses and localize polysaccharide attachment sites on the EPA carrier protein. MSFragger-supported glycopeptide identification and annotation (including EThcD and CID spectra) were used to verify glycoform composition and site occupancy prior to in vivo studies, providing analytical validation of bioconjugate structure without extensive downstream computational modeling. 

---



