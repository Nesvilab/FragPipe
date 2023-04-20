# Diagnostic Ion Mining with PTM-Shepherd in FragPipe

Diagnostic ion mining allows allows you to find fragmentation for PTMs from offset or open searches. These 
can be used for things like chemical probe design or as input into MS-Fragger's labile mode in subsequent 
searches. For info about the labile mode, check out the [MSFragger Labile paper](https://www.mcponline.org/article/S1535-9476(23)00048-8/fulltext). For info about the diagnostic ion mining mode, check out the [diagnostic ion mining paper](https://www.biorxiv.org/content/10.1101/2022.09.12.507594v1.full). 

This tutorial will provide a brief overview of running the diagnostic ion mining tool in FragPipe. To build a labile
search workflow, we recommend loading the Diagnostic-ion-mining workflow in FragPipe. This workflow performs an open search before diagnostic ion mining, so it can be useful if you don't know the mass of your PTM of interest.
However, you can generally get more PSMs by doing an offset search if you already know the mass of your PTM, so we'll demonstrate how to enable diagnostic ion for offset search too.

If you are new to MSFragger or FragPipe searches, please first consult the [Setup](https://fragpipe.nesvilab.org/docs/tutorial_setup_fragpipe.html) and [Basic](https://fragpipe.nesvilab.org/docs/tutorial_fragpipe.html) tutorials. 

## Tutorial contents
* Diagnostic ion mining overview
* Running a diagnostic ion mining search using the default workflow
* Running a diagnostic ion mining search using an offset search
* Interpreting results
* Using PTM fragmentation patterns in subsequent labile searches
