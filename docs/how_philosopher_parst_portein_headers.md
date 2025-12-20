# How Philosopher Parses Protein Headers

## Overview

Philosopher parses protein headers in two stages:
1. **FASTA file parsing** (`lib/fas/fas.go`) - reads the raw header
2. **Header classification and field extraction** (`lib/dat/db.go`) - parses header components

---

## Stage 1: Initial FASTA Parsing

In `lib/fas/fas.go:28-69`:
- Lines starting with `>` are treated as headers
- The `>` character is stripped
- Tabs are converted to spaces
- Headers can be up to 1MB in size

---

## Stage 2: Database Classification

The `Classify()` function (`lib/dat/db.go:381-417`) determines the database type by examining header prefixes (after removing decoy/contaminant tags):

| Database Type | Detection Criteria |
|--------------|-------------------|
| **UniProt** | Starts with `sp\|`, `tr\|`, or `db\|` |
| **NCBI** | Starts with `AP_`, `NP_`, `YP_`, `XP_`, `ZP`, or `WP_` |
| **CPTAC Ensembl** | Contains `ENSP` AND `\|ENST` AND `\|ENSG` |
| **Ensembl** | Starts with `ENSP` |
| **UniRef** | Starts with `UniRef` |
| **TAIR** | Starts with `AT` and matches pattern `^AT.+\s\|\sSymbols:[^\|]+\s\|[^\|]+\s\|.*` |
| **NextProt** | Starts with `nxp` |
| **Generic** | Everything else (fallback) |

---

## Stage 3: Field Extraction by Database Type

Each database type uses specific regex patterns to extract fields:

### UniProt Format

Expected format: `sp|ACCESSION|ENTRYNAME ProteinName OS=Organism OX=TaxID GN=GeneName PE=# SV=#`

| Field | Regex Pattern |
|-------|---------------|
| ID (Accession) | `[sp\|tr]\|(.+?)\|` |
| Entry Name | `\w+\|.+?\|(.+?)\s` |
| Protein Name | `[[:alnum:]]+\_[[:alnum:]]+\s(.+?)\s[[:upper:]][[:upper:]]\=.+` |
| Organism | `OS\=(.+?)\s?[OX\=\|GN\=\|PE\=\|$?]` |
| Gene Name | `GN\=([[:graph:]]+)` |
| Protein Existence | `PE\=(.+)\s[\[\|OX\=\|GN\=\|SV\=\|$]` (values 1-5) |

### NCBI Format

Expected format: `XX_123456789.1 Protein description [Organism name]`

| Field | Regex Pattern |
|-------|---------------|
| ID | `(\w{2}_\d{1,10}\.?(\d{1,2})?)` |
| Entry Name | `(\w{2}_\d{1,10}\.?(\d{1,2})?)` |
| Protein Name | `[[:graph:]]\s(.+)` |
| Organism | `\[(.+)\]$` (in brackets at end) |
| Gene Name | `GN\=(.+)\s[\[\|OX\=\|GN\=\|PE\=\|$]` |

### Ensembl Format

Expected format: `ENSP00000123456 description:Protein description`

| Field | Regex Pattern |
|-------|---------------|
| ID | `(ENSP\w+\.?[^\|])` |
| Entry Name | `(ENSP\w+\.?[^\|])` |
| Protein Name | `description\:(.+)\s?$` |
| Gene Name | `(ENSG\d{1,11}\.?\d?\d?)` |

### CPTAC Ensembl Format

Expected format: `ENSP...|ENST...|ENSG...|Protein description`

| Field | Regex Pattern |
|-------|---------------|
| ID | `(ENSP\w+\.?[^\|])` |
| Entry Name | `(ENSP\w+\.?[^\|])` |
| Protein Name | `ENS[P\|T\|G]\d{1,11}\|ENS[P\|T\|G]\d{1,11}\|ENS[P\|T\|G]\d{1,11}\|(.+)$` |
| Gene Name | `(ENSG\d{1,11}\.?\d?\d?)` |

### NextProt Format

Expected format: `nxp|ENTRY_ID|GeneName|ProteinName`

| Field | Extraction Method |
|-------|-------------------|
| ID | Regex: `nxp\|(.+?)\|` |
| Entry Name | Regex: `nxp\|(.+?)\|` |
| Gene Name | Split by `\|`, take index 2 |
| Protein Name | Split by `\|`, take index 3 |
| Organism | Hardcoded as "Homo sapiens" |

### TAIR Format (Arabidopsis)

Expected format: `AT1G01010 | Symbols: SYMBOL | Description | ...`

| Field | Regex Pattern |
|-------|---------------|
| ID | `^(AT.+)\s\|\sSymbols` |
| Entry Name | `^(AT.+)\s\|\sSymbols` |
| Protein Name | `^AT.+\s\|\sSymbols:[^\|]+\s\|\s([^\|]+)\s\|.*` |
| Gene Name | `\|\sSymbols\:(.+?)\s\|` |

### Generic Format

- **ID**: Returns the entire header
- **Entry Name**: Returns the entire header
- **Protein Name**: Returns the entire header
- All other fields return empty strings

---

## Header Format Requirements Summary

For optimal parsing, headers should follow these patterns:

### UniProt (recommended for most use cases)

```
>sp|P12345|ENTRY_HUMAN Protein name OS=Homo sapiens OX=9606 GN=GENE PE=1 SV=1
>tr|A0A123|ENTRY_HUMAN Protein name OS=Homo sapiens GN=GENE PE=2 SV=1
```

### NCBI

```
>NP_001234567.1 protein description [Homo sapiens]
>XP_001234567.1 predicted protein [Mus musculus]
```

### Ensembl

```
>ENSP00000123456 description:Protein description
```

### NextProt

```
>nxp|NX_P12345|GENENAME|Protein description
```

### TAIR

```
>AT1G01010 | Symbols: SYMBOL | Protein description | other info
```

---

## Notes

- If your headers don't match any of these formats, they'll be classified as **generic** and the entire header will be used as the ID/entry name/protein name.
- Decoy entries are detected by a configurable prefix tag (commonly `rev_`)
- Contaminant entries are detected by the `contam_` prefix
- These prefixes are stripped before classification
