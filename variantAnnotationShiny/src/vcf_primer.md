## VCF and Variant Annotation Primer

This primer explains the concepts behind each tab of the Variant Annotation
module: what a VCF file contains, how the summary statistics are derived, what
the filters and annotation options do, and how to read the results. It is
aimed at users who are new to variant data.

### 1. What a VCF file is

A VCF (Variant Call Format) file lists the positions where sequenced DNA
differs from a reference genome. It has two parts:

* **Header** — lines starting with `##` that describe the file: the reference
  contigs, and the meaning of every INFO, FORMAT and FILTER field.
* **Data rows** — one row (record) per variant position.

```
#CHROM POS    ID  REF ALT QUAL FILTER INFO            FORMAT  ALICE   BOB     CAROL
chr1   1000   .   A   G   50   PASS   DP=90;AF=0.33   GT:DP   0/1:30  0/0:28  0/1:32
chr1   2000   .   C   T   45   PASS   DP=85;AF=0.17   GT:DP   0/0:29  0/1:27  0/0:29
```

The first eight columns are **fixed** and present in every VCF:

| Column | Meaning |
|---|---|
| `CHROM` | Contig (chromosome) name |
| `POS` | Position on the contig (1-based) |
| `ID` | Variant identifier, e.g. a dbSNP rsID, or `.` |
| `REF` | Base(s) in the reference genome |
| `ALT` | Alternative base(s) observed; several are separated by commas |
| `QUAL` | Confidence that a variant exists at this site (Phred scale) |
| `FILTER` | `PASS`, `.`, or the names of quality filters the record failed |
| `INFO` | Site-level annotations as `KEY=value` pairs separated by `;` |

When the file contains samples, a `FORMAT` column follows, then **one column
per sample**.

#### Single-sample and multi-sample files

A VCF can hold one sample or many. In a multi-sample file each row is still
one variant position, and each sample gets its own column. Population and
imputed datasets (for example 1000 Genomes) often contain hundreds or
thousands of sample columns. Files with no sample columns are called
*sites-only* VCFs.

### 2. INFO fields vs FORMAT fields

Both are declared in the header, but they describe different things.

| | INFO | FORMAT |
|---|---|---|
| Describes | The variant site | Each individual sample at that site |
| Values per row | One set | One set per sample |
| Header line | `##INFO=<ID=...>` | `##FORMAT=<ID=...>` |
| Layout in the row | `KEY=val;KEY=val` | Keys in the FORMAT column, `val:val` in each sample column |
| In a sites-only VCF | Present | Absent |
| Typical fields | `DP`, `AF`, `AC`, `MQ`, `R2`, `ANN`/`CSQ` | `GT`, `DS`, `GL`, `AD`, `DP`, `GQ` |

Reading the first data row of the example above:

* **INFO** — `DP=90` is the total read depth across all samples; `AF=0.33` is
  the frequency of the G allele in this group.
* **FORMAT** — Alice `0/1:30` carries one A and one G with 30 reads; Bob
  `0/0:28` has two A's; Carol `0/1:32` is heterozygous with 32 reads.

The per-sample depths (30 + 28 + 32) add up to the site-level `INFO/DP` (90).
A field name such as `DP` can exist in both places with different meanings.

#### The header field tables

The **Header: INFO fields** and **Header: FORMAT fields** tables list each
field declared in the header:

| Column | Meaning |
|---|---|
| Field | Short ID used in the data rows |
| Number | Values per record or sample: `1`, `A` (one per ALT allele), `R` (one per allele including REF), `G` (one per possible genotype), `.` (variable) |
| Type | `Integer`, `Float`, `String`, `Character` or `Flag` |
| Description | Free text taken from the header |

Common FORMAT fields:

| ID | Meaning |
|---|---|
| `GT` | Genotype, e.g. `0/1`, `1/1`, `0|1` (`|` = phased, `/` = unphased; `0` = REF, `1` = first ALT) |
| `DS` | Genotype dosage: expected number of ALT alleles, 0–2 (from imputation) |
| `GL` | Log10 genotype likelihoods; for a biallelic diploid site, three values for 0/0, 0/1, 1/1 |
| `AD` | Read depth per allele |
| `DP` | Read depth for this sample |
| `GQ` | Genotype quality (Phred-scaled confidence in the GT call) |
| `PS` | Phase set |

The **Non-reference genotypes per sample** plot requires `GT`.

### 3. Contigs

A **contig** is any named reference sequence that variants can sit on. For
human data this is usually a chromosome (`1`–`22`, `X`, `Y`, `MT`, or
`chr1`… depending on naming style), but a reference genome also contains
sequences that are not chromosomes:

| Example | Kind |
|---|---|
| `chr1`–`chr22`, `chrX`, `chrY` | Chromosomes |
| `chrM` | Mitochondrial genome |
| `chr1_KI270706v1_random` | Known chromosome, unknown position |
| `chrUn_KI270302v1` | Unplaced sequence |
| `chr6_GL000251v2_alt` | Alternate haplotype |
| `chrEBV` | Decoy (Epstein-Barr virus) |

The VCF specification, BAM/SAM and tabix therefore use the neutral term
*contig*.

#### Why the header lists contigs without variants

Variant callers copy the complete contig list from the reference genome into
the header before any variants are found. The list:

* identifies the genome build (contig names and lengths differ between builds);
* records which sequences were analysed, so "no rows" means "nothing found";
* keeps headers identical across samples so files can be merged;
* defines the expected sort order of contigs.

#### Records per contig

The **Contigs** table counts how many records each contig has. The counts come
from the **tabix index** (`.tbi` or `.csi`), a companion file that records
where each contig's rows are stored in the bgzip-compressed VCF, so the app can
count them without reading the whole file. Only contigs that actually have
records are shown. The same contig list populates the chromosome selector in
the Filter tab.

### 4. Record statistics

Record statistics stream through every record once and use only the fixed
columns and `INFO/DP`. None of them is read from an INFO field except the DP
distribution.

| Statistic | Source column(s) |
|---|---|
| Variant types | `REF` and `ALT` |
| FILTER values | `FILTER` |
| QUAL distribution | `QUAL` |
| Variants per chromosome | `CHROM` |
| Substitution spectrum and Ti/Tv | `REF` and `ALT` |
| INFO/DP distribution | `INFO/DP` |

#### Variant types

Each record is classified by comparing `REF` and `ALT`:

| Type | Rule | Example (REF → ALT) |
|---|---|---|
| SNV | REF and ALT are both one base | `A → G` |
| Indel | REF and ALT have different lengths | `A → ATT` (insertion), `ACT → A` (deletion) |
| MNV | Same length, more than one base | `AC → GT` |
| Multiallelic | More than one ALT allele | `A → G,T` |
| Other | ALT is not plain A/C/G/T/N | `<DEL>`, `<CNV>`, `*`, breakends |

#### Multiallelic sites

A comma in `ALT` means that more than one alternative allele was observed at
the same position. Alleles are numbered in order and referenced by `GT`:

```
POS       REF  ALT    FORMAT  ALICE  BOB  CAROL
16050527  C    A,T    GT      0/1    1/2  2/2
```

`0` = C, `1` = A, `2` = T. Alice carries C and A, Bob carries A and T, Carol
carries two T's. Many pipelines split such rows into one row per ALT allele
(`bcftools norm -m-`); the module also splits them during annotation.

#### FILTER values

| Value | Meaning |
|---|---|
| `PASS` | The record passed all quality filters |
| `.` | No filtering was applied; status unknown |
| A filter name, e.g. `LowQual`, `q10` | The record failed that filter |
| Several names, e.g. `LowQual;LowDP` | The record failed more than one filter |

The filters are defined by the tool that produced the VCF, not by this module,
and are declared in the header:

```
##FILTER=<ID=q10,Description="Quality below 10">
##FILTER=<ID=LowDP,Description="Read depth below 10">
```

Every pipeline defines its own names (for example GATK VariantFiltration uses
`QD2`, `FS60`; GATK VQSR uses `VQSRTranche...`; DeepVariant uses `RefCall`).
Failed records are kept in the file and labelled, not deleted. To list the
filters in a file:

```bash
zgrep '^##FILTER' your_file.vcf.gz
```

Example:

```
POS    REF ALT  QUAL  FILTER
1000   A   G    250   PASS     ← confident call, passed all filters
2000   C   T    8     q10      ← QUAL too low
3000   G   A    40    LowDP    ← too few reads covered the site
4000   T   C    .     .        ← no score and no filtering applied
```

#### QUAL

QUAL answers one question: how sure is the variant caller that a variant
really exists at this site in at least one sample? It is a **Phred score**, not
a percentage:

```
QUAL = −10 × log10(probability the variant is not real)
```

| QUAL | Chance the variant is false | Confidence |
|---|---|---|
| 10 | 1 in 10 | 90% |
| 20 | 1 in 100 | 99% |
| 30 | 1 in 1,000 | 99.9% |
| 50 | 1 in 100,000 | 99.999% |
| 100 | 1 in 10¹⁰ | practically certain |
| 200 | 1 in 10²⁰ | practically certain |

QUAL ÷ 10 is the number of zeros in "1 in 1000…". The error probability can
keep shrinking, so QUAL has no upper limit; sites supported by many samples
and many reads often reach hundreds or thousands. Above about 30–50 a call is
effectively certain, so thresholds matter only at the low end (commonly 20 or
30).

QUAL does **not** tell you:

* whether a variant is harmful (that comes from annotation);
* whether each sample's genotype is correct (that is `GQ`, or `GL`/`DS`);
* whether a variant is rare (that is allele frequency, `AF`);
* anything comparable between different variant callers.

In the **QUAL distribution** table, `[100,200)` means "at least 100 and less
than 200"; `1e+03` is 1,000 and `Inf` is infinity. `missing` counts records
with `.` in QUAL. The plot on the Plots tab shows the same values as a
histogram on a log scale. A large pile at low QUAL suggests setting a minimum
QUAL; everything in one bin usually means the pipeline capped or fixed the
values.

#### INFO/DP

`INFO/DP` is the total read depth at the site. Imputed datasets usually have no
`INFO/DP`, because their genotypes are inferred statistically rather than
counted from reads; the module then reports that the VCF has no INFO/DP field.

### 5. Genomic restriction and gene symbols

A VCF contains only positions, not gene names. When gene symbols are entered,
the module converts them to coordinates using reference databases bundled with
the selected genome:

```
"TP53"
  │  org.Hs.eg.db: gene symbol → Entrez gene ID 7157
  ▼
  │  TxDb (selected build): gene ID → chr17:7,661,779-7,687,538 (hg38)
  ▼
  │  add the flank on both sides
  ▼
  │  tabix index: read only the records whose CHROM and POS fall inside
  ▼
selected variants
```

Gene coordinates differ between genome builds, so the reference genome
selection must match the VCF.

The **flank** is extra sequence added to both ends of each gene so that nearby
regulatory regions, such as the promoter, are included:

```
        flank          ┌──── gene ────┐          flank
 ──────[ 2000 bp ]─────████████████████─────[ 2000 bp ]──────
```

| Flank | Use when |
|---|---|
| `0` | Only variants inside the gene |
| `2000` (default) | Gene plus the nearby promoter area |
| `5000`–`10000` | Wider regulatory regions as well |

| Input | Example | Uses a database? |
|---|---|---|
| Chromosomes | `chr17` | No, matches CHROM |
| Regions | `chr17:7565097-7590856` | No, matches CHROM and POS |
| Gene symbols | `TP53` | Yes, symbol → coordinates, then CHROM and POS |

### 6. Annotation

#### Reference resources

The reference genome contains only DNA letters. Annotation combines three
resources:

| Resource | Contents | Analogy |
|---|---|---|
| Reference genome (BSgenome) | The DNA sequence of every contig | The text of a book |
| Transcript database (TxDb) | Where each gene, exon, intron, UTR and coding region starts and ends | The table of contents |
| Organism database (org.Hs.eg.db) | Gene IDs ↔ gene symbols | The index of names |

Protein sequences are not stored; they are computed. For a coding variant the
TxDb locates the codon, the genome supplies its bases, the variant is applied,
and the universal genetic code translates both codons:

```
TxDb:    position lies in TP53 coding exon, codon 175
Genome:  reference codon CGC
Variant: CGC → CAC
Code:    CGC = Arg (R), CAC = His (H)
Result:  nonsynonymous, R175H
```

What a protein *does* is not part of these resources; that comes from
databases such as GO and KEGG, used in the enrichment step.

#### Promoter window

The **TSS** (transcription start site) is where a transcript begins. The two
promoter options define the window around the TSS in which a variant is
labelled `promoter`:

```
                        TSS
                         ↓
 ──────[ upstream 2000 | downstream 200 ]════════ gene ════════▶
        └──────────── promoter ─────────┘
```

2000/200 is a common convention. The Filter-tab flank decides which variants
are *loaded*; the promoter window decides which are *labelled* promoter.

#### Transcripts and RNA splicing

Human genes are split into **exons** (kept) and **introns** (removed). After a
gene is copied into RNA, **splicing** cuts out the introns and joins the exons:

```
pre-mRNA:     [Exon 1]────intron────[Exon 2]────intron────[Exon 3]
                          ✂ splicing ✂
mature mRNA:  [Exon 1][Exon 2][Exon 3]  → translated into protein
```

Introns are marked by **splice sites**: almost every intron starts with `GT`
(donor) and ends with `AG` (acceptor). A variant that changes these signals can
cause exon skipping, intron retention or a shifted cut site, any of which can
break the protein.

Cells can also splice one gene in different ways (**alternative splicing**),
producing several **transcripts** of the same gene. A single variant can
therefore fall in a coding exon of one transcript and in an intron of another.

#### Locations and consequences

Each variant is located against every transcript:

| Location | Meaning |
|---|---|
| coding | Inside the protein-coding sequence |
| spliceSite | At an exon–intron boundary |
| intron | Inside the gene, in a region removed by splicing |
| fiveUTR / threeUTR | In the RNA but not translated |
| promoter | Inside the promoter window |
| intergenic | Not near any gene |

Coding variants receive a predicted consequence and amino-acid change:

| Consequence | Meaning | Example |
|---|---|---|
| synonymous | Codon changes but the amino acid does not | `GCA → GCG`: Ala → Ala |
| nonsynonymous | Amino acid changes (missense) | Arg → His |
| nonsense | Creates a stop codon; the protein is truncated | Gln → Stop |
| frameshift | An indel shifts the reading frame | |

When the dbSNP `SNPlocs` package for the genome is installed, known variants
also receive an **rsID** (for example `rs28934578`) that can be looked up in
dbSNP, ClinVar or gnomAD.

#### Most severe effect

The per-variant view keeps the most severe effect across all transcripts, in
this order:

```
frameshift > nonsense > nonsynonymous > spliceSite > coding > not translated
> synonymous > promoter > fiveUTR > threeUTR > intron > intergenic
```

### 7. Enrichment handoff

Enrichment (over-representation analysis, ORA) asks whether the genes carrying
variants belong to particular pathways or functions more often than expected
by chance. For example, if about 0.5% of all human genes are in a DNA repair
pathway, a random list of 26 genes would be expected to contain about 0.13 of
them; finding 6 would suggest the pathway is enriched.

ClusterProfShinyORA tests the list against:

| Database | Groups genes by | Example terms |
|---|---|---|
| GO Biological Process | What the gene helps the cell do | DNA repair, immune response |
| GO Molecular Function | What the protein does chemically | kinase activity, DNA binding |
| GO Cellular Component | Where the protein is located | nucleus, mitochondrion |
| KEGG | Known pathways and diseases | p53 signaling, insulin signaling |

Options:

* **Gene identifier** — gene symbol (`TP53`) or Entrez ID (`7157`). Choose the
  same Keytype (`SYMBOL` or `ENTREZID`) and organism Human in
  ClusterProfShinyORA.
* **Only genes with coding or splice-site variants** — sends only genes where a
  variant could change the protein, reducing noise from intronic and
  intergenic variants.

Interpretation notes:

* Enrichment generates hypotheses; it does not show that variants cause an
  effect.
* Short gene lists often give few or no significant terms.
* Long genes collect variants by chance and can bias results.
* In population or imputed datasets most variants are common and benign;
  enrichment is most informative for case samples or for variants filtered to
  rare or damaging ones.

### 8. Downloads

| File | Contents | Use it to |
|---|---|---|
| Per-variant table (CSV) | One row per variant with its most severe effect, `n_transcripts` and `all_genes` | Review and filter variants |
| Per-variant, per-transcript table (CSV) | One row per variant × transcript, with codons and protein position | Check the effect on a specific transcript |
| Genes carrying variants (CSV) | `gene_id` and `gene_symbol` of every gene with an annotated variant | Run your own enrichment or compare with a gene panel |
| Filter summary (CSV) | Records scanned and kept, and every filter setting | Document the methods |
| Filtered VCF (`.vcf.bgz`) | Filtered records with the fixed columns, `INFO/DP` and, for files with at most 50 samples, `GT` | Continue in other VCF tools |

The filtered VCF does not contain the annotation results or other INFO/FORMAT
fields such as `DS` or `GL`.

#### Per-variant vs per-transcript

Consider one variant in a gene with three transcripts:

```
Gene ABC:      ──[Ex1]────[Ex2]────[Ex3]────[Ex4]──
                           ↑ variant
Transcript 1:  [Ex1][Ex2][Ex3][Ex4]   → coding
Transcript 2:  [Ex1]     [Ex3][Ex4]   → Ex2 skipped → intron
Transcript 3:  [Ex1][Ex2]             → untranslated part → fiveUTR
```

The **per-transcript** table has three rows for this variant:

| position | transcript_id | location | effect | amino_acid_change |
|---|---|---|---|---|
| 7675088 | T1 | coding | nonsynonymous | R175H |
| 7675088 | T2 | intron | intron | |
| 7675088 | T3 | fiveUTR | fiveUTR | |

The **per-variant** table keeps only the most severe row:

| position | location | effect | amino_acid_change | n_transcripts |
|---|---|---|---|---|
| 7675088 | coding | nonsynonymous | R175H | 3 |

Start with the per-variant table; use the per-transcript table when a specific
transcript matters.

Annotation tables and caches are also written to a project directory on the
server. For uploaded files this is a temporary directory that is removed when
the session ends, so download the files you need.
