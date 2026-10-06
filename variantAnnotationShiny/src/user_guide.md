## Variant Annotation

This module annotates variants from a VCF file with the Bioconductor
**VariantAnnotation** package. Variants are placed on the UCSC knownGene
transcript model of the selected reference genome, coding variants receive
predicted consequences and amino-acid changes, and the genes carrying variants
can be sent directly to the enrichment (ORA) module.

All annotation runs offline inside the NASQAR2 image. No external services or
licences are required.

### 1. Input Data

Choose one of three sources:

* **Upload VCF file** — one `.vcf`, `.vcf.gz` or `.vcf.bgz` file. Upload the
  matching `.tbi` index in the same step to skip re-indexing.
* **Example VCF** — the chr22 subset of the 1000 Genomes project shipped with
  VariantAnnotation (GRCh37 coordinates, contig name `22`). The hg19
  reference is selected automatically.
* **VCF in the private space** — the absolute path of a VCF under the
  configured read-only data root. The VCF is read in place.

**Projects.** In a private instance every run belongs to a persistent project,
`projects/VariantAnnotation/<name>` in the private space (the *Persistent
project name* box; uploads are copied into it so they can be reopened). The
project keeps the indexed VCF, the filtered VCF, the annotation tables, the
caches, a small `state.rds` describing what was loaded, the settings and
the stage reached, and a readable `state.json` with those settings and every
input value at the time, for provenance and methods sections. Every figure
or table you download is also kept in the project (`plots/`, `downloads/`)
and listed in the gallery of the project panel. **Restore an existing project** lists the projects on this
instance; restoring one brings back the loaded VCF, the filters and the
annotation results in seconds, without recomputing. A restore is refused when
the source VCF changed since the project was made. Public instances have no
private space and therefore no projects.

Select the **reference genome**. Only genomes whose BSgenome, TxDb and
organism packages are installed appear in the list. When the dbSNP `SNPlocs`
package for that genome is installed, rsIDs are added automatically.

Human hg38 and hg19 ship with NASQAR2. To work with another species, open
**Install another reference genome**, pick it from the catalog (mouse, rat,
zebrafish, fruit fly, nematode, yeast, chicken, pig, cow, dog, rhesus macaque,
chimpanzee) and press **Install**. The packages are downloaded from
Bioconductor in the background and installed into the writable R library
(`R_LIBS_USER`), so in a private instance they stay available in later
sessions. The server needs internet access; on an HPC compute node without it,
ask an administrator to pre-install the packages into the private R library.

Files that are not bgzip-compressed and tabix-indexed are converted once into
the project directory; the source file is never modified. The VCF must be
coordinate-sorted.

### 2. VCF Summary

Shows the header (INFO and FORMAT definitions, samples) and the number of
records per contig taken from the tabix index. **Record statistics** stream
through the file once, reading only the fixed columns and `INFO/DP`, and report
variant types, FILTER values and QUAL/DP distributions. Files with fewer
records than the chunk size are summarised automatically; larger files start on
request.

### 3. Filter Variants

Record filters:

* **PASS only** keeps records whose FILTER is `PASS` or `.` (no filter applied).
* **Minimum QUAL** and **Minimum INFO/DP** thresholds; records with a missing
  value fail a threshold when one is set.
* **Variant types**: SNV, Indel, MNV, Multiallelic, Other. Multiallelic
  records are split into one row per alternate allele during annotation.
  `Other` covers symbolic alleles such as `<DEL>` or breakends, which cannot be
  annotated and are removed before annotation.

Genomic restriction:

* **Chromosomes**, **regions** (`chr:start-end`, one per line) and **gene
  symbols** (mapped to gene bodies plus a flank). Region and chromosome names
  are converted between `chr1` and `1` styles automatically.

Large files are read in tiles or streamed in chunks (`NASQAR_VARIANT_YIELD_SIZE`,
default 100,000 records). Records that pass the filters are not held in
memory: they are written straight to a bgzip-compressed, tabix-indexed VCF in
the project directory, so any number of records can be kept. Restricting
chromosomes, regions or genes still makes a whole-genome run much faster.

For files with at most 50 samples the `GT` genotype field is read so that
per-sample counts can be plotted. Larger cohorts are annotated from the fixed
columns only.

### 4. Annotate

* **locateVariants** places every allele on all overlapping transcripts:
  coding, splice site, promoter, 5' UTR, 3' UTR, intron, intergenic. The
  promoter window is configurable.
* **predictCoding** predicts the consequence of every coding allele:
  synonymous, nonsynonymous, nonsense, frameshift, or not translated, with
  reference and variant codons, amino acids and protein position.
* Gene identifiers from the transcript model (Entrez Gene) are mapped to
  symbols with the organism package.
* **dbSNP rsIDs** are added to single-nucleotide variants whose alternate
  allele matches the dbSNP record, whenever the dbSNP package for the genome
  is installed.

Annotation streams the filtered VCF in chunks (`NASQAR_VARIANT_ANNOTATION_CHUNK`,
default 50,000 records), so memory use depends on the chunk size rather than
on the number of filtered variants; a cohort-sized VCF annotates in bounded
memory, it only takes longer. Two tables are produced and written to the
project directory as CSV while the run proceeds:

* **Per variant and transcript**: one row for every allele–transcript pair.
* **Per variant**: one row per allele keeping the most severe effect, with the
  number of transcripts and all overlapping genes.

The interactive table shows the first 10,000 rows of each
(`NASQAR_VARIANT_TABLE_PREVIEW`); the complete tables are available under
Downloads and in the project directory.

Severity order: frameshift > nonsense > nonsynonymous > splice site > coding
(unpredicted) > not translated > synonymous > promoter > 5' UTR > 3' UTR >
intron > intergenic.

Results are cached in the project directory. The cache key covers the VCF path,
size and modification time, all filter and annotation settings, and the versions
of VariantAnnotation and the genome packages, so re-running the same settings is
instant.

### 5. Summary Plots

Variant types, genomic locations, predicted effects, substitution spectrum with
the transition/transversion ratio, variants per chromosome, QUAL and DP
distributions and, for multi-sample files, non-reference genotypes per sample.
Every figure downloads as PNG (300 DPI), PDF or SVG.

### 6. Downloads

Per-variant and per-transcript CSV tables, the gene list, the filter summary
and the filtered records as a bgzip-compressed VCF. The filtered VCF carries
the columns that were read for annotation (fixed fields, `INFO/DP` and `GT` for
files with at most 50 samples).

### 7. Enrichment Handoff

The genes carrying variants (optionally only coding or splice-site variants)
are written to the NASQAR2 exchange area and opened in **ClusterProfShinyORA**.
In the ORA module select the organism Human and set the Keytype to the
identifier you transferred (SYMBOL or ENTREZID).

### Reference genomes

| Genome | Sequence | Gene model | dbSNP |
|---|---|---|---|
| hg38 / GRCh38 | BSgenome.Hsapiens.UCSC.hg38 | TxDb.Hsapiens.UCSC.hg38.knownGene | SNPlocs.Hsapiens.dbSNP144.GRCh38 |
| hg19 / GRCh37 | BSgenome.Hsapiens.UCSC.hg19 | TxDb.Hsapiens.UCSC.hg19.knownGene | SNPlocs.Hsapiens.dbSNP144.GRCh37 |

The dbSNP packages ship in the NASQAR2 image. If a deployment omits them,
annotation runs without rsIDs and the report says so. On HPC an administrator
can add one to the private R library with
`BiocManager::install("SNPlocs.Hsapiens.dbSNP144.GRCh38", lib = Sys.getenv("R_LIBS_USER"))`.

### Citation

> Obenchain V, Lawrence M, Carey V, Gogarten S, Shannon P, Morgan M.
> VariantAnnotation: a Bioconductor package for exploration and annotation of
> genetic variants. *Bioinformatics* 30(14):2076–2078 (2014).
> https://doi.org/10.1093/bioinformatics/btu168
