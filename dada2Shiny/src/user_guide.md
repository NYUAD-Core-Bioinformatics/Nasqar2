# DADA2 Pipeline User Guide

## Introduction

This guide provides a basic overview of using DADA2 within an R Shiny application for analyzing microbiome sequencing data. DADA2 is a tool for processing and denoising amplicon sequencing data, such as 16S rRNA sequences, ensuring high-resolution identification of sequence variants.

The original paper describing DADA2 can be found in the citation at the bottom of this page.

---

### The RShiny DADA2 app will perform the following analysis steps:

1. **Quality Profiling:**
   - Assess the quality of the sequencing reads to determine where to truncate reads to remove poor-quality regions.
   
2. **Filtering and Trimming:**
   - Filter and trim reads to remove low-quality bases and noisy reads, ensuring that only high-quality data is processed.

3. **Denoising:**
   - Use the DADA2 algorithm to correct errors and identify true biological sequences.

4. **Merging Paired Reads:**
   - Merge forward and reverse reads into full sequences. Only sequences that overlap and pass error checks are merged.


5. **Chimera Removal:**
   - Detect and remove chimeras to prevent false diversity.

6. **Assigning Taxonomy (Optional):**
   - Assign taxonomic categories (e.g., species, genus) by comparing the sequences to a reference database.


---

The DADA2 pipeline provides a streamlined method for analyzing amplicon sequencing data in a quick and efficient manaer. By using this pipeline, users can obtain a high-resolution view of their microbial communities.

For detailed instructions and additional resources, please refer to the [DADA2 Documentation](https://benjjneb.github.io/dada2/).

## Citation:

If you use this App for your analysis, please use the following citations:

1. Callahan, B., McMurdie, P., Rosen, M. et al. DADA2: High-resolution sample inference from Illumina amplicon data. Nat Methods 13, 581–583 (2016). https://doi.org/10.1038/nmeth.3869
2. NASQAR 2 :(Pending publication)

