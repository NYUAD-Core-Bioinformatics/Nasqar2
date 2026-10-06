# Variant Annotation (NASQAR2 module)

Shiny application that filters and annotates variants from a VCF with the
Bioconductor VariantAnnotation package (locateVariants, predictCoding), maps
genes with org.Hs.eg.db, optionally adds dbSNP rsIDs, draws summary figures and
hands the affected genes to ClusterProfShinyORA.

## Layout

- `src/ui.R`, `src/server.R`, `src/global.R`: application entry points.
- `src/analysis-helpers.R`: pure helpers (VCF preparation, streaming reader,
  filters, annotation tables, plots) shared with `src/test_analysis_helpers.R`.
- `src/ui-tab-*.R`, `src/server-*.R`: one pair per sidebar step.
- `src/user_guide.md`: in-app documentation.
- `environment.yaml`: conda environment `v_variantannotation`.

## Run locally

```bash
conda env create -f environment.yaml
conda run -n v_variantannotation Rscript -e "shiny::runApp('src', port = 4013)"
```

## Tests

```bash
cd src && conda run -n v_variantannotation Rscript test_analysis_helpers.R
```

## Environment variables

| Variable | Purpose |
|---|---|
| `NASQAR_VARIANT_PROJECT_DIR` | Root of persistent project directories (tempdir fallback). |
| `NASQAR_DATA_ROOT` | Read-only private data root that VCF paths must live under. |
| `NASQAR_VARIANT_ANNOTATION_CHUNK` | Filtered records annotated per chunk (default 50000); bounds annotation memory. |
| `NASQAR_VARIANT_TABLE_PREVIEW` | Rows of each result table kept for the interactive preview (default 10000). |
| `NASQAR_VARIANT_YIELD_SIZE` | Records per streamed chunk (default 100000). |
| `NASQAR_EXCHANGE_DIR`, `NASQAR_BASE_PATH`, `NASQAR_PUBLIC_KEY_HEX` | Shared NASQAR2 handoff settings. |
| `R_LIBS_USER` | Writable library that on-the-fly genome installs go into (falls back to the first writable `.libPaths()` entry). |
| `NASQAR_GENOME_DOWNLOAD_TIMEOUT` | Seconds allowed per package download during on-the-fly genome installs (default 7200; BSgenome tarballs reach 1 GB). |

The hg38 BSgenome package is installed from a Bioconductor tarball in the
Dockerfiles because the bioconda stub downloads it at environment creation.
The dbSNP packages (`SNPlocs.Hsapiens.dbSNP144.GRCh38`,
`SNPlocs.Hsapiens.dbSNP144.GRCh37`, about 0.9 GB each) are installed the same
way. rsIDs are added automatically when the package for the selected genome is
present in the R library; otherwise annotation runs without them.
