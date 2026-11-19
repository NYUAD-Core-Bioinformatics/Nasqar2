<style>
.guide { max-width: 1040px; color: #333; font-size: 15px; line-height: 1.6; }
.guide h3 { font-size: 20px; font-weight: 600; margin: 30px 0 6px; }
.guide h3:first-child { margin-top: 6px; }
.guide .guide-lead { font-size: 16px; color: #444; margin-bottom: 4px; }
.guide .guide-sub { color: #6b7280; margin: 0 0 16px; }
.guide .guide-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 14px; }
.guide .guide-card { border: 1px solid #e3e6ea; border-radius: 6px; background: #fff; padding: 14px 16px; }
.guide .guide-card h4 { font-size: 15px; font-weight: 600; margin: 0 0 4px; }
.guide .guide-card p { margin: 0; color: #555; font-size: 14px; }
.guide .guide-step { display: inline-block; width: 24px; height: 24px; line-height: 24px; border-radius: 50%; background: #2fa4e7; color: #fff; text-align: center; font-size: 13px; font-weight: 600; margin-right: 8px; }
.guide ul { padding-left: 20px; margin-bottom: 14px; }
.guide li { margin-bottom: 5px; }
.guide code { background: #f1f3f5; color: #333; padding: 1px 5px; border-radius: 3px; }
</style>
<div class="guide">

<h3>Merge per-sample count files into one count matrix</h3>
<p class="guide-lead">Turn the count files of your samples into a single table that DESeq2Shiny and other analysis apps can read. No R programming is needed.</p>
<p class="guide-sub">Works with raw gene counts, such as the output of HTSeq or featureCounts, and with Kallisto estimated counts.</p>

<h3>How it works</h3>
<div class="guide-grid">
<div class="guide-card"><h4><span class="guide-step">1</span>Input data</h4><p>Choose raw counts or Kallisto, then upload one file per sample, or two or more count matrices to join.</p></div>
<div class="guide-card"><h4><span class="guide-step">2</span>Configure</h4><p>Pick the gene and count columns. For Kallisto, choose the transcript-to-gene mapping. Optionally turn Ensembl IDs into gene names.</p></div>
<div class="guide-card"><h4><span class="guide-step">3</span>Merge</h4><p>Press Merge files. The merged table appears on the Output tab, where you can edit the sample names and download it.</p></div>
<div class="guide-card"><h4><span class="guide-step">4</span>Continue</h4><p>Open DESeq2Shiny or START with the merged counts already loaded.</p></div>
</div>

<h3>Good to know</h3>
<ul>
<li><strong>The gene IDs must match across all files</strong>: they are the first column of every raw count file.</li>
<li><strong>Raw count files are read without a header row</strong> unless you tick <em>Files have header row</em>.</li>
<li><strong>File names become the sample names</strong> of the merged table. They can be edited after merging.</li>
<li><strong>Kallisto needs a transcript-to-gene mapping</strong>: choose a pre-built one for a supported genome, or upload a two-column CSV with <code>TX_NAME</code> and <code>GENE_ID</code>.</li>
<li>Gene names can be looked up from Ensembl gene IDs with a pre-built mapping, or with your own CSV of <code>GENE_ID</code> and <code>GENE_NAME</code>. They can replace the IDs or be added as an extra column.</li>
<li>If your genome is not among the pre-built ones and you have its <code>.gtf</code> file, the Configure panel links to R scripts that generate both mappings.</li>
</ul>

</div>
