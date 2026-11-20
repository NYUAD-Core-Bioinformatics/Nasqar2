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
.guide .guide-step { display: inline-block; width: 24px; height: 24px; line-height: 24px; border-radius: 50%; background: #446e9b; color: #fff; text-align: center; font-size: 13px; font-weight: 600; margin-right: 8px; }
.guide ul { padding-left: 20px; margin-bottom: 14px; }
.guide li { margin-bottom: 5px; }
.guide code { background: #f1f3f5; color: #333; padding: 1px 5px; border-radius: 3px; }
.guide table.guide-table { border-collapse: collapse; font-size: 13px; background: #fff; margin: 6px 0 4px; }
.guide table.guide-table th, .guide table.guide-table td { border: 1px solid #e3e6ea; padding: 4px 12px; text-align: left; }
.guide table.guide-table th { background: #f5f6f8; font-weight: 600; }
.guide .guide-caption { font-size: 13px; color: #555; margin: 12px 0 2px; }
</style>
<div class="guide">

<h3>Merge per-sample FPKM files into one table</h3>
<p class="guide-lead">Turn the FPKM files of your samples, such as the <code>genes.fpkm_tracking</code> files written by Cufflinks, into a single table with one column per sample. No R programming is needed.</p>
<p class="guide-sub">The values can be converted to TPM on the way, and Ensembl gene IDs can be turned into gene names.</p>

<h3>How it works</h3>
<div class="guide-grid">
<div class="guide-card"><h4><span class="guide-step">1</span>Input data</h4><p>Select the FPKM files of all your samples at once, at least two.</p></div>
<div class="guide-card"><h4><span class="guide-step">2</span>Options</h4><p>Optionally convert FPKM to TPM, and look up gene names from Ensembl IDs.</p></div>
<div class="guide-card"><h4><span class="guide-step">3</span>Merge</h4><p>Press Merge files. The merged table replaces this guide on the page.</p></div>
<div class="guide-card"><h4><span class="guide-step">4</span>Download</h4><p>Download the merged table as a CSV file.</p></div>
</div>

<h3>Preparing your files</h3>
<ul>
<li>One plain-text table per sample, tab or comma separated, with a header row.</li>
<li><strong>The columns <code>gene_id</code> and <code>FPKM</code> must be present.</strong> Any other column is ignored.</li>
<li><strong>Every file must list the same genes.</strong> The files are placed side by side, so a file with a different gene set is refused with an explanation.</li>
<li><strong>File names become the sample names</strong> of the merged table, so short, clear file names give a tidier table.</li>
<li><strong>An upload can be at most 600 MB in total.</strong></li>
</ul>
<p class="guide-caption"><strong>Example input</strong>, two of the files:</p>
<div style="display:flex; gap:24px; flex-wrap:wrap;">
<div><p class="guide-caption">SRX003921.genes.fpkm_tracking</p><table class="guide-table"><thead><tr><th>gene_id</th><th>FPKM</th></tr></thead><tbody><tr><td>ENSG00000001036</td><td>1.300</td></tr><tr><td>ENSG00000001167</td><td>26.900</td></tr><tr><td>ENSG00000001561</td><td>0.100</td></tr><tr><td>...</td><td>...</td></tr></tbody></table></div>
<div><p class="guide-caption">SRX003924.genes.fpkm_tracking</p><table class="guide-table"><thead><tr><th>gene_id</th><th>FPKM</th></tr></thead><tbody><tr><td>ENSG00000001036</td><td>12.500</td></tr><tr><td>ENSG00000001167</td><td>40.400</td></tr><tr><td>ENSG00000001561</td><td>0.000</td></tr><tr><td>...</td><td>...</td></tr></tbody></table></div>
</div>

<h3>What you get</h3>
<p>One row per gene and one column per sample. With gene names looked up, they are added as an extra column or replace the IDs, as you choose.</p>
<p class="guide-caption"><strong>Default</strong></p>
<table class="guide-table"><thead><tr><th>gene.ids</th><th>SRX003921.genes.fpkm_tracking</th><th>SRX003924.genes.fpkm_tracking</th><th>...</th></tr></thead><tbody><tr><td>ENSG00000001036</td><td>1.3</td><td>12.5</td><td>...</td></tr><tr><td>ENSG00000001167</td><td>26.9</td><td>40.4</td><td>...</td></tr><tr><td>...</td><td>...</td><td>...</td><td>...</td></tr></tbody></table>
<p class="guide-caption"><strong>Gene names added</strong></p>
<table class="guide-table"><thead><tr><th>gene.ids</th><th>gene.names</th><th>SRX003921.genes.fpkm_tracking</th><th>SRX003924.genes.fpkm_tracking</th><th>...</th></tr></thead><tbody><tr><td>ENSG00000001036</td><td>FUCA2</td><td>1.3</td><td>12.5</td><td>...</td></tr><tr><td>ENSG00000001167</td><td>NFYA</td><td>26.9</td><td>40.4</td><td>...</td></tr><tr><td>...</td><td>...</td><td>...</td><td>...</td><td>...</td></tr></tbody></table>
<p class="guide-sub" style="margin-top:10px;">Gene names are available for these genome annotations: Homo sapiens GRCh38 (releases 81 and 84), Mus musculus GRCm38.82, Danio rerio GRCz10.84 and Drosophila melanogaster BDGP6.81.</p>

<h3>On a private instance</h3>
<p>NASQAR2 can also run as a private instance on your own server or cluster, where your data stays on your own machines. Everything above works the same way there. In addition:</p>
<ul>
<li><strong>Files need no upload.</strong> Choose <strong>Files in the private space</strong> and press <strong>Browse</strong> to pick files that are already on the server.</li>
<li><strong>The merged table is saved automatically</strong> in the project chosen at the top of the input step.</li>
<li><strong>Every analysis belongs to a project.</strong> The <strong>Project</strong> question at the top of the input step asks whether to <strong>create a new project</strong> or <strong>open an existing one</strong>. All applications of one study save into the same project, each in its own folder.</li>
<li><strong>You can continue later.</strong> Choose <strong>Open an existing project</strong>, pick the project and press <strong>Continue</strong>.</li>
<li><strong>Your downloads are kept together</strong> on the <strong>Project files</strong> page.</li>
</ul>
<p>These options do not appear on the public site.</p>

</div>
