<style>
.guide { max-width: 1040px; color: #333; font-size: 15px; line-height: 1.6; }
.guide h3 { font-size: 20px; font-weight: 600; margin: 34px 0 6px; }
.guide h3:first-child { margin-top: 6px; }
.guide .guide-lead { font-size: 16px; color: #444; margin-bottom: 4px; }
.guide .guide-sub { color: #6b7280; margin: 0 0 16px; }
.guide .guide-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(220px, 1fr)); gap: 14px; }
.guide .guide-grid.guide-plots { grid-template-columns: repeat(auto-fit, minmax(170px, 1fr)); }
.guide .guide-card { border: 1px solid #e3e6ea; border-radius: 6px; background: #fff; padding: 14px 16px; }
.guide .guide-card h4 { font-size: 15px; font-weight: 600; margin: 0 0 4px; }
.guide .guide-card p { margin: 0; color: #555; font-size: 14px; }
.guide .guide-step { display: inline-block; width: 24px; height: 24px; line-height: 24px; border-radius: 50%; background: #3c8dbc; color: #fff; text-align: center; font-size: 13px; font-weight: 600; margin-right: 8px; }
.guide .guide-figure { border: 1px solid #e3e6ea; border-radius: 6px; background: #fff; padding: 10px; margin: 0; }
.guide .guide-figure img { width: 100%; height: 150px; object-fit: contain; display: block; }
.guide .guide-figure.guide-table img { height: auto; }
.guide .guide-figure figcaption { font-size: 13px; color: #555; margin-top: 8px; }
.guide .guide-figure figcaption strong { color: #333; }
.guide a.pop { cursor: zoom-in; display: block; }
.guide ul { padding-left: 20px; margin-bottom: 14px; }
.guide li { margin-bottom: 5px; }
.guide .guide-note { border-left: 3px solid #3c8dbc; background: #f4f8fb; padding: 10px 14px; margin: 14px 0; font-size: 14px; }
.guide code { background: #f1f3f5; color: #333; padding: 1px 5px; border-radius: 3px; }
</style>
<div class="guide">

<h3>From amplicon reads to sequence variants and taxonomy</h3>
<p class="guide-lead">Load the FASTQ files of an amplicon sequencing run, such as 16S rRNA, and get the exact sequence variants in each sample, their taxonomy, and diversity plots. No R programming is needed.</p>
<p class="guide-sub">The analysis is run by <a href="https://benjjneb.github.io/dada2/" target="_blank">DADA2</a>, which corrects sequencing errors to tell true biological sequences apart instead of grouping reads into clusters.</p>

<h3>How it works</h3>
<p class="guide-sub">Each step appears in the sidebar when the one before it has finished.</p>
<div class="guide-grid">
<div class="guide-card"><h4><span class="guide-step">1</span>Input data</h4><p>Upload FASTQ files or start from the example data, say whether the run is single-end or paired-end, and press Run QC.</p></div>
<div class="guide-card"><h4><span class="guide-step">2</span>Quality profile</h4><p>Look at the read quality of each sample to decide where the reads should be cut.</p></div>
<div class="guide-card"><h4><span class="guide-step">3</span>Run DADA2</h4><p>Set the truncation lengths and error limits. The app then filters, learns the error rates, denoises, merges read pairs and removes chimeras.</p></div>
<div class="guide-card"><h4><span class="guide-step">4</span>Taxonomy</h4><p>Assign each sequence variant to a taxonomy with the SILVA reference, or with a reference of your own.</p></div>
<div class="guide-card"><h4><span class="guide-step">5</span>Diversity and abundance</h4><p>Add a sample metadata file to compare diversity and composition between groups of samples.</p></div>
</div>

<h3>What you get</h3>
<p class="guide-sub">Figures from the example data. Click a figure to enlarge it.</p>
<div class="guide-grid guide-plots">
<figure class="guide-figure"><a class="pop"><img src="www/guide-quality.png" alt="Read quality profile"/></a><figcaption><strong>Quality profile</strong><br/>Base quality along the read, for the forward and the reverse reads of a sample.</figcaption></figure>
<figure class="guide-figure"><a class="pop"><img src="www/guide-errors.png" alt="Error model"/></a><figcaption><strong>Error rates</strong><br/>How well the learned error model fits the observed errors.</figcaption></figure>
<figure class="guide-figure"><a class="pop"><img src="www/guide-sequences.png" alt="Sequence distribution"/></a><figcaption><strong>Sequence abundance</strong><br/>How the reads of the selected sequence variants are spread over the samples.</figcaption></figure>
<figure class="guide-figure"><a class="pop"><img src="www/guide-alpha.png" alt="Alpha diversity"/></a><figcaption><strong>Alpha diversity</strong><br/>Shannon and Simpson diversity within each sample, by group.</figcaption></figure>
<figure class="guide-figure"><a class="pop"><img src="www/guide-ordination.png" alt="Ordination"/></a><figcaption><strong>Ordination</strong><br/>Samples placed by how different their communities are (Bray-Curtis, NMDS).</figcaption></figure>
<figure class="guide-figure"><a class="pop"><img src="www/guide-composition.png" alt="Taxonomic composition"/></a><figcaption><strong>Composition</strong><br/>Relative abundance at the taxonomy rank you choose.</figcaption></figure>
</div>
<p class="guide-sub" style="margin-top:14px;">Tables to download: reads kept at every step, the sequence table, the taxonomy table (CSV and FASTA) and the denoised and chimera-free sequences. Every figure downloads as PNG, PDF or SVG.</p>

<h3>Preparing your reads</h3>
<ul>
<li>FASTQ files, plain or gzip-compressed (<code>.fastq</code> or <code>.fastq.gz</code>), one file per sample and read direction.</li>
<li>For a paired-end run, the forward and the reverse file of a sample share the same name and differ only in their ending. Enter the two endings under <em>Read layout</em>; the default is <code>_R1_001.fastq</code> and <code>_R2_001.fastq</code>.</li>
<li><strong>Every forward file needs its reverse partner.</strong></li>
<li>Single-end runs need only the forward file ending.</li>
</ul>

<h3>Choosing the settings</h3>
<ul>
<li><strong>Truncation length.</strong> Reads are cut at this position and shorter reads are dropped. Pick the point where quality falls off in the quality profile. The defaults, 240 for forward and 160 for reverse reads, suit the example data.</li>
<li><strong>Maximum expected errors.</strong> Reads with more expected errors than this are dropped. The default is 2.</li>
<li><strong>Paired reads must still overlap after truncation</strong>, or they cannot be merged. The Track Reads tab shows how many reads survive each step, which is the quickest way to spot settings that are too strict.</li>
</ul>

<h3>Assigning taxonomy</h3>
<p>The SILVA v138.1 reference is provided. You can upload your own reference instead, as long as it is formatted for DADA2 (see <a href="https://benjjneb.github.io/dada2/training.html" target="_blank">how to prepare a reference database</a>). Species-level assignment is optional. Select rows of the taxonomy table to plot how those sequence variants are spread over the samples.</p>

<h3>Diversity and abundance</h3>
<ul>
<li>Upload a sample metadata file in QIIME format: tab-separated, with the sample names in a first column called <code>#SampleID</code> and one column per variable.</li>
<li><strong>The sample names must match the DADA2 sample names exactly</strong>, which are the file names without their read ending.</li>
<li>Choose the variable that groups the samples, an optional colour, and the taxonomy rank.</li>
<li>The composition plot shows <strong>the 20 most abundant sequence variants</strong>.</li>
</ul>

<h3>On a private instance</h3>
<p>NASQAR2 can also run as a private instance on your own server or cluster, where your data stays on your own machines. Everything above works the same way there. In addition:</p>
<ul>
<li><strong>Your work is saved automatically</strong> under the name in the <strong>Project name</strong> box on the Input Data tab.</li>
<li><strong>Large runs need no upload.</strong> Choose <strong>FASTQ folder in the private space</strong> and press <strong>Browse folder</strong> to use reads that are already on the server.</li>
<li><strong>You can continue later.</strong> Choose <strong>Reopen a saved project</strong> on the Input Data tab, pick your project and press <strong>Open project</strong>. The results come back without running the analysis again. <strong>Only projects started from a FASTQ folder in the private space can be reopened.</strong></li>
<li><strong>Your figures and tables are kept together</strong> on the <strong>Project files</strong> page. <strong>Save all to project</strong> stores all of them in one click.</li>
</ul>
<p>These options do not appear on the public site.</p>

<h3>Citation</h3>
</div>
<script>
$(document).on('click', '.guide a.pop', function() {
  $('.imagepreview').attr('src', $(this).find('img').attr('src'));
  $('#imagemodal').modal('show');
});
</script>

<div class="modal fade" id="imagemodal" tabindex="-1" role="dialog" aria-hidden="true">
<div class="modal-dialog modal-lg">
<div class="modal-content">
<div class="modal-body">
<button type="button" class="close" data-dismiss="modal"><span aria-hidden="true">&times;</span><span class="sr-only">Close</span></button>
<img src="" class="imagepreview" style="width: 100%;" >
</div>
</div>
</div>
</div>

If you use this App for your analysis, please use the following citations:

1. Callahan, B., McMurdie, P., Rosen, M. et al. DADA2: High-resolution sample inference from Illumina amplicon data. Nat Methods 13, 581–583 (2016). https://doi.org/10.1038/nmeth.3869
2. NASQAR 2 :(Pending publication)
