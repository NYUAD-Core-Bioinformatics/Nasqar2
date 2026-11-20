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

<h3>Quality control of ATAC-seq alignments</h3>
<p class="guide-lead">Load the aligned reads of an ATAC-seq experiment and check whether the libraries are good: fragment sizes, library complexity, signal at transcription start sites and nucleosome patterns. No R programming is needed.</p>
<p class="guide-sub">The analysis is run by the Bioconductor <a href="https://bioconductor.org/packages/release/bioc/html/ATACseqQC.html" target="_blank">ATACseqQC</a> package.</p>

<h3>How it works</h3>
<p class="guide-sub">Each step appears in the sidebar when the one before it has finished.</p>
<div class="guide-grid">
<div class="guide-card"><h4><span class="guide-step">1</span>Input data</h4><p>Upload BAM files with their index files, or start from the example data.</p></div>
<div class="guide-card"><h4><span class="guide-step">2</span>Reference genome</h4><p>Choose the genome the reads were aligned to and press Initialize QC.</p></div>
<div class="guide-card"><h4><span class="guide-step">3</span>First look</h4><p>The Heatmap, Library Complexity and Fragment size tabs are ready straight away.</p></div>
<div class="guide-card"><h4><span class="guide-step">4</span>Core QC</h4><p>Pick a sample and a chromosome and run the core QC for the scores at transcription start sites.</p></div>
<div class="guide-card"><h4><span class="guide-step">5</span>Go deeper</h4><p>With the core QC done, run the nucleosome profiles and the transcription-factor footprinting.</p></div>
</div>

<h3>What you get</h3>
<p class="guide-sub">Figures from the example data. Click a figure to enlarge it.</p>
<div class="guide-grid guide-plots">
<figure class="guide-figure"><a class="pop"><img src="www/guide-fragments.png" alt="Fragment size distribution"/></a><figcaption><strong>Fragment sizes</strong><br/>A good library shows nucleosome-free fragments and a ladder of one, two and more nucleosomes.</figcaption></figure>
<figure class="guide-figure"><a class="pop"><img src="www/guide-complexity.png" alt="Library complexity"/></a><figcaption><strong>Library complexity</strong><br/>How many distinct fragments the library holds, and how many more deeper sequencing would add.</figcaption></figure>
<figure class="guide-figure"><a class="pop"><img src="www/guide-heatmap.png" alt="Sample correlation heatmap"/></a><figcaption><strong>Sample heatmap</strong><br/>How similar the samples are to each other on the chromosome you choose.</figcaption></figure>
<figure class="guide-figure"><a class="pop"><img src="www/guide-tss.png" alt="TSS enrichment"/></a><figcaption><strong>TSS enrichment</strong><br/>Cut-site signal piled up around transcription start sites.</figcaption></figure>
<figure class="guide-figure"><a class="pop"><img src="www/guide-tss-heatmap.png" alt="TSS-aligned heatmap"/></a><figcaption><strong>TSS-aligned heatmap</strong><br/>Signal of each fragment class across transcription start sites.</figcaption></figure>
<figure class="guide-figure"><a class="pop"><img src="www/guide-nucleosome.png" alt="Nucleosome signal"/></a><figcaption><strong>Nucleosome signal</strong><br/>Nucleosome-free and nucleosome-bound coverage around transcription start sites.</figcaption></figure>
</div>
<p class="guide-sub" style="margin-top:14px;">The Core QC tab also gives the promoter to transcript body score and the nucleosome-free region score. Every figure downloads as PNG, PDF or SVG.</p>

<h3>Preparing your files</h3>
<ul>
<li>Aligned reads as BAM files, coordinate-sorted, each with its BAI index.</li>
<li><strong>Every BAM file needs an index file with the same name</strong>, for example <code>sample1.bam</code> and <code>sample1.bai</code>. Upload both together.</li>
<li><strong>The reference genomes offered are the ones installed on this instance.</strong> The reads must have been aligned to one of them.</li>
<li><strong>The sample heatmap needs more than one BAM file.</strong></li>
</ul>

<h3>The analyses</h3>
<ul>
<li><strong>Heatmap, Library Complexity, Fragment size.</strong> Available as soon as the QC is initialized. Choose the sample, or the chromosome for the heatmap, at the top of each tab.</li>
<li><strong>Core QC.</strong> Choose a sample and a chromosome and press <em>Run core QC</em>. You get the promoter to transcript body score, the nucleosome-free region score and the TSS enrichment score.</li>
<li><strong>Nucleosome Profiles.</strong> Press <em>Run nucleosome analysis</em> for the cumulative tag allocation, the TSS-aligned heatmap and the nucleosome-free and nucleosome-bound distributions. The reads split by fragment class can be downloaded as BAM files.</li>
<li><strong>TF Footprinting.</strong> Enter a motif name, CTCF by default, and press <em>Run footprint analysis</em> for the footprint of that factor and a heatmap of its binding sites.</li>
<li><strong>Nucleosome Profiles and TF Footprinting use the sample and chromosome of the core QC</strong>, so run the core QC first.</li>
</ul>

<h3>On a private instance</h3>
<p>NASQAR2 can also run as a private instance on your own server or cluster, where your data stays on your own machines. Everything above works the same way there. In addition:</p>
<ul>
<li><strong>BAM files need no upload.</strong> Choose <strong>BAM folder in the private space</strong>, press <strong>Browse folder</strong> to pick a folder that is already on the server, then <strong>Load folder</strong>.</li>
<li><strong>Your work is saved automatically</strong> in the project chosen at the top of the input step, for runs that start from a BAM folder in the private space.</li>
<li><strong>Every analysis belongs to a project.</strong> The <strong>Project</strong> question at the top of the input step asks whether to <strong>create a new project</strong> or <strong>open an existing one</strong>. All applications of one study save into the same project, each in its own folder.</li>
<li><strong>You can continue later.</strong> Choose <strong>Open an existing project</strong>, pick the project and press <strong>Continue</strong>. The folder and the reference genome come back, and results already computed are not computed again.</li>
<li><strong>Your figures are kept together</strong> on the <strong>Project files</strong> page. <strong>Save all to project</strong> stores all of them in one click.</li>
</ul>
<p>These options do not appear on the public site.</p>

<h3>Citation</h3>
<p>Ou J, Liu H, Yu J, et al. ATACseqQC: a Bioconductor package for post-alignment quality assessment of ATAC-seq data. <em>BMC Genomics</em> 19, 169 (2018). <a href="https://doi.org/10.1186/s12864-018-4559-3" target="_blank">https://doi.org/10.1186/s12864-018-4559-3</a></p>
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
