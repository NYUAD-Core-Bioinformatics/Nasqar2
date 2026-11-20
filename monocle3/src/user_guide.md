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
.guide .guide-figure img { width: 100%; height: 210px; object-fit: contain; display: block; }
.guide .guide-figure.guide-table img { height: auto; }
.guide .guide-figure figcaption { font-size: 13px; color: #555; margin-top: 8px; }
.guide .guide-figure figcaption strong { color: #333; }
.guide a.pop { cursor: zoom-in; display: block; }
.guide ul { padding-left: 20px; margin-bottom: 14px; }
.guide li { margin-bottom: 5px; }
.guide .guide-note { border-left: 3px solid #3c8dbc; background: #f4f8fb; padding: 10px 14px; margin: 14px 0; font-size: 14px; }
.guide code { background: #f1f3f5; color: #333; padding: 1px 5px; border-radius: 3px; }
</style>
<div class="guide" style="padding-top:12px;">

<h3>Order single cells along a trajectory</h3>
<p class="guide-lead">Start from a single-cell data set that has already been clustered, choose where the process begins, and see how the cells line up in pseudotime and which genes change along the way. No R programming is needed.</p>
<p class="guide-sub">The analysis is run by <a href="https://cole-trapnell-lab.github.io/monocle3/" target="_blank">Monocle 3</a> on a Seurat object.</p>

<h3>How it works</h3>
<div class="guide-grid">
<div class="guide-card"><h4><span class="guide-step">1</span>Input data</h4><p>Upload a Seurat object and press Load object.</p></div>
<div class="guide-card"><h4><span class="guide-step">2</span>Trajectory settings</h4><p>Choose the assay, the identity that groups the cells, and a progenitor gene that marks where the trajectory starts. Press Run Monocle3.</p></div>
<div class="guide-card"><h4><span class="guide-step">3</span>Results</h4><p>The Results tab shows the cells by group, the trajectory coloured by pseudotime, and a heatmap of the genes that change along it.</p></div>
<div class="guide-card"><h4><span class="guide-step">4</span>Download</h4><p>Download any of the three plots, and the results table as a CSV file.</p></div>
</div>

<h3>What you get</h3>
<p class="guide-sub">Figures from an example data set of blood cells. Click a figure to enlarge it.</p>
<div class="guide-grid guide-plots" style="grid-template-columns: repeat(auto-fit, minmax(240px, 1fr));">
<figure class="guide-figure"><a class="pop"><img src="www/guide-dim.png" alt="Cells by identity"/></a><figcaption><strong>Cells by group</strong><br/>The cells on the UMAP, coloured by the identity you chose.</figcaption></figure>
<figure class="guide-figure"><a class="pop"><img src="www/guide-pseudotime.png" alt="Trajectory and pseudotime"/></a><figcaption><strong>Trajectory</strong><br/>The path through the cells, coloured by pseudotime from the starting point.</figcaption></figure>
<figure class="guide-figure"><a class="pop"><img src="www/guide-heatmap.png" alt="Pseudotime heatmap"/></a><figcaption><strong>Genes along the trajectory</strong><br/>Genes significantly associated with pseudotime, with cells ordered along it.</figcaption></figure>
</div>

<h3>Preparing your data</h3>
<ul>
<li>A Seurat object saved as an <code>.rds</code> file, or an <code>.RData</code> file. The Seurat app in NASQAR2 exports one.</li>
<li><strong>An <code>.RData</code> file must hold exactly one Seurat object.</strong></li>
<li><strong>The object must already be analysed</strong>: it needs a UMAP and clusters, since the trajectory is drawn on them.</li>
<li><strong>The progenitor gene must be present in the data.</strong> Use a gene that is high in the cells where the process starts.</li>
<li>Arriving from the Seurat app with its hand-off button loads the object for you, with nothing to upload.</li>
</ul>

<h3>On a private instance</h3>
<p>NASQAR2 can also run as a private instance on your own server or cluster, where your data stays on your own machines. Everything above works the same way there. In addition:</p>
<ul>
<li><strong>Large objects need no upload.</strong> Choose <strong>Seurat object in the private space</strong> and press <strong>Browse</strong> to use a file that is already on the server.</li>
<li><strong>Your work is saved automatically</strong> in the project chosen at the top of the input step, for objects read from the private space.</li>
<li><strong>Every analysis belongs to a project.</strong> The <strong>Project</strong> question at the top of the input step asks whether to <strong>create a new project</strong> or <strong>open an existing one</strong>. All applications of one study save into the same project, each in its own folder.</li>
<li><strong>You can continue later.</strong> Choose <strong>Open an existing project</strong>, pick the project and press <strong>Continue</strong>. The settings and the trajectory come back without running it again. <strong>Only projects made from an object in the private space can be reopened.</strong></li>
<li><strong>Your downloads are kept together</strong> on the <strong>Project files</strong> tab.</li>
</ul>
<p>These options do not appear on the public site.</p>

<h3>Citation</h3>
<p>Cao J, Spielmann M, Qiu X, et al. The single-cell transcriptional landscape of mammalian organogenesis. <em>Nature</em> 566, 496-502 (2019). <a href="https://doi.org/10.1038/s41586-019-0969-x" target="_blank">https://doi.org/10.1038/s41586-019-0969-x</a></p>
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
