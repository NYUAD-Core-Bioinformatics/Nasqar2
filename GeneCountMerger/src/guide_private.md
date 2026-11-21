<div class="guide">
<h3>On a private instance</h3>
<p>NASQAR2 can also run as a private instance on your own server or cluster, where your data stays on your own machines. Everything above works the same way there. In addition:</p>
<ul>
<li><strong>Raw count files need no upload.</strong> Choose <strong>Files in the private space</strong> and press <strong>Browse</strong> to pick files that are already on the server. Kallisto files are always uploaded.</li>
<li><strong>The merged table is saved automatically</strong> in the project chosen at the top of the input step, where DESeq2Shiny can read it directly.</li>
<li><strong>Every analysis belongs to a project.</strong> The <strong>Project</strong> question at the top of the input step asks whether to <strong>create a new project</strong> or <strong>open an existing one</strong>. All applications of one study save into the same project, each in its own folder.</li>
<li><strong>Uploaded count files are kept in the project</strong>, in its <code>inputs</code> folder.</li>
<li><strong>You can continue later.</strong> Choose <strong>Open an existing project</strong>, pick the project and press <strong>Continue</strong>. The merged table comes back, the count files are filled in and the merge settings are restored. You can then change a setting, such as adding gene names, and press <strong>Merge files</strong> again without uploading anything. Kallisto files are not kept.</li>
<li><strong>Your downloads are kept together</strong> on the <strong>Project files</strong> page.</li>
</ul>
<p>These options do not appear on the public site.</p>
</div>
