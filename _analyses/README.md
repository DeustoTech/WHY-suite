# How to proceed

## clValid
1. ``clValid-analysis_v03P.R``: Cluster data using clValid (remark: using many processors may collapse memory).
2. ``clValid-validation-plots_v01.R``: Plot validation curves.
3. ``clValid-validation-report_v01.Rmd``: Write validation report.
4. ``clValid-heatmap-matrices_v03.r``: Generate heatmap matrices.
5. ``clValid-heatmap-plots_v01.R``: Plot heatmap matrices.
6. ``clValid-graphs_v01.R``: Plot features and socioeconomical graphs.
7. ``clValid-clustering-report_v01.Rmd``: Write clustering report.

## clValid2
REMARK: ``selectable_variables.R`` must be included in the folder.
1. ``clValid2-analysis_v04P.R``: Cluster data using clValid2.
2. ``clValid2-validations_v01.R``: Plot validation curves.
3. ``clValid2-heatmaps_v02P.R``: Generate heatmap matrices and plots (check if .scale TRUE).
4. ``clValid2-graphs_v04.R``: Plot features, statistics and surveys.
5. ``clValid2-report_v01.Rmd``: Markdown report.

## Cards
See folder ``cards`` for printable heatmaps.

## PCA & t-SNE for metaclustering
See folder ``metaclustering`` for clustering heatmaps.