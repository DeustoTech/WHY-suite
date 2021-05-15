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
1. ``clValid2-analysis_v03P.R``: Cluster data using clValid2 (v04P incorporates data scaling)
2. -- validation (NO validation report)
3. ``clValid2-heatmaps_v02P.R``: Generate heatmap matrices and plots.
4. ``clValid2-graphs_v02.R``