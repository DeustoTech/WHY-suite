## Remarks

* ``clValid2`` requires the installation of "tuned" ``clValid2`` package (**[here](https://github.com/quesadagranja/clValid2)**).
* ``selectable_variables.R`` must be included in the folder for versions of ``clValid2-analysis`` from 03 to 05. Not required for versions >= 10. 
* Consider using ``row_conditions_fun`` in ``selectable_variables.R``. Variable ``row_conditions`` appears in:
	* ``clValid2-analysis_vXX.R``
	* ``clValid2-heatmaps_vXX.R``
	* ``clValid2-graphs_vXX.R``

## Summary of files

1. ``clValid2-analysis_v04P.R``: Cluster data using clValid2.
	* ``v03P``: does not scale data.
	* ``v04``: better for heavy amounts of files (non-parallelized version).
	* ``v05``: like ``v04`` but for **variable** number of clusters per dataset.
	* ``v10``: RENEWED VERSION - improved and created for ``go4``.
2. ``clValid2-validations_v01.R``: Plot validation curves.
3. ``clValid2-heatmaps_v02P.R``: Generate heatmap matrices (hmm) and heatmap plots (hmp). Check if ``.scale`` is ``TRUE``.
	* ``v03P``: for **variable** number of clusters per dataset (linked to ``clValid2-analysis_v05.R``).
	* ``v13P``: RENEWED VERSION - improved and created for ``go4`` (linked to ``clValid2-analysis_v10.R``).
4. ``clValid2-graphs_v04.R``: Plot features, statistics and surveys (if skipped, generate a "summary report").
	* ``v05``: like ``v04`` but for **variable** number of clusters per dataset.
5. ``clValid2-report_v01.Rmd``: Markdown report.
6. ``clValid2-summary_report.Rmd``: Summary report of heatmaps (markdown report when feats & stats are skipped).
	* ``v02``: for **variable** number of clusters per dataset (linked to ``clValid2-heatmaps_v03P.R``).
