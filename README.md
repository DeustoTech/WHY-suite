## Repository of Task 2.1 of the European H2020 Project _WHY_
Welcome to the repository! Task 2.1 of the [European H2020 Project WHY](https://www.why-h2020.eu/) is devoted to the development of a dictionary of residential electricity consumption profiles based on the load profiles of tens of thousands of users.
Here you can find the files that have been implemented for this purpose.

The scheme followed is as follows:

![Data processing scheme](https://github.com/DeustoTech/why-T2.1/blob/master/.old/img/data_proc_scheme.png?raw=true)

The scripts used to move from one stage to the next are described below:

* From original dataset files to raw files: check the `datasets` folder.

For the rest of stages, check the `suite` folder. The main file there is `suite_v01.R`. It contains a sequence of chained functions to process each stage:

* `raw2imp()`: from raw files to imputed (or processed) files.
* `imp2fea()`: from imputed files to feature extraction.
* `fea2clu()`: from features to cluster analysis and validation.
* `clu2hmp()`: from clusters to heatmap visualizations.
* `hmp2rep()`: from heatmaps to reports.

File `x2y.R` contains real usage examples.