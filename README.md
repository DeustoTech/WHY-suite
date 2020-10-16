# why-T2.1
Welcome! Here you'll find the R scripts to carry out the **task T2.1** of the **WHY** H2020 project.

The code in this repository is currently under development, so the files and their content may change very quickly. 🏃‍♂️💨

The `why-T2.1-pkg` folder contains a package with the most useful functions of the project. All of them are documented in the `man` folder via Roxygen2.

<!--
## Relevant files
### why-source.R
This file must be sourced by the rest of the scripts.

It contains useful links to libraries, global variables and function definitions.

### get-features.R
Extracts features from all time series in a dataset.

Given a dataset folder, it makes a file-by-file analysis. The analysis consists in extracting a time series between the start and end dates provided by the user. If this interval does not exist in the file or it contains more than the desirable NaNs, that time series is discarded. If the time series is OK, its features are extracted.

It generates 3 outputs:
* `feats.csv`: the features of the analyzed files;
* `data_info.csv`: info about the analyzed files;
* `rejected.csv`: info about the rejected files.

### compute-pca.R
Given a CSV file of features (such as `feats.csv`), it computes PCA.

The output is a plot of the time series in the chosen principal components. It is also possible to identify the plotted elements.

### compute-kmeans.R
Given a CSV file of features (such as `feats.csv`), it computes k-means.

The output can be a plot of either the elbow curve or the resulting clusters.
*  For the elbow curve, the maximum number of clusters to analyze must be provided.
*  For the resulting clusters, the number of clusters must be provided. The output is a plot of points, representing the time series, clustered by colors and represented in 2D via the first two principal components.

### compute-combined-pca-kmeans.R
Given a CSV file of features (such as `feats.csv`), it computes PCA. Then, k-means is computed from the resulting PCA scores ([more info](https://365datascience.com/pca-k-means/)).

The number of principal components of the scores from which k-means is computed can be selected by the user to reduce the noise of the model. The output is a plot of points, representing the time series, clustered by colors and represented in 2D via the first two principal components (of the new reduced set).

The daily accumulated values of energy consumption over the month can be represented for each cluster.

### feats-to-ts.R
Simple implementation of the [GRATIS method](https://onlinelibrary.wiley.com/doi/abs/10.1002/sam.11461) for generating time series from features.

## Not-so-relevant files
### features-library.R
Create a visual library of features so that they can be easily understood.

### plot-lcl.R
Plot as PDF files the time series analyzed in `get-features.R`.

### ts-vs-msts-comparison.R
Simple check that the function `ts` gives [the same results](https://docs.google.com/spreadsheets/d/1uKM50_3fQVKlFBdOtd9o47aULvEy91RGXDwSKua90qI/edit?usp=sharing) as the function `msts`, with one seasonal period being equal to the frequency of `ts`.
-->
