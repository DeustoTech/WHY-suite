# why-T2.1
Programs to carry out task T2.1 of the WHY project.

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

The output is a plot of the time series in the chosen principal components.

### compute-kmeans.R
Given a CSV file of features (such as `feats.csv`), it computes k-means.

The output can be a plot of either the elbow curve or the resulting clusters.
*  For the elbow curve, the maximum number of clusters to analyze must be provided.
*  For the resulting clusters, the number of clusters must be provided. The output is a plot of points, representing the time series, clustered by colors and represented in 2D via the first two principal components.

### compute-combined-pca-kmeans.R
Given a CSV file of features (such as `feats.csv`), it computes PCA. Then, k-means is computed from the resulting PCA scores.

The number of principal components of the scores from which k-means is computed can be selected by the user to reduce the noise of the model ([see link](https://365datascience.com/pca-k-means/). The output is a plot of points, representing the time series, clustered by colors and represented in 2D via the first two principal components (of the new reduced set).

The accumulated values of energy consumption over the month can be represented for each cluster.

## Not-so-relevant files
