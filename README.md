# why-T2.1
Programs to carry out task T2.1 of the WHY project.

## Relevant files
### why-source.R
This file must be sourced by the rest of the scripts.

It contains useful links to libraries, global variables and function definitions.

### get-features.R
This script extracts features from all time series in a dataset.

Given a dataset folder, it makes a file-by-file analysis. The analysis consists in extracting a time series between the start and end dates provided by the user. If this interval does not exist in the file or it contains more than the desirable NaNs, that time series is discarded. If the time series is OK, its features are extracted.

It generates 3 outputs:
* `feats.csv`: the features of the analyzed files;
* `data_info.csv`: info about the analyzed files;
* `rejected.csv`: info about the rejected files.

### compute-pca.R
Computes and plots the PCA from a CSV file of features (such as `feats.csv`).

## Not-so-relevant files