# why-T2.1
Programs to carry out task T2.1 of the WHY project.

## why-source.R
This is the main file that must be sourced by the rest of the scripts.
It contains useful global variables and function definitions.

## get-features.R
This script extracts features from all time series in a dataset.

Given a dataset folder, it makes a file-by-file analysis. The analysis consists in extracting a time series between the start date and the end date provided by the user. If this interval does not exist in the file or it contains more than the desirable NaNs, the time series is discarded. If the time series is OK, it extracts its features.

It generates 3 outputs:
* `feats.csv`: the features of the analyzed files;
* `data_info.csv`: info about the analyzed files;
* `rejected.csv`: info about the rejected files.

