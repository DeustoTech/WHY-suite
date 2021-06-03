# What to do with raw *.csv files

### Some considerations

Raw *.csv files may contain gaps or be shorter than 800 days. They need to be converted to a format compatible with the feature extraction file. In the context of this package, this format is the _extended_ *.RData file (or _ext_ file). _ext_ files are big lists containing (1) a data frame with the time series and (2) metadata. The time series within the data frame of the _ext_ file can be plotted using the function ``whyT2.1::plot_dataframe(edf$df)``.

### Files
* ``raw2ext.R``: convert from raw to ext. If the raw files do not come from one of the existing datasets, it is necessary to provide extra info to the code: check all functions in ``R/dataset-metadata.R``. After this, some **optional** processing can be performed:
	* ``correct-dataset-metadata-field.R``: batch-corrects fields in EXT files of a dataset in case it's wrong (e.g. ``edf$dset_key: "go2"`` to ``edf$dset_key: "meg"`` in dataset ``meg``).
	* ``DST_correction.R``: in case the daylight saving time of the time series needs correction.
	* Inside the 3-letter dataset folders there may be more extra post-processing files. For example, dataset ``nee`` requires timezone correction and the specific file to do it is there. Please check.
* ``get_features_v2.R``: compute features.
* ``perl get_c22.pl`` (command line): compute Catch-22 features. They have to be computed piecewise due to [its known memory leak bug](https://github.com/chlubba/catch22/issues/4). ``get_c22_features.R`` is required.
* ``put_feats_together.R``: join conventional and Catch-22 features.
* ``get_metadata_v4.R``: extract metadata from EXT files.