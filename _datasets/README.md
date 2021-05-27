# What to do with raw *.csv files
Raw files may contain gaps or be shorter than 800 days. They need to be converted to a format that the feature extraction file can understand. This format is known in this package as **"ext" file** ("ext" for "extended"). "ext" files are lists containing a data frame with the time series and metadata. The extension of "ext" files is "RData", unlike "csv" of raw files. The time series within the data frame of the "ext" file can be printed using the function ``whyT2.1::plot_dataframe()``.

* ``raw2ext.R``: convert from raw to ext. If the raw files do not come from one of the existing datasets, it is necessary to provide extra info to the code (file ``data-load-and-processing.R`` in the ``R`` folder).
* ``DST_correction.R``: in case the daylight saving time of the time series needs correction.
* ``get_features.R``: compute features. The process is parallelized, so several files will be created in the output folder.