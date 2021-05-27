# What to do with raw *.csv files

## Some considerations

Raw csv files may contain gaps or be shorter than 800 days. They need to be converted to a format compatible with the feature extraction file. In the context of this package, this format is the _extended file_ (or _ext_ file), with the extension "RData". Ext files are lists containing (1) a data frame with the time series and (2) metadata. The time series within the data frame of the "ext" file can be plotted using the function ``whyT2.1::plot_dataframe(edf$df)``.

## Files

* ``raw2ext.R``: convert from raw to ext. If the raw files do not come from one of the existing datasets, it is necessary to provide extra info to the code (file ``data-load-and-processing.R`` in the ``R`` folder). <small>REMARK: This should be externalized at some moment.</small>
* ``DST_correction.R``: in case the daylight saving time of the time series needs correction.
* ``get_features.R``: compute features. The process is parallelized, so several files will be created in the output folder.