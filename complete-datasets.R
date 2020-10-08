get_na_sequences <- function(cdf) {
  na_seq <- rle(is.na(cdf[,2]))
  na_seq <- list(lenghts = na_seq$lengths[na_seq$values == T],
                 indices = cumsum(c(1, na_seq$lengths))[na_seq$values == T])
  return(na_seq)
}

summarize_datasets_in_folder <- function(folder_path) {
  # Get list of filenames in dataset folder
  dset_filenames <- list.files(folder_path)
  dset_summary <- NULL
  # Analysis loop
  for (dset_filename in dset_filenames) {
    # Load raw dataframe from dataset
    # print(dset_filename)
    file_path <- paste(folder_path, dset_filename, sep="")
    rdf <- get_raw_dataframe_from_dataset(file_path)
    cdf <- cook_raw_dataframe(rdf, "first", "last", "lcl")
    # Get initial date
    initial_date <- cdf$df[1,1]
    # Get final date
    final_date <- tail(cdf$df, n=1)[[1]]
    # Length in days
    length_in_days <- as.numeric(final_date - initial_date)
    # Sequence of NA
    na_seq <- get_na_sequences(cdf$df)
    # Number of NA
    total_na <- sum(na_seq$lenghts)
    # Summary
    ss <- data.frame(dset_filename, initial_date, final_date, length_in_days,
                     total_na)
    print(ss)
    dset_summary <- dplyr::bind_rows(dset_summary, ss)
  }
  return(dset_summary)
}

impute_cooked_dataframe <- function(cdf) {
  # Interval to plot
  interv <- 1300:2100
  # Sequences of NA before imputation
  na_seqs <- get_na_sequences(cdf$df)
  print(na_seqs)
  plot_dataframe(cdf$df[interv,])
  # Time series pending imputation
  not_imputed <- ts(data=cdf$df[,2], frequency=336)
  # Imputed time series
  imputed <- imputeTS::na_seasplit(not_imputed, algorithm="mean")
  # Imputed dataframe
  imp_df <- data.frame(times=cdf$df[,1], 
                       values=imputed, 
                       imputed=as.integer(is.na(not_imputed)))
  # Sequences of NA after imputation
  na_seqs_2 <- get_na_sequences(imp_df) 
  print(na_seqs_2)
  plot_dataframe(imp_df[interv,])
}

# folder <- "G:/Mi unidad/WHY/Datasets/lcl/"
# dset_summary <- summarize_datasets_in_folder(folder)

path <- "G:/Mi unidad/WHY/Datasets/lcl/MAC001908.csv"
rdf <- get_raw_dataframe_from_dataset(path)
cdf <- cook_raw_dataframe(rdf, "first", "last", "lcl")
impute_cooked_dataframe(cdf)
