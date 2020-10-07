
impute_cooked_dataframe <- function(cdf) {
  print(cdf)
}

get_na_sequences <- function(df) {
  na_seq <- rle(is.na(df[,2]))
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
    print(dset_filename)
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
    ss <- data.frame(initial_date, final_date, length_in_days, total_na)
    dset_summary <- dplyr::bind_rows(dset_summary, ss)
  }
  return(dset_summary)
}

# ONLY DATES, NOT NA
summarize_datasets_in_folder_SIMPLE <- function(folder_path) {
  # Get list of filenames in dataset folder
  dset_filenames <- list.files(folder_path)
  dset_summary <- NULL
  # Analysis loop
  for (dset_filename in dset_filenames) {
    # Load raw dataframe from dataset
    print(dset_filename)
    file_path <- paste(folder_path, dset_filename, sep="")
    rdf <- get_raw_dataframe_from_dataset(file_path)
    # cdf <- cook_raw_dataframe(rdf, "first", "last", "lcl")
    # Get initial date
    initial_date <- rdf[1,1]
    # Get final date
    final_date <- tail(rdf, n=1)[[1]]
    # Length in days
    length_in_days <- as.numeric(final_date - initial_date)
    # # Sequence of NA
    # na_seq <- get_na_sequences(cdf$df)
    # # Number of NA
    # total_na <- sum(na_seq$lenghts)
    # Summary
    ss <- data.frame(initial_date, final_date, length_in_days)
    dset_summary <- dplyr::bind_rows(dset_summary, ss)
  }
  return(dset_summary)
}

folder <- "G:/Mi unidad/WHY/Datasets/lcl/"
dset_summary <- summarize_datasets_in_folder_SIMPLE(folder)

# path <- "G:/Mi unidad/WHY/Datasets/lcl/MAC000002.csv"
# rdf <- get_raw_dataframe_from_dataset(path)
# cdf <- cook_raw_dataframe(rdf, "first", "last", "lcl")
# impute_cooked_dataframe(cdf)
