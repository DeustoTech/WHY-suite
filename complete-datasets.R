
impute_cooked_dataframe <- function(cdf) {
  print(cdf)
}

summarize_datasets_in_folder <- function(folder_path) {
  # Get list of filenames in dataset folder
  dset_filenames <- list.files(folder_path)
  # Analysis loop
  for (dset_filename in dset_filenames) {
    # Load raw dataframe from dataset
    print(dset_filename)
    file_path <- paste(folder_path, dset_filename, sep="")
    rdf <- get_raw_dataframe_from_dataset(file_path)
    cdf <- cook_raw_dataframe(rdf, "first", "last", "lcl")
    # Get initial date
    initial_date <- print(cdf$df[1,1])
    # Get final date
    final_date <- tail(cdf$df, n=1)[[1]]
    # Length in days
    length_in_days <- as.numeric(final_date - initial_date)
    # Number of NA
    total_na <- sum(is.na(cdf$df[,2]))
    # Sequence of NA
    na_seq <- rle(is.na(cdf$df[,2]))
    na_indices <- cumsum(c(1, na_seq$lengths))
    # Indices of interest
    na_iof <- na_seq$values == TRUE
    na_seq <- list(lenghts = na_seq$lengths[na_iof],
                   indices = na_indices[na_iof])
  }
}

folder <- "G:/Mi unidad/WHY/Datasets/lcl/"
summarize_datasets_in_folder(folder)

# path <- "G:/Mi unidad/WHY/Datasets/lcl/MAC000002.csv"
# rdf <- get_raw_dataframe_from_dataset(path)
# cdf <- cook_raw_dataframe(rdf, "first", "last", "lcl")
# impute_cooked_dataframe(cdf)
