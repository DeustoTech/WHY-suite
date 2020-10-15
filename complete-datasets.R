library(whyT2.1)

get_na_sequences <- function(cdf) {
  na_seq <- rle(is.na(cdf[,2]))
  na_seq <- list(lenghts = na_seq$lengths[na_seq$values == T],
                 indices = cumsum(c(1, na_seq$lengths))[na_seq$values == T])
  return(na_seq)
}



input_folder <- "G:/Mi unidad/WHY/Datasets/lcl/"
output_folder <- "G:/Mi unidad/WHY/Datasets/lcl-ext/"
extend_datasets(input_folder,output_folder)

################################################################################

# summarize_datasets_in_folder <- function(folder_path) {
#   # Get list of filenames in dataset folder
#   dset_filenames <- list.files(folder_path)
#   dset_summary <- NULL
#   # Analysis loop
#   for (dset_filename in dset_filenames) {
#     # Load raw dataframe from dataset
#     # print(dset_filename)
#     file_path <- paste(folder_path, dset_filename, sep="")
#     rdf <- get_raw_dataframe_from_dataset(file_path)
#     cdf <- cook_raw_dataframe(rdf, "first", "last", "lcl")
#     # Get initial date
#     initial_date <- cdf$df[1,1]
#     # Get final date
#     final_date <- tail(cdf$df, n=1)[[1]]
#     # Length in days
#     length_in_days <- as.numeric(final_date - initial_date)
#     # Sequence of NA
#     na_seq <- get_na_sequences(cdf$df)
#     # Number of NA
#     total_na <- sum(na_seq$lenghts)
#     # Summary
#     ss <- data.frame(dset_filename, initial_date, final_date, length_in_days,
#                      total_na)
#     print(ss)
#     dset_summary <- dplyr::bind_rows(dset_summary, ss)
#   }
#   return(dset_summary)
# }
# 
# impute_cooked_dataframe.OLD <- function(cdf) {
#   # Interval to plot
#   interv <- 1300:2100
#   # Sequences of NA before imputation
#   na_seqs <- get_na_sequences(cdf$df)
#   print(na_seqs)
#   plot_dataframe(cdf$df[interv,])
#   # Time series pending imputation
#   not_imputed <- ts(data=cdf$df[,2], frequency=336)
#   # Imputed time series
#   imputed <- imputeTS::na_seasplit(not_imputed, algorithm="interpolation")
#   # Imputed dataframe
#   imp_df <- data.frame(times=cdf$df[,1], 
#                        values=imputed, 
#                        imputed=as.integer(is.na(not_imputed)))
#   # Sequences of NA after imputation
#   na_seqs_2 <- get_na_sequences(imp_df) 
#   print(na_seqs_2)
#   plot_dataframe(imp_df[interv,])
# }
# 
# impute_cooked_dataframe <- function(cdf) {
#   # Time series pending imputation
#   not_imp_ts <- ts(data=cdf$df[,2], frequency=336) # 1 week
#   # Imputed time series
#   imp_ts <- imputeTS::na_seasplit(not_imp_ts, 
#                                   algorithm = "interpolation",
#                                   maxgap = 16) # 8 hours
#   imp_ts <- imputeTS::na_seasplit(imp_ts, 
#                                   algorithm = "locf")
#   # Imputed dataframe
#   imp_df <- data.frame(times   = cdf$df[,1], 
#                        values  = imp_ts, 
#                        imputed = as.integer(is.na(not_imp_ts)))
#   return(imp_df)
# }
# 
# # folder <- "G:/Mi unidad/WHY/Datasets/lcl/"
# # dset_summary <- summarize_datasets_in_folder(folder)
# 
# path <- "G:/Mi unidad/WHY/Datasets/lcl/MAC001908.csv"
# rdf <- get_raw_dataframe_from_dataset(path)
# cdf <- cook_raw_dataframe(rdf, "first", "last", "lcl")
# imp_df <- impute_cooked_dataframe(cdf)
# plot_dataframe(cdf$df[1000:2000,])
# plot_dataframe(imp_df[1000:2000,])
