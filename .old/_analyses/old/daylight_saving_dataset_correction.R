# THIS FILE CORRECTS THE SHIFT GENERATED BY THE DAYLIGHT SAVING TIMES
# IN DATASETS GO2, MEG & LCL

library(foreach)
library(whyT2.1)

new_feats_to_detect_outliers <- function() {

  # User defined variables
  if (.Platform$OS.type == "windows") {
    feats_dir  <- "G:/Mi unidad/WHY/Features/"
    out_dir    <- "G:/Mi unidad/WHY/Features/other_files/"
    dset_dir   <- "G:/Mi unidad/WHY/Datasets/"
  }
  if (.Platform$OS.type == "unix") {
    feats_dir  <- "/home/ubuntu/carlos.quesada/disk/features/"
    out_dir    <- "/home/ubuntu/carlos.quesada/disk/features/heatmaps_v1.11/"
    dset_dir   <- "/home/ubuntu/carlos.quesada/disk/"
  }

  feats_vers <- "v1.11"

  feats_path <- paste0(feats_dir, "feats_", feats_vers, ".csv")

  feats <- data.table::fread(
    file   = feats_path,
    header = TRUE,
    sep    = ",",
    select = c("file", "data_set", "overall_start_date", "overall_end_date")
  )

  # Setup parallel backend to use many processors
  cores <- parallel::detectCores() - 1
  cl <- parallel::makeCluster(cores, outfile = "")
  doParallel::registerDoParallel(cl)

  new_feats <- foreach::foreach(x = 1:dim(feats)[1], .combine = rbind) %do% {
    print(paste(feats$data_set[x], feats$file[x]))
    dset_path <- paste0(dset_dir, feats$data_set[x], "/ext/", feats$file[x], ".RData")
    load(dset_path)

    browser()

    # Get heatmap matrix
    m <- whyT2.1::get_heatmap_matrix(data.frame(dset_path))
    # Plot heatmap matrix
    whyT2.1::plot_heatmap_matrix(
      m           = m,
      format_file = "png",
      file_path   = paste0(out_dir, feats$data_set[x], "_", feats$file[x], ".png"),
      plot_width  = 1200,
      plot_height = 900
    )

    feats_sum <- sum(edf$df$values)
    feats_sum_per_day <- feats_sum / feats$overall_days[x]
    out_df <- data.frame(
      file = feats$file[x],
      data_set = feats$data_set[x],
      sum = feats_sum,
      sum_per_day = feats_sum_per_day
    )
    return(out_df)
  }

  # Stop parallelization
  parallel::stopCluster(cl)

  # Write file
  data.table::fwrite(
    new_feats,
    file       = paste0(out_dir, "new_feats.csv"),
    row.names  = FALSE,
    col.names  = TRUE,
    sep        = ",",
    na         = "",
    dateTimeAs = "write.csv"
  )
}

new_feats_to_detect_outliers()
