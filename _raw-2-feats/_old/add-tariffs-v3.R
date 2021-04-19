# Version 3 accounts for the maximum point value of all periods in the new tariff

library(lubridate)
library(foreach)

if (.Platform$OS.type == "windows") {
  # File path to big CSV
  feats_path <- "G:/Mi unidad/WHY/Features/feats_v1.06.csv"
  # Path to disk
  disk_path <- "G:/Mi unidad/WHY/Datasets/"
}
if (.Platform$OS.type == "unix") {
  # File path to big CSV
  feats_path <- "/home/ubuntu/carlos.quesada/disk/features/feats_v1.06.csv"
  # Path to disk
  disk_path <- "/home/ubuntu/carlos.quesada/disk/"
  # Output dataframe
  output_folder <- "/home/ubuntu/carlos.quesada/R_scripts/"
}

# Load feats
feats <- data.table::fread(
  file   = feats_path,
  header = TRUE,
  sep    = ","
)

# Dataset loop
for (dset in c("goi")) { #  c("goi", "iss", "lcl", "ref")
  # List of dataset filenames
  fnames <- feats[feats$data_set == dset,]$file
  
  # Setup parallel backend to use many processors
  cores <- parallel::detectCores() - 1
  cl <- parallel::makeCluster(cores)
  doParallel::registerDoParallel(cl)
  
  # Filenames loop
  o <- foreach::foreach (ff = 1:length(fnames), .combine = rbind, .packages = c("lubridate")) %do% {
    # Complete filename
    fname <- paste0(fnames[ff], ".RData")
    # Load file
    load(paste0(disk_path, dset, "/ext/", fname))
    # Get start period
    end_period <- tail(edf$df$times, n=1)
    start_period <- end_period %m-% years(2)
    # Get just two years of the dataframe
    t <- edf$df[edf$df$times > start_period, 1:2]
    t[,1] <- as.POSIXct(t[,1], tz="GMT")

    ### TARIFF PERIODS #########################################################
    #            TOTAL -> 00:00 - 23:59
    #         OLD PEAK -> 13:00 - 22:59
    #         OLD FLAT -> 23:00 - 00:59 & 07:00 - 12:59
    #       OLD VALLEY -> 23:00 - 12:59
    # OLD SUPER-VALLEY -> 01:00 - 06:59
    #         NEW PEAK -> 10:00 - 13:59 & 18:00 - 21:59 (WORKDAYS)
    #         NEW FLAT -> 08:00 - 09:59 & 14:00 - 17:59 & 22:00 - 23:59 (WDs)
    #       NEW VALLEY -> 00:00 - 07:59 (WORKDAYS) & FULL WEEKENDS
    ############################################################################
    
    ### NEW TARIFFS
    # Get the bins of the hours
    h_factor <- lubridate::hour(t[,1])
    h_factor[h_factor == 0] <- 24
    h_factor <- as.factor(h_factor)
    # Aggregate by workday/weekend
    d_f <- cut(t[,1], breaks = "1 day")
    # Convert to vector of dates
    d_v <- as.POSIXct(as.vector(d_f), tz="GMT")
    # Numbers indicating 0-4 weekdays, 5-6 weekends
    d_factor <- (wday(d_v) - 2) %% 7
    # Get TD bins
    td_factor <- as.numeric(h_factor)
    td_factor[d_factor == 5 | d_factor == 6] <- 99
    td_factor <- as.factor(td_factor)
    # Aggregate data (sum) according to the bins
    aggr_n <- stats::aggregate(
      x   = t[,2],
      by  = list(bin = td_factor),
      FUN = max
    )
    
    # NEW PEAK -> 10:00 - 13:59 & 18:00 - 21:59 (WORKDAYS)
    max_kwh_2y_peak_new <- max(c(aggr_n$x[10:13], aggr_n$x[18:21]))
    # NEW FLAT -> 08:00 - 09:59 & 14:00 - 17:59 & 22:00 - 23:59 (WDs)
    max_kwh_2y_flat_new <- 
      max(c(aggr_n$x[8:9], aggr_n$x[14:17], aggr_n$x[22:23]))
    # NEW VALLEY -> 00:00 - 07:59 (WDs) & 00:00 - 23:59 (WEEKENDS)
    max_kwh_2y_valley_new <- max(c(aggr_n$x[1:7], aggr_n$x[24:25]))
    # NEW PEAK & FLAT
    max_kwh_2y_peakflat_new <- max(c(max_kwh_2y_peak_new, max_kwh_2y_flat_new))
    
    # Output
    df <- data.frame(
      filename                = fnames[ff],
      max_kwh_2y_peak_new     = max_kwh_2y_peak_new,
      max_kwh_2y_flat_new     = max_kwh_2y_flat_new,
      max_kwh_2y_valley_new   = max_kwh_2y_valley_new,
      max_kwh_2y_peakflat_new = max_kwh_2y_peakflat_new
    )
  }
  
  # Stop parallelization
  parallel::stopCluster(cl)
}

# Save o
data.table::fwrite(
  x         = o,
  file      = paste0(output_folder, "goi_tariffs_v4.csv"),
  append    = F,
  quote     = F,
  sep       = ",",
  row.names = F,
  col.names = T,
  dateTimeAs = "write.csv"
)