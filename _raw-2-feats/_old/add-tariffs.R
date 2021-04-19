library(lubridate)
# File path to big CSV
feats_path <- "/home/ubuntu/carlos.quesada/disk/features/feats_v1.06.csv"
# GOIENER data
goien_fold <- "/home/ubuntu/carlos.quesada/R_scripts/"
# GOIENER extended data
goien_ext_fold <- "/home/ubuntu/carlos.quesada/disk/goi/ext/"
# GOIENER file 1
goi_1_file <- "Contratos_Goiener_20201013_anonymized.csv"
# GOIENER file 2
goi_2_file <- "Contratos_Goiener_20201209_anonymized.csv"
# GOIENER file paths
goi_1_path <- paste(goien_fold, goi_1_file, sep="")
goi_2_path <- paste(goien_fold, goi_2_file, sep="")
# Output folder
output_fold <- goien_fold

# Load feats
feats <- data.table::fread(
  file   = feats_path,
  header = TRUE,
  sep    = ","
)
# List of GOIENER filenames
goi_cups <- feats[feats$data_set == "goi",]$file

# Load Goiener 1 metadata
goi_1 <- data.table::fread(
  file   = goi_1_path,
  header = TRUE,
  sep    = ","
)
# Load Goiener 2 metadata
goi_2 <- data.table::fread(
  file   = goi_2_path,
  header = TRUE,
  sep    = ","
)
# Goiener
names(goi_1) <- names(goi_2)
goien <- rbind(goi_1, goi_2)
goien <- unique(goien)
goien <- dplyr::arrange(
  goien,
  cups_ref,
  desc(fecha_alta),
  desc(fecha_baja)
)

# Compute data
o <- data.frame()
for (gg in goi_cups) {
  # Indices of CUPS 
  idx <- which(goien$cups_ref == gg)
  # If CUPS has metadata
  if (length(idx) != 0) {
	print(gg)
	# Tariff type
	tariff <- goien$tarifa.tarifa_atr_ref[min(idx)]
	# Load extended time series file
	load(paste(goien_ext_fold, gg, ".RData", sep = ""))
	# Get start period
	# as.POSIXct(feats[feats$file == gg,]$overall_end_date, tz = "GMT")
	end_period <- edf$df$times[length(edf$df$times)]
	start_period <- end_period %m-% years(2)
	# Get just two years of the dataframe
	t <- edf$df[edf$df$times > start_period,1:2]
	t[,1] <- as.POSIXct(t[,1], tz="GMT")
	# Get the bins of the hours
	h_factor <- as.factor(lubridate::hour(t[,1]))
	# Aggregate data (sum) according to the bins
	aggr <- stats::aggregate(
	  x   = t[,2],
	  by  = list(bin = h_factor),
	  FUN = sum
	)
	### OLD TARIFFS ########################################################
	kwh_2y_total <- sum(aggr$x)
	# 00:00-23:59 -> PEAK
	if (tariff == "2.0A" | tariff == "2.1A") {
	  kwh_2y_peak_old   <- sum(aggr$x)
	  kwh_2y_flat_old   <- NA
	  kwh_2y_valley_old <- NA
	  pct_2y_peak_old   <- 1.0 
	  pct_2y_flat_old   <- NA
	  pct_2y_valley_old <- NA
	}
	# 12:00-21:59 -> PEAK
	# 22:00-11:59 -> VALLEY
	if (tariff == "2.0DHA" | tariff == "2.1DHA") {
	  kwh_2y_peak_old   <- sum(aggr$x[13:22])
	  kwh_2y_flat_old   <- NA
	  kwh_2y_valley_old <- sum(aggr$x[23:24]) + sum(aggr$x[1:12])
	  pct_2y_peak_old   <- kwh_2y_peak_old / kwh_2y_total
	  pct_2y_flat_old   <- NA
	  pct_2y_valley_old <- kwh_2y_valley_old / kwh_2y_total
	}
	# 13:00-22:59 -> PEAK
	# 23:00-00:59 & 07:00-12:59 -> FLAT
	# 01:00-06:59 -> VALLEY
	if (tariff == "2.0DHS" | tariff == "2.1DHS") {
	  kwh_2y_peak_old   <- sum(aggr$x[14:23])
	  kwh_2y_flat_old   <- aggr$x[24] + aggr$x[1] + sum(aggr$x[8:13])
	  kwh_2y_valley_old <- sum(aggr$x[2:7])
	  pct_2y_peak_old   <- kwh_2y_peak_old / kwh_2y_total
	  pct_2y_flat_old   <- kwh_2y_flat_old / kwh_2y_total
	  pct_2y_valley_old <- kwh_2y_valley_old / kwh_2y_total
	}
	### NEW TARIFFS (2.0TD) ################################################
	# Aggregate by workday/weekend
	d_f <- cut(t[,1], breaks = "1 day")
	# Convert to vector of dates
	d_v <- as.POSIXct(as.vector(d_f), tz="GMT")
	# Numbers indicating 0-4 weekdays, 5-6 weekends
	d_factor <- (wday(d_v) - 2) %% 7
	# Get TD bins
	td_factor <- as.numeric(h_factor)
	td_factor[d_factor == 5 | d_factor == 6] <- 0
	td_factor <- as.factor(td_factor)
	# 10:00-13:59 & 18:00-21:59 -> PEAK
	# 08:00-09:59 & 14:00-17:59 & 22:00-23:59 -> FLAT
	# 00:00-07:59 & WEEKENDS -> VALLEY
	kwh_2y_peak_new   <- sum(aggr$x[11:14]) + sum(aggr$x[19:22])
	kwh_2y_flat_new   <- sum(aggr$x[9:10]) + sum(aggr$x[15:18]) +
						 sum(aggr$x[23:24])
	kwh_2y_valley_new <- sum(aggr$x[0:8])
	pct_2y_peak_new   <- kwh_2y_peak_new / kwh_2y_total
	pct_2y_flat_new   <- kwh_2y_flat_new / kwh_2y_total
	pct_2y_valley_new <- kwh_2y_valley_new / kwh_2y_total
	
	### Put all data together into a dataframe #############################
	df <- data.frame(
	  cups              = gg, 
	  tariff            = tariff,
	  kwh_2y_peak_old   = kwh_2y_peak_old,
	  kwh_2y_flat_old   = kwh_2y_flat_old,
	  kwh_2y_valley_old = kwh_2y_valley_old,
	  pct_2y_peak_old   = pct_2y_peak_old,
	  pct_2y_flat_old   = pct_2y_flat_old,
	  pct_2y_valley_old = pct_2y_valley_old,
	  kwh_2y_peak_new   = kwh_2y_peak_new,
	  kwh_2y_flat_new   = kwh_2y_flat_new,
	  kwh_2y_valley_new = kwh_2y_valley_new,
	  pct_2y_peak_new   = pct_2y_peak_new,
	  pct_2y_flat_new   = pct_2y_flat_new,
	  pct_2y_valley_new = pct_2y_valley_new
	)
  # If CUPS does NOT have metadata
  } else {
	df <- data.frame(
	  cups              = gg,   tariff            = NA,
	  kwh_2y_peak_old   = NA,   kwh_2y_flat_old   = NA,
	  kwh_2y_valley_old = NA,   pct_2y_peak_old   = NA,
	  pct_2y_flat_old   = NA,   pct_2y_valley_old = NA,
	  kwh_2y_peak_new   = NA,   kwh_2y_flat_new   = NA,
	  kwh_2y_valley_new = NA,   pct_2y_peak_new   = NA,
	  pct_2y_flat_new   = NA,   pct_2y_valley_new = NA
	)
  }
  # Add to final dataframe
  o <- rbind(o, df)
}

### ADD SHORT DATAFRAME TO BIG FEATURES DATAFRAME
# Save o
data.table::fwrite(
  x         = o,
  file      = paste(output_folder, "goi_tariffs.csv", sep=""),
  append    = F,
  quote     = F,
  sep       = ",",
  row.names = F,
  col.names = T,
  dateTimeAs = "write.csv"
)
# Get dimensions of both dataframes
dim_feats <- dim(feats)
dim_o <- dim(o)
# Check that CUPS are properly sorted
if (all(feats$file[1:dim_o[1]] == o$file)) {
  # Pad short dataframe with NAs
  pad_o <- data.frame(
	matrix(
	  NA,
	  nrow = dim_feats[1] - dim_o[1],
	  ncol = dim_o[2]
	)
  )
  names(pad_o) <- names(o)
  pad_o <- rbind(o, pad_o)
  # Incorporate columns
  feats <- feats %>% add_column(pad_o[,2:dim_o[2]], .before = "file")
  browser()
} else {
  print("ERROR")
}
# Save feats
data.table::fwrite(
  x         = o,
  file      = paste(output_folder, "new_feats.csv", sep=""),
  append    = F,
  quote     = F,
  sep       = ",",
  row.names = F,
  col.names = T,
  dateTimeAs = "write.csv"
)