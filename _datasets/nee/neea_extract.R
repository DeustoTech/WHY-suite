# EXTRACT RAW DATA FROM NEEA DATASET ###########################################

# User defined variables
if (.Platform$OS.type == "windows") {
  in_dir  <- "G:/Mi unidad/WHY/Datos (raw)/NEEA/"
  out_dir <- "G:/Mi unidad/WHY/Datasets/nee/raw/"
}
if (.Platform$OS.type == "unix") {
  in_dir  <- "/home/ubuntu/carlos.quesada/disk/nee/pre-raw/"
  out_dir <- "/home/ubuntu/carlos.quesada/disk/nee/raw/"
}

yearz   <- 2018:2020

################################################################################

# Months per Q
months_per_q <- list()
for (ii in 1:4) months_per_q[[ii]] <- month.abb[(3*ii-2):(3*ii)]

# Function that extracts the data given an existing file
do_the_magic <- function(in_file) {
  # Load feats
  w_data <- data.table::fread(
    file   = in_file,
    header = TRUE,
    sep    = ","
  )
  
  uniq_values <- unique(w_data[,c("ee_site_id", "regname")])

  for (rr in dim(uniq_values)[1]) {
    # Subset data
    sel_df <- subset(
      w_data,
      ee_site_id == uniq_values$ee_site_id[rr] & 
        regname == uniq_values$regname[rr],
      select = c(min_t, power)
    )
    
    # From kW to kWh
    sel_df$power <- sel_df$power * 0.25
    
    # File name
    fpath <- paste0(
      out_dir,
      uniq_values$ee_site_id[rr],
      "-",
      uniq_values$regname[rr],
      ".csv"
    )
    
    # Save file
    data.table::fwrite(
      x         = sel_df,
      file      = fpath,
      append    = T,
      quote     = F,
      sep       = ",",
      row.names = F,
      col.names = T,
      dateTimeAs = "write.csv"
    )
  }
}

# Loop to find an existing file
for (yy in yearz) {
  for (qq in 1:4) {
    # Create dir name
    in_sdir <- paste0(in_dir, yy, "_Q", qq, "_v3/")
    # Check dir existence
    if (dir.exists(in_sdir)) {
      for (mm in months_per_q[[qq]]) {
        in_ssdir <- paste0(in_sdir, mm, "/power/")
        # Check month dir existence
        if (dir.exists(in_ssdir)) {
          for (dd in 1:31) {
            in_file <- paste0(
              in_ssdir, "power-fifteen_", yy,
              stringr::str_pad(which(month.abb == mm), 2, pad="0"),
              stringr::str_pad(dd, 2, pad="0"), ".csv"
            )
            if (file.exists(in_file)) {
              do_the_magic(in_file)
            }
          }
        }
      }
    }
  }
}