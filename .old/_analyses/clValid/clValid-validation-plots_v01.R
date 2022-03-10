# User defined variables
if (.Platform$OS.type == "windows") {
  # clValid files
  clValid_dir <- "G:/Mi unidad/WHY/Analyses/clValid/data/"
  output_dir <- "G:/Mi unidad/WHY/Analyses/clValid/"
}
if (.Platform$OS.type == "unix") {
  # TO BE FILLED IF REQUIRED
}

################################################################################
##  File existence checker
################################################################################

check_existence_of <- function(folder, file) {
  # Check if folder not exists
  if (!file.exists(paste0(output_dir, folder))) {
    # Create folder if not exist
    dir.create(paste0(output_dir, folder, "/"))
    # File does not exist
    return(FALSE)
  } else {
    # Check if file exists
    return(file.exists(paste0(output_dir, folder, "/", file)))
  }
}

################################################################################
##  Internal validation
################################################################################

plot_internal_validation <- function() {
  key_wd <- "meas"
  for (ii in 1:3) {
    # Name of the stats file to open or create
    key_file <- paste0(key_wd, "_", fname_id, "_", ii, ".png")
    # Does this computation already exist?
    if (!check_existence_of(key_wd, key_file) & !all(is.na(o@measures[ii,,1]))) {
      png(
        paste0(output_dir, key_wd, "/", key_file),
        width  = 1200,
        height = 900
      )
      plot(
        x    = o@nClust,
        y    = o@measures[ii,,1],
        type = "o",
        xlab = "Number of clusters",
        ylab = o@measNames[ii]
      )
      dev.off()
    }
  }
}

################################################################################
##  Stability validation
################################################################################

plot_stability_validation <- function() {
  key_wd <- "meas"
  for (ii in 1:4) {
    # Name of the stats file to open or create
    key_file <- paste0(key_wd, "_", fname_id, "_", ii, ".png")
    # Does this computation already exist?
    if (!check_existence_of(key_wd, key_file) & !all(is.na(o@measures[ii,,1]))) {
      png(
        paste0(output_dir, key_wd, "/", key_file),
        width  = 1200,
        height = 900
      )
      plot(
        x    = o@nClust,
        y    = o@measures[ii,,1],
        type = "o",
        xlab = "Number of clusters",
        ylab = o@measNames[ii]
      )
      dev.off()
    }
  }
}

################################################################################
##  MAIN ANALYSIS
################################################################################

library(readr)
library(here)

# Get list of filenames in dataset folder
dset_fnames <- list.files(clValid_dir)
dset_fnames <- dset_fnames[dset_fnames != "desktop.ini"]

# 1st DIGIT: SETS OF FEATURES
# 2nd DIGIT: DATASETS
# 3rd DIGIT: CLUSTERING METHOD
# 4th DIGIT: VALIDATION METHOD
# 5th DIGIT: SET OF NUMBERS OF CLUSTERS

for (fname in dset_fnames) {
  # Get clValid ID
  fname_id <- readr::parse_number(fname)
  
  # Extract digits
  extr_fname_id <- as.integer(
    substring(
      fname_id,
      seq(nchar(fname_id)),
      seq(nchar(fname_id))
    )
  )
  
  # Load file
  print(paste0(clValid_dir, fname))
  load(paste0(clValid_dir, fname))
  
  if(extr_fname_id[4] == 1) {
    # Plot internal validation
    plot_internal_validation()
  }
  
  if(extr_fname_id[4] == 2) {
    # Plot stability validation
    plot_stability_validation()
  }
}

