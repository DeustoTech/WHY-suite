# User defined variables
if (.Platform$OS.type == "windows") {
  # clValid files
  clValid_dir <- "G:/Mi unidad/WHY/Analyses/clValid/"
}
if (.Platform$OS.type == "unix") {
  # TO BE FILLED IF REQUIRED
}

################################################################################
##  File existence checker
################################################################################

check_existence_of <- function(folder, file) {
  # Check if folder not exists
  if (!file.exists(here::here(key, folder))) {
    # Create folder if not exist
    dir.create(here::here(key, folder))
    # File does not exist
    return(FALSE)
  } else {
    # Check if file exists
    return(file.exists(here::here(key, folder, file)))
  }
}

################################################################################
##  Stability validation
################################################################################

plot_stab_validation <- function() {
  # Name of the stats file to open or create
  stab_file <- paste0("stab", fname_id, ".png")
  # Does this computation already exist?
  if (!check_existence_of("stab", sta_file)) {
    png(
      here::here(key, "stab", sta_file),
      width = 1200,
      height = 900
    )
    # FEATURE SET 1
    if (ss == 1) {
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

for (fname in dset_fnames) {
  # Get clValid ID
  fname_id <- readr::parse_number(fname)
  # Load clValid file
  load(paste0(clValid_dir, fname))
  # APN
  ap
  # AD
  # ADM
  # FOM
}

# Plot stability validation
plot_stab_validation()

