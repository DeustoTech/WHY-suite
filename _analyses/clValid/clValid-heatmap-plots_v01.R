library(foreach)
library(stringr)

# User defined variables
if (.Platform$OS.type == "windows") {
  # Input
  hmm_dir <- "G:/Mi unidad/WHY/Analyses/clValid/hmm/"
  # Output
  hmp_dir <- "G:/Mi unidad/WHY/Analyses/clValid/hmp/"
}
if (.Platform$OS.type == "unix") {
  # TO BE FILLED IF REQUIRED
}

# Get list of filenames in dataset folder
dset_fnames <- list.files(hmm_dir)
dset_fnames <- dset_fnames[dset_fnames != "desktop.ini"]

# Setup parallel backend to use many processors
cores <- parallel::detectCores() - 1
cl <- parallel::makeCluster(cores)
doParallel::registerDoParallel(cl)

# Loop
foreach::foreach (ff = 1:length(dset_fnames)) %do% {
  
  load(paste0(hmm_dir, dset_fnames[ff]))
  
  output_path <- paste0(
    hmp_dir,
    tools::file_path_sans_ext(dset_fnames[ff]),
    ".png"
  )
  
  output_path <- stringr::str_replace(output_path, "/hmm_", "/hmp_")
  
  whyT2.1::plot_heatmap_matrix(
    m           = m,
    format_file = "png",
    file_path   = output_path,
    plot_width  = 1200,
    plot_height = 900
  )
}

# Stop parallelization
parallel::stopCluster(cl)