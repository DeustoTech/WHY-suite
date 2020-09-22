# Carlos Quesada - Universidad de Deusto
# 2020.07.21
# Create a visual library of features so they can be easily understood

# Load source file and libraries
source("why-source.R")

# User parameters
dset_key <- "lcl"
from_date <- ISOdate(2013, 2, 1, 0, 0, 0) #"first"
to_date <- ISOdate(2013, 2, 28, 23, 30, 0) #"last"
samples_per_day <- list(lcl = 48)
step_in_secs <- 86400 / samples_per_day[[dset_key]]
results_folder <- "G:/Mi unidad/WHY/Resultados/lcl/features/2012-2013, 0% NA, scale=FALSE, 70 feats/"
feats_to_plot <- 5:60

# Call the function
Create_Features_Library(
  results_folder = results_folder,
  feats_to_plot  = feats_to_plot
)
