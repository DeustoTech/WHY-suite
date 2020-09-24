# Carlos Quesada - Universidad de Deusto
# 2020.07.21
# Create a visual library of features so they can be easily understood

# Load source file and libraries
source("why-source.R", local=TRUE)

# User parameters
sampling_period <- 86400 / SAMPLES_PER_DAY[["lcl"]]
feats_folder    <- "G:/Mi unidad/WHY/Resultados/lcl/features/2012-2013, 0% NA, scale=FALSE, 70 feats/"
feats_to_plot   <- c(1:10, 15:70)

# Function call
Create_Features_Library(
  sampling_period = sampling_period,
  feats_folder    = feats_folder,
  feats_to_plot   = feats_to_plot
)
