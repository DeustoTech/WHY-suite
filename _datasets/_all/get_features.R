library(whyT2.1)
library(foreach)
# Input parameters
input_folder      <- c(
	"/home/ubuntu/carlos.quesada/disk/go2/ext/",
	"/home/ubuntu/carlos.quesada/disk/iss/ext/",
	"/home/ubuntu/carlos.quesada/disk/lcl/ext/",
	"/home/ubuntu/carlos.quesada/disk/meg/ext/",
	"/home/ubuntu/carlos.quesada/disk/por/ext/"
)
output_folder     <- c(
	"/home/ubuntu/carlos.quesada/disk/features/go2_21.05.27/",
	"/home/ubuntu/carlos.quesada/disk/features/iss_21.05.27/",
	"/home/ubuntu/carlos.quesada/disk/features/lcl_21.05.27/",
	"/home/ubuntu/carlos.quesada/disk/features/meg_21.05.27/",
	"/home/ubuntu/carlos.quesada/disk/features/por_21.05.27/"
)
type_of_analysis   <- "extra" #"basic"
#list_of_functions <- c("stat_data_aggregates", "load_factors")
#.scale            <- FALSE

# Compute features
for (ii in 1:1) {
	feats <- whyT2.1::get_features_from_ext_datasets(input_folder[ii], output_folder[ii], type_of_analysis)
	#feats <- whyT2.1::get_features_from_ext_datasets(input_folder[ii], output_folder[ii], type_of_analysis, 
	#list_of_functions=list_of_functions, .scale=.scale, parallelize=TRUE)
}
