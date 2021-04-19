library(whyT2.1)
library(foreach)
# Input parameters
input_folder      <- c(
	"/home/ubuntu/carlos.quesada/disk/meg/ext/"
	#"/home/ubuntu/carlos.quesada/disk/goiener/ext-bug-corr-rep/",
	#"/home/ubuntu/carlos.quesada/disk/issda/ext/",
	#"/home/ubuntu/carlos.quesada/disk/lcl/ext/",
	#"/home/ubuntu/carlos.quesada/disk/refit/ext/"
)
output_folder     <- c(
	"/home/ubuntu/carlos.quesada/disk/features/meg_21.04.15/"
	#"/home/ubuntu/carlos.quesada/disk/features/goiener_21.02.04/",
	#"/home/ubuntu/carlos.quesada/disk/features/issda_21.02.04/",
	#"/home/ubuntu/carlos.quesada/disk/features/lcl_21.02.04/",
	#"/home/ubuntu/carlos.quesada/disk/features/refit_21.02.04/"
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
