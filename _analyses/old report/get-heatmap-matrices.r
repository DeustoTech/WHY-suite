library(foreach)

# Script parameters
dsets <- c("lcl", "iss", "goi")
fsets <- c(3, 7)
n_ccs <- 25:2

# Inputs & outputs
feats_file      <- "/home/ubuntu/carlos.quesada/disk/features/feats_v1.05.csv"
output_folder   <- "/home/ubuntu/carlos.quesada/R_scripts/heatmaps/"
datasets_folder <- "/home/ubuntu/carlos.quesada/disk/"

# Load feats
feats <- data.table::fread(
  file   = feats_file,
  header = TRUE,
  sep    = ","
)

for (dset in dsets) {
	for (fset in fsets) {
		for (n_cc in n_ccs) {
			# Load clusterization
			km_cluster_path <- paste("/home/ubuntu/carlos.quesada/R_scripts/heatmaps/analysis_", dset, "_set", fset, "_cc", n_cc, ".RData", sep="")
			load(km_cluster_path)

			# Retrieve working data
			w_feats <- subset(
			  x      = feats,
			  subset =
				data_set == dset & is_household == 1 & total_imputed_pct < 2/3,
			  select = c("data_set", "file")
			)

			# Setup parallel backend to use many processors
			cores <- parallel::detectCores() - 1
			cl <- parallel::makeCluster(cores)
			doParallel::registerDoParallel(cl)

			# Loop
			foreach::foreach (cc = 1:n_cc) %dopar% {
			  # Get cluster list
			  cluster_list <- w_feats[km$cluster == cc,]
			  # Set vector of paths
			  paths_vector <- paste(
				datasets_folder,
				cluster_list$data_set,
				"/ext/",
				cluster_list$file,
				".RData",
				sep = ""
			  )
			  # Get heatmap matrix
			  m <- whyT2.1::get_heatmap_matrix(data.frame(paths_vector))
			  # Save heatmap matrix
			  save(m, file = paste(output_folder, "heatmap_", dset, "_s", fset, "_c", n_cc, "-", cc, ".RData", sep=""))
			}
			
			# Stop parallelization
			parallel::stopCluster(cl)
		}
	}
}