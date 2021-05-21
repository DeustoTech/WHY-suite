library(NbClust)

set.seed(1981)

feats_vers <- "v1.12"

# User defined variables
if (.Platform$OS.type == "windows") {
  feats_dir <- "G:/Mi unidad/WHY/Features/"
  out_dir   <- "G:/Mi unidad/WHY/Analyses/NbClust/"
}
if (.Platform$OS.type == "unix") {
  feats_dir <- "/home/ubuntu/carlos.quesada/disk/features/"
  out_dir   <- "/home/ubuntu/carlos.quesada/analyses/NbClust/"
}

feats_path <- paste0(feats_dir, "feats_", feats_vers, ".csv")

feats <- data.table::fread(
  file   = feats_path,
  header = TRUE,
  sep    = ","
)

row_conditions <-
  feats$data_set %in% c("go2", "meg") &
  feats$imputed_na_pct < 0.1 &
  feats$is_household %in% 1 &
  feats$sum_per_day > 0.1

ft_set <- c(
  "rel_mean_00h04hspr", "rel_mean_04h08hspr", "rel_mean_08h12hspr",
  "rel_mean_12h16hspr", "rel_mean_16h20hspr", "rel_mean_20h00hspr",
  "rel_mean_00h04hsum", "rel_mean_04h08hsum", "rel_mean_08h12hsum",
  "rel_mean_12h16hsum", "rel_mean_16h20hsum", "rel_mean_20h00hsum",
  "rel_mean_00h04haut", "rel_mean_04h08haut", "rel_mean_08h12haut",
  "rel_mean_12h16haut", "rel_mean_16h20haut", "rel_mean_20h00haut",
  "rel_mean_00h04hwin", "rel_mean_04h08hwin", "rel_mean_08h12hwin",
  "rel_mean_12h16hwin", "rel_mean_16h20hwin", "rel_mean_20h00hwin",
  "rel_mean_weekday_pday"
)

row.names(feats) <- paste0(feats$data_set, "_", feats$file)
feats <- feats[row_conditions,]
feats <- subset(feats, select = ft_set)
feats <- as.matrix(feats)

o <- NbClust::NbClust(
  data       = feats,
  diss       = NULL,
  distance   = "euclidean",
  min.nc     = 15,
  max.nc     = 35,
  method     = "kmeans",
  index      = "all"
)

filename <- paste0(out_dir, "o_kmeans.NbClust")
save("o", file = filename)
