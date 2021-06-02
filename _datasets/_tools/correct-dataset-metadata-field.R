# Correct a field of metadata in a dataset for all EXT files

# Dataset folder
dataset_folder <- "/home/ubuntu/carlos.quesada/disk/meg/ext/"

for (ff in list.files(dataset_folder, pattern="*.RData")) {
  load(paste0(dataset_folder, ff))
  edf$dset_key <- "meg"
  save(edf, file=paste0(dataset_folder, ff))
}