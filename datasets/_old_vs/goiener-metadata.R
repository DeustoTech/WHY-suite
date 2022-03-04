# INCORPORATE METADATA TO GOIENER EXTENDED FILES FROM CONTRACT FILES

library(doParallel)

# Vector of folders to search in
f_in <- 
  # "/home/ubuntu/carlos.quesada/disk/goiener/ext-bug-corr-rep/"
  "C:/goiener-ext/ext-bug/"
# Output folder
f_out <- 
  #"/home/ubuntu/carlos.quesada/disk/goiener/ext-mdata/"
  "C:/goiener-ext/ext-mdata/"
# GOIENER contracts file
metadata_file <- "G:/.shortcut-targets-by-id/1g1D2rJfAektwZCB-O_F0EHxDYHxJmhmc/20WHY datasets/GOIENER/Contratos_Goiener_20201209_anonymized.csv"

# Load metadata file
metadata <- data.table::fread(
  file = metadata_file,
  header = TRUE,
  sep = ",",
  na.strings = ""
)

# Setup parallel backend to use many processors
cl <- parallel::makeCluster(parallel::detectCores() - 1)
doParallel::registerDoParallel(cl)

# Check all entries in contracts file
#foreach (x = 1:length(metadata[[1]])) %dopar% {
for (x in 1:length(metadata[[1]])) {
  # Compose file name
  fname_in <- paste(f_in, metadata[[1]][x], ".Rdata", sep="")
  # Check file existence
  if (file.exists(fname_in)) {
    # Load file
    load(fname_in)
    # Check metadata existence
    if (is.na(edf$cnae)) {
      ### Create new metadata
      edf$cups         <-            metadata[[ 1]][x]
      edf$start_date   <- as.POSIXct(metadata[[ 2]][x], tz="GMT")
      edf$end_date     <- as.POSIXct(metadata[[ 3]][x], tz="GMT")
      edf$tariff       <-            metadata[[ 4]][x]
      edf$p1_kw        <- as.numeric(metadata[[ 5]][x])
      edf$p2_kw        <- as.numeric(metadata[[ 6]][x])
      edf$p3_kw        <- as.numeric(metadata[[ 7]][x])
      edf$self_consump <-            metadata[[ 8]][x]
      edf$province     <-            metadata[[ 9]][x]
      edf$municipality <-            metadata[[10]][x]
      edf$zip_code     <- as.numeric(metadata[[11]][x])
      edf$cnae         <- as.numeric(metadata[[12]][x])
    }
    ### Save file
    fname_out <- paste(f_out, metadata[[1]][x], ".Rdata", sep="")
    save(edf, file = fname_out)
  }
}

parallel::stopCluster(cl)