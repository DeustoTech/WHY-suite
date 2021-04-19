# CNAE ID identificator in GOIENER files

# Folder of extended GOIENER files
f_in <- "C:/goiener-ext/"
# Get all filenames in folder
fnames <- list.files(f_in, pattern="^[[:xdigit:]]")
# Initialization
cnae_list <- c()
# Loop to get cnae codes
for (fname in fnames) {
  load(paste(f_in, fname, sep=""))
  cnae_list <- c(cnae_list, edf$cnae)
}
# Create table
tbl <- table(cnae_list)
tbl <- tbl[order(-as.numeric(tbl))]
