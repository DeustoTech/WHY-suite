# This file changes the time offset of NEEA ext files by adding (actually
# subtracting) the tz_utc_offset field to the times column.

folder <- "G:/Mi unidad/WHY/Datasets/nee/ext/"
for (ff in list.files(folder, pattern="*.RData")) {
  print(ff)
  load(paste0(folder, ff))
  edf$df$times <- edf$df$times + as.difftime(edf$tz_utc_offset, units="hours")
  save(edf, file=paste0(folder, ff))
}