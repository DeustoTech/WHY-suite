# DAYLIGHT SUMMER TIME CORRECTION

library(foreach)

# INPUT FOLDERS
input_dirs <- c(
  "/home/ubuntu/carlos.quesada/disk/go2/",
  "/home/ubuntu/carlos.quesada/disk/meg/",
  "/home/ubuntu/carlos.quesada/disk/lcl/"
)
# OUTPUT FOLDERS
output_dirs <- c(
  "/home/ubuntu/carlos.quesada/disk/go2_dst/",
  "/home/ubuntu/carlos.quesada/disk/meg_dst/",
  "/home/ubuntu/carlos.quesada/disk/lcl_dst/"
)

in_path_list <- c()
out_path_list <- c()

for (ii in 1:3) {
  input_dir <- input_dirs[ii]
  output_dir <- output_dirs[ii]
  
  fnames <- list.files(input_dir)
  
  in_path_list <- c(
    in_path_list,
    paste0(input_dir, fnames)
  )
  
  out_path_list <- c(
    out_path_list,
    paste0(output_dir, fnames)
  )
}

cores <- parallel::detectCores() - 1
cl <- parallel::makeCluster(cores, outfile = "")
doParallel::registerDoParallel(cl)

o_ <- foreach::foreach(
  ff       = 1:length(in_path_list),
  .inorder = FALSE
) %dopar% {
  load(in_path_list[ff])
  
  df_1 <- edf$df
  df_2 <- edf$df
  
  attr(df_2$times, "tzone") <- "CET"
  # +1 and -1 indicate the EXACT moments
  time_diffs <- lubridate::hour(df_2$times) - lubridate::hour(df_1$times)
  time_diffs <- c(0, diff(time_diffs))
  
  times_df <- data.frame(
    times1     = df_1$times, 
    times2     = df_2$times,
    time_diffs = time_diffs,
    values     = df_1$values,
    imputed    = df_1$imputed
  )
  
  minpos <- which(times_df$time_diffs == +1)
  minneg <- which(times_df$time_diffs == -1)
  
  # PAD WITH AN EXTRA ROW IN THE BEGINNING IF WE START IN SUMMER TIME
  if (minneg[1] < minpos[1]) {
    times_df <- rbind(
      times_df[1,],
      times_df
    )
    times_df$imputed[1] <- 1
  }
  
  # REMOVE AN EXTRA ROW IN THE END IF WE FINNISH IN SUMMER TIME
  if (minpos[length(minpos)] > minneg[length(minneg)]) {
    times_df <- times_df[1:(nrow(times_df)-1),]
  }
  
  # ADD EXTRA HOURS
  while(TRUE) {
    idx <- (which(times_df$time_diffs == +1) - 1)[1]
    if (is.na(idx)) break
    new_row <- data.frame(
      times1 = NA,
      times2 = NA,
      time_diffs = 0,
      values = (times_df$values[idx] + times_df$values[idx+1])/2,
      imputed = 1
    )
    times_df$time_diffs[idx+1] <- 0
    times_df <- rbind(
      times_df[1:idx,],
      new_row,
      times_df[(idx+1):nrow(times_df),]
    )
  }
  
  # REMOVE EXTRA HOURS
  while(TRUE) {
    idx <- (which(times_df$time_diffs == -1) - 1)[1]
    if (is.na(idx)) break
    times_df <- rbind(
      times_df[1:idx,],
      times_df[(idx+2):nrow(times_df),]
    )
  }
  
  edf$df <- data.frame(
    times   = edf$df$times,
    values  = times_df$values,
    imputed = times_df$imputed
  )
  
  save(edf, file = out_path_list[ff])
}