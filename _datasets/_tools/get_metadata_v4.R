#########################
###  GET METADATA v3  ###
#########################

library(foreach)
library(stringi)

# User defined variables
if (.Platform$OS.type == "unix") {
  # EXT input folders
  input_folders <- c(
    "/home/ubuntu/carlos.quesada/disk/go2/ext/",
    "/home/ubuntu/carlos.quesada/disk/meg/ext/",
    "/home/ubuntu/carlos.quesada/disk/iss/ext/",
    "/home/ubuntu/carlos.quesada/disk/lcl/ext/",
    "/home/ubuntu/carlos.quesada/disk/por/ext/",
    "/home/ubuntu/carlos.quesada/disk/nee/ext/"
  )
  # Path of the output file
  out_file <- "/home/ubuntu/carlos.quesada/disk/features/metadata/2021.06.03_mdata.csv"
}
if (.Platform$OS.type == "windows") {
  # BETTER UNDER LINUX
}

################################################################################

# Get all filenames in the ext folders
ext_filenames <- c()
for (ii in 1:length(input_folders)) {
  ext_filenames <- c(
    ext_filenames,
    list.files(path=input_folders[ii], pattern="*.RData", full.names=T)
  )
}

# Setup parallel backend to use many processors
cores <- parallel::detectCores() - 1
cl <- parallel::makeCluster(cores, outfile = "")
doParallel::registerDoParallel(cl)

x <- foreach::foreach(
  ii             = 1:length(ext_filenames), 
  .combine       = dplyr::bind_rows, 
  .inorder       = T, 
  .errorhandling = "stop", 
  .packages      = c("stringi"),
  .verbose       = F
) %dopar% {
  
  # Load file
  load(ext_filenames[ii])
  print(ext_filenames[ii])
  
  o <- list()
  
  ## COMMON METADATA ###########################################################
  # file
  o$file <- sub('\\.csv$', '', edf$filename)
  # data_set
  o$data_set <- edf$dset_key
  # overall_start_date
  o$overall_start_date <- edf$df$times[1]
  # overall_end_date
  o$overall_end_date <- dplyr::last(edf$df$times)
  # overall_days
  o$overall_days <- as.numeric(
    difftime(
      o$overall_end_date,
      o$overall_start_date,
      units = "days")
  )
  ## Get vector of imputed dates
  imputed_dates <- which(edf$df$imputed == 2)
  # imputed_start_date
  o$imputed_start_date <- edf$df$times[imputed_dates[1]]
  # imputed_end_date
  o$imputed_end_date <- edf$df$times[dplyr::last(imputed_dates)]
  # imputed_days
  o$imputed_days <- as.numeric(
    difftime(
      o$imputed_end_date,
      o$imputed_start_date,
      units = "days")
  )
  if (is.na(o$imputed_days)) o$imputed_days <- 0
  # imputed_days_pct
  o$imputed_days_pct <- o$imputed_days / o$overall_days
  # imputed_na
  o$imputed_na <- edf$number_of_na
  # imputed_na_pct
  o$imputed_na_pct <- edf$number_of_na / length(edf$df$times)
  # total_imputed_pct
  o$total_imputed_pct <- o$imputed_days_pct + o$imputed_na_pct
  
  ## GOIENER-SPECIFIC METADATA #################################################
  if (o$data_set %in% c("goi", "go2", "meg", "cor")) {
    # country
    o$country <- "es"
    # administrative_division
    o$administrative_division <-
      stringi::stri_trans_general(
        stringr::str_replace(edf$province, ",", ";"),
        "latin-ascii"
      )
    # municipality
    o$municipality <-
      stringi::stri_trans_general(
        stringr::str_replace(edf$municipality, ",", ";"),
        "latin-ascii"
      )
    # zip_code
    o$zip_code <- edf$zip_code
    ## Spatial resolution
	if (length(edf$cnae) == 0) edf$cnae <- NA
    if (is.na(edf$cnae)) {
      o$is_household <- NA
    } else {
      if (floor(edf$cnae/100) == 98) {
        o$is_household <- 1
      } else {
        o$is_household <- 0
      }
    }
    # cnae
    o$cnae <- edf$cnae
    # acorn
    o$acorn <- NA
    # acorn_grouped
    o$acorn_grouped <- NA
  	# tariff
  	o$tariff <- edf$tariff
  	# contracted power
  	o$p1_kw <- edf$p1_kw
  	o$p2_kw <- edf$p2_kw
  	o$p3_kw <- edf$p3_kw
  	# type of self-consumption
  	if (length(edf$self_consump) == 0) {
  	  o$self_consumption_type <- NA
  	} else {
  	  o$self_consumption_type <- as.character(edf$self_consump)
  	}
  }
  
  ## ISSDA-SPECIFIC METADATA ###################################################
  if (o$data_set == "iss") {
    # country
    o$country <- "ie"
    # administrative_division
    o$administrative_division <- NA
    # municipality
    o$municipality <- NA
    # zip_code
    o$zip_code <- NA
    ## Spatial resolution
    if (edf$id == 1) {
      o$is_household <- 1
    } else {
      o$is_household <- 0
    }
    # cnae
    o$cnae <- NA
    # acorn
    o$acorn <- NA
    # acorn_grouped
    o$acorn_grouped <- NA
  }

  ## LOW CARBON LONDON-SPECIFIC METADATA #######################################
  if (o$data_set == "lcl") {
    # country
    o$country <- "gb"
    # administrative_division
    o$administrative_division <- "Greater London"
    # municipality
    o$municipality <- NA
    # zip_code
    o$zip_code <- NA
    ## Spatial resolution
    o$is_household <- 1
    # cnae
    o$cnae <- NA
    # acorn
    o$acorn <- edf$acorn
    # acorn_grouped
    o$acorn_grouped <- edf$acorn_grouped
  }
  
  ## NEEA-SPECIFIC METADATA ####################################################
  if (o$data_set == "nee") {
    # country
    o$country <- "us"
    # administrative_division
    o$administrative_division <- tolower(edf$state)
  } 
  
  ## PORTUGAL METADATA #########################################################
  if (o$data_set == "por") {
    # country
    o$country <- "pt"
  }  
  
  ## FINAL PROCESSING ##########################################################
  # Convert numeric(0) to NA
  for (jj in 1:length(o)) {
    if (length(o[[jj]]) == 0) o[[jj]] <- NA
  }
  
  return(as.data.frame(o))
}

# Stop parallelization
parallel::stopCluster(cl)

# Save
data.table::fwrite(
  x         = x,
  file      = out_file,
  append    = F,
  quote     = F,
  sep       = ",",
  row.names = F,
  col.names = T,
  dateTimeAs = "write.csv"
)
