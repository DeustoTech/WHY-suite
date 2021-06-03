#########################
###  GET METADATA v3  ###
#########################

library(foreach)
library(stringi)

# Key of the dataset
key <- "por"

# User defined variables
if (.Platform$OS.type == "unix") {
  # Path of the output file
  out_file <- "/home/ubuntu/carlos.quesada/disk/features/metadata/metadata-meg.csv"
  # Folder of the extended files
  ext_dir <- "/home/ubuntu/carlos.quesada/disk/meg/ext/"
}
if (.Platform$OS.type == "windows") {
  # Path of the output file
  out_file <- "G:/Mi unidad/WHY/Features/feats_21.06.01/metadata-por.csv"
  # Folder of the extended files
  ext_dir <- "G:/Mi unidad/WHY/Datasets/por/ext/"
}

################################################################################

# Get all filenames in the ext folder
ext_filenames <- list.files(ext_dir)

# Setup parallel backend to use many processors
cores <- parallel::detectCores() - 1
cl <- parallel::makeCluster(cores, outfile = "")
doParallel::registerDoParallel(cl)

x <- foreach::foreach(ii = 1:length(ext_filenames), .combine = rbind, .inorder = TRUE, .errorhandling="remove", .packages = c("stringi")) %dol% {
  
  # Load file
  load(paste0(ext_dir, ext_filenames[ii]))
  print(ext_filenames[ii])
  
  o <- list()
  
  ## COMMON METADATA ###########################################################
  # file
  o$file <- sub('\\.csv$', '', edf$filename)
  # data_set
  o$data_set <- key
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
  if (key %in% c("goi", "go2", "meg", "cor")) {
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
	o$self_consumption_type <- edf$self_consump
  }
  
  ## ISSDA-SPECIFIC METADATA ###################################################
  if (key == "iss") {
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
  if (key == "lcl") {
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
  if (key == "nee") {
    # country
    o$country <- "us"
    # administrative_division
    o$administrative_division <- tolower(edf$state)
  } 
  
  ## PORTUGAL METADATA ####################################################
  if (key == "por") {
    # country
    o$country <- "pt"
  }  
  
  return(data.frame(o))
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
