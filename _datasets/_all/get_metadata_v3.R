#########################
###  GET METADATA v3  ###
#########################

library(foreach)
library(stringi)

# Key of the dataset
key <- "meg"

# User defined variables
if (.Platform$OS.type == "unix") {
  # Path of the output file
  out_file <- "/home/ubuntu/carlos.quesada/disk/features/metadata/metadata-meg.csv"
  # Folder of the extended files
  ext_dir <- "/home/ubuntu/carlos.quesada/disk/meg/ext/"
}
if (.Platform$OS.type == "windows") {
  # Path of the output file
  out_file <- "G:/Mi unidad/WHY/Features/metadata-go2.csv"
  # Folder of the extended files
  ext_dir <- "G:/Mi unidad/WHY/Datasets/go2/ext/"
}

################################################################################

# Get all filenames in the ext folder
ext_filenames <- list.files(ext_dir)

# Setup parallel backend to use many processors
cores <- parallel::detectCores() - 1
cl <- parallel::makeCluster(cores, outfile = "")
doParallel::registerDoParallel(cl)

x <- foreach::foreach(ii = 1:length(ext_filenames), .combine = rbind, .inorder = FALSE, .errorhandling="remove", .packages = c("stringi")) %dopar% {
  # Load file
  load(paste0(ext_dir, ext_filenames[ii]))
  print(ext_filenames[ii])
  
  o <- list()
  
  ## COMMON METADATA
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
  
  ## GOIENER-SPECIFIC METADATA
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
  
  ## ISSDA-SPECIFIC METADATA
  if (data_set == "iss") {
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

  ## LOW CARBON LONDON-SPECIFIC METADATA
  if (data_set == "lcl") {
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


# # FUNCTION TO LOAD METADATA FROM RDATA FILES
# load_metadata <- function(input_df) {
#   ## INPUTS
#   file_name <- input_df[[1]]
#   data_set  <- input_df[[2]]
#   ## OUTPUT
#   o <- list()
#
#   ## LOAD FILE
#   # Goiener data set
#   if (data_set %in% c("goi", "go2", "meg")) {
#     load(paste(goien_path, file_name, ".RData", sep=""))
#   }
#   # ISSDA data set
#   if (data_set == "iss") {
#     load(paste(issda_path, file_name, ".RData", sep=""))
#   }
#   # Low Carbon London data set
#   if (data_set == "lcl") {
#     load(paste(loclo_path, file_name, ".RData", sep=""))
#   }
#   # REFIT data set
#   if (data_set == "ref") {
#     load(paste(refit_path, file_name, ".RData", sep=""))
#   }
#
#   ## COMMON METADATA
#   # file
#   o$file <- file_name
#   # overall_start_date
#   o$overall_start_date <- edf$df$times[1]
#   # overall_end_date
#   o$overall_end_date <- dplyr::last(edf$df$times)
#   # overall_days
#   o$overall_days <- as.numeric(
#     difftime(
#       o$overall_end_date,
#       o$overall_start_date,
#       units = "days")
#   )
#   ## Get vector of imputed dates
#   imputed_dates <- which(edf$df$imputed == 2)
#   # imputed_start_date
#   o$imputed_start_date <- edf$df$times[imputed_dates[1]]
#   # imputed_end_date
#   o$imputed_end_date <- edf$df$times[dplyr::last(imputed_dates)]
#   # imputed_days
#   o$imputed_days <- as.numeric(
#     difftime(
#       o$imputed_end_date,
#       o$imputed_start_date,
#       units = "days")
#   )
#   if (is.na(o$imputed_days)) o$imputed_days <- 0
#   # imputed_days_pct
#   o$imputed_days_pct <- o$imputed_days / o$overall_days
#   # imputed_na
#   o$imputed_na <- edf$number_of_na
#   # imputed_na_pct
#   o$imputed_na_pct <- edf$number_of_na / length(edf$df$times)
#   # total_imputed_pct
#   o$total_imputed_pct <- o$imputed_days_pct + o$imputed_na_pct
#
#   ## GOIENER-SPECIFIC METADATA
#   if (data_set == "goi") {
#     # country
#     o$country <- "es"
#     # administrative_division
#     o$administrative_division <-
#       stringr::str_replace(edf$province, ",", ";")
#     # municipality
#     o$municipality <-
#       stringr::str_replace(edf$municipality, ",", ";")
#     # zip_code
#     o$zip_code <- edf$zip_code
#     ## Spatial resolution
#     if (is.na(edf$cnae)) {
#       o$is_household <- NA
#     } else {
#       if (floor(edf$cnae/100) == 98) {
#         o$is_household <- 1
#       } else {
#         o$is_household <- 0
#       }
#     }
#     # cnae
#     o$cnae <- edf$cnae
#     # acorn
#     o$acorn <- NA
#     # acorn_grouped
#     o$acorn_grouped <- NA
#   }
#
#   ## ISSDA-SPECIFIC METADATA
#   if (data_set == "iss") {
#     # country
#     o$country <- "ie"
#     # administrative_division
#     o$administrative_division <- NA
#     # municipality
#     o$municipality <- NA
#     # zip_code
#     o$zip_code <- NA
#     ## Spatial resolution
#     if (edf$id == 1) {
#       o$is_household <- 1
#     } else {
#       o$is_household <- 0
#     }
#     # cnae
#     o$cnae <- NA
#     # acorn
#     o$acorn <- NA
#     # acorn_grouped
#     o$acorn_grouped <- NA
#   }
#
#   ## LOW CARBON LONDON-SPECIFIC METADATA
#   if (data_set == "lcl") {
#     # country
#     o$country <- "gb"
#     # administrative_division
#     o$administrative_division <- "Greater London"
#     # municipality
#     o$municipality <- NA
#     # zip_code
#     o$zip_code <- NA
#     ## Spatial resolution
#     o$is_household <- 1
#     # cnae
#     o$cnae <- NA
#     # acorn
#     o$acorn <- edf$acorn
#     # acorn_grouped
#     o$acorn_grouped <- edf$acorn_grouped
#   }
#
#   ## REFIT-SPECIFIC METADATA
#   if (data_set == "ref") {
#     # country
#     o$country <- "gb"
#     # administrative_division
#     o$administrative_division <- NA
#     # municipality
#     o$municipality <- "Loughborough"
#     # zip_code
#     o$zip_code <- NA
#     ## Spatial resolution
#     o$is_household <- 1
#     # cnae
#     o$cnae <- NA
#     # acorn
#     o$acorn <- NA
#     # acorn_grouped
#     o$acorn_grouped <- NA
#   }
#
#   return(o)
# }
#
#
#
# # Generate table
# x <- do.call(dplyr::bind_rows, apply(feats[,1:2], 1, load_metadata))
