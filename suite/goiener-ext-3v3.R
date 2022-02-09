library(foreach)
library(lubridate)
library(tidyr)

################################################################################
# PREVIOUS FUNCTIONS FROM "whyT2.1" PACKAGE
################################################################################

################################################################################
# get_samples_per_day()
################################################################################

get_samples_per_day <- function() {
  list(
    goi = 24, 
    iss = 48, 
    lcl = 48, 
    nee = 96,
    por = 96,
    ref = 24
  )
}

################################################################################
# extract_metadata()
################################################################################

extract_metadata <- function(edf, dfs, dset_key, filename) {
  # Initialize the output list
  out <- list()
  
  # Common metadata (depending on the data.frame) ##############################
  out[["data_set"]]       <- edf$dset_key
  out[["num_of_samples"]] <- length(edf$df$values)
  out[["ts_start_date"]]  <- edf$df$times[1]
  out[["ts_end_date"]]    <- edf$df$times[nrow(edf$df)]
  out[["ts_days"]]        <- as.numeric(edf$df$times[nrow(edf$df)] - edf$df$times[1])
  out[["abs_imputed_na"]] <- edf$number_of_na
  out[["rel_imputed_na"]] <- edf$number_of_na / length(edf$df$values)
  
  # Low Carbon London ##########################################################
  if (dset_key == "lcl") {
    # Identify current user
    file_id <- strsplit(filename, ".csv")[[1]]
    out[["fname"]] <- file_id
    
    # Retrieve index in metadata file
    idx <- which(dfs[[1]]$LCLid == file_id)
    
    # Retrieve all columns in metadata file
    out[["std_or_ToU"]]     <- dfs[[1]]$stdorToU[idx]
    out[["acorn"]]          <- dfs[[1]]$Acorn[idx]
    out[["acorn_grouped"]]  <- dfs[[1]]$Acorn_grouped[idx]
    out[["block_file"]]     <- dfs[[1]]$file[idx]
    
    # Processed metadata
    out[["mdata_file_idx"]] <- idx
    out[["country"]] <- "gb"
    out[["is_household"]] <- 
      ifelse(dfs[[1]]$Acorn_grouped[idx] %in% c("ACORN-", "ACORN-U"), 0, 1)
  }
  
  # ISSDA ######################################################################
  if (dset_key == "iss") {
    # Identify current user
    file_id <- strsplit(filename, ".csv")[[1]]
    out[["fname"]] <- file_id
    
    # Retrieve index in metadata file
    idx <- which(dfs[[1]]$id == file_id)
    
    # Retrieve all columns in metadata file
    out[["iss_code"]]             <- dfs[[1]]$code[idx]
    out[["resid_tariff_alloc"]]   <- dfs[[1]]$residential_tariff_allocation[idx]
    out[["resid_stimulus_alloc"]] <- dfs[[1]]$residential_stimulus_allocation[idx]
    out[["sme_alloc"]]            <- dfs[[1]]$sme_allocation[idx]
    
    # Processed metadata
    out[["mdata_file_idx"]] <- idx
    out[["country"]] <- "ie"
    out[["is_household"]] <- ifelse(dfs[[1]]$code[idx] == 1, 1, 0)
  }
  
  # NEEA #######################################################################
  if (dset_key == "nee") {
    # Identify current user
    file_id <- strsplit(filename, "-Mains.csv")[[1]]
    out[["fname"]] <- file_id
    
    # Retrieve index in metadata file
    idx <- which(dfs[[1]]$ee_site_id == file_id)
    
    # Retrieve all columns in metadata file
    out[["site_id"]]       <- dfs[[1]]$site_id[idx]
    out[["administrative_division"]] <- dfs[[1]]$state[idx]
    out[["hz"]]            <- dfs[[1]]$hz[idx]
    out[["cz"]]            <- dfs[[1]]$cz[idx]
    out[["tz_abbr"]]       <- dfs[[1]]$tz_abbr[idx]
    out[["tz_utc_offset"]] <- dfs[[1]]$tz_utc_offset[idx]
    out[["station_id"]]    <- dfs[[1]]$station_id[idx]
    
    # Processed metadata
    out[["mdata_file_idx"]] <- idx
    out[["country"]] <- "us"
    out[["is_household"]] <- 1
  }
  
  # GoiEner ####################################################################
  if (dset_key == "goi") {
    # Identify current user
    cups <- strsplit(filename, ".csv")[[1]]
    out[["fname"]] <- cups
    
    if("cups_ref" %in% colnames(dfs[[1]])) {
      all_idx <- which(dfs[[1]]$cups_ref == cups)
      idx <- max(all_idx)
      out[["mdata_file_idx"]] <- idx
    }
    if("fecha_alta" %in% colnames(dfs[[1]])) {
      start_date <- min(lubridate::ymd(dfs[[1]]$fecha_alta[all_idx]))
      out[["contract_start_date"]] <- start_date
    }
    if("fecha_baja" %in% colnames(dfs[[1]])) {
      end_date <- max(lubridate::ymd(dfs[[1]]$fecha_baja[all_idx]))
      out[["contract_end_date"]] <- end_date
    }
    if("tarifa_ref" %in% colnames(dfs[[1]])) {
      ref_tariff <- dfs[[1]]$tarifa_ref[idx]
      ref_tariff <- iconv(ref_tariff, from="UTF-8", to="ASCII//TRANSLIT")
      out[["ref_tariff"]] <- ref_tariff
    }
    if("tarifa.tarifa_atr_ref" %in% colnames(dfs[[1]])) {
      ref_atr_tariff <- dfs[[1]]$tarifa.tarifa_atr_ref[idx]
      out[["ref_atr_tariff"]] <- ref_atr_tariff
    }
    if("proceso_atr_ref" %in% colnames(dfs[[1]])) {
      ref_atr_proc <- dfs[[1]]$proceso_atr_ref[idx]
      out[["ref_atr_proc"]] <- ref_atr_proc
    }
    if("modo_facturacion_ref" %in% colnames(dfs[[1]])) {
      billing_type <- dfs[[1]]$modo_facturacion_ref[idx]
      out[["billing_type"]] <- billing_type
    }
    if("margen_indexado" %in% colnames(dfs[[1]])) {
      indexed_margin <- dfs[[1]]$margen_indexado[idx]
      out[["indexed_margin"]] <- indexed_margin
    }
    if("tipo_autoconsumo_ref" %in% colnames(dfs[[1]])) {
      self_consump <- dfs[[1]]$tipo_autoconsumo_ref[idx]
      out[["self_consump"]] <- self_consump
    }
    if("p1_kw" %in% colnames(dfs[[1]])) {
      p1_kw <- dfs[[1]]$p1_kw[idx]
      out[["p1_kw"]] <- p1_kw
    }
    if("p2_kw" %in% colnames(dfs[[1]])) {
      p2_kw <- dfs[[1]]$p2_kw[idx]
      out[["p2_kw"]] <- p2_kw
    }
    if("p3_kw" %in% colnames(dfs[[1]])) {
      p3_kw <- dfs[[1]]$p3_kw[idx]
      out[["p3_kw"]] <- p3_kw
    }
    if("p4_kw" %in% colnames(dfs[[1]])) {
      p4_kw <- dfs[[1]]$p4_kw[idx]
      out[["p4_kw"]] <- p4_kw
    }
    if("p5_kw" %in% colnames(dfs[[1]])) {
      p5_kw <- dfs[[1]]$p5_kw[idx]
      out[["p5_kw"]] <- p5_kw
    }
    if("p6_kw" %in% colnames(dfs[[1]])) {
      p6_kw <- dfs[[1]]$p6_kw[idx]
      out[["p6_kw"]] <- p6_kw
    }
    if("cups.direccion_prov.nombre_oficial" %in% colnames(dfs[[1]])) {
      province <- dfs[[1]]$cups.direccion_prov.nombre_oficial[idx]
      province <- iconv(province, from="UTF-8", to="ASCII//TRANSLIT")
      out[["administrative_division"]] <- province
    }
    if("cups.direccion_muni.nombre_oficial" %in% colnames(dfs[[1]])) {
      municipality <- dfs[[1]]$cups.direccion_muni.nombre_oficial[idx]
      municipality <- iconv(municipality, from="UTF-8", to="ASCII//TRANSLIT")
      out[["municipality"]] <- municipality
    }
    if("cups.direccion_cp" %in% colnames(dfs[[1]])) {
      zip_code <- dfs[[1]]$cups.direccion_cp[idx]
      out[["zip_code"]] <- zip_code
    }
    if("cnae" %in% colnames(dfs[[1]])) {
      cnae <- dfs[[1]]$cnae[idx]
      out[["cnae"]] <- cnae
    }
    
    # Processed metadata
    out[["country"]] <- "es"
    out[["is_household"]] <- ifelse(dfs[[1]]$cnae[idx] %/% 100 == 98, 1, 0)
  }
  
  # REFIT ######################################################################
  if (dset_key == "ref" | dset_key == "por") {
    out <- NULL
  }
  
  return(out)
}

################################################################################
# get_raw_dataframe_from_dataset()
################################################################################

get_raw_dataframe_from_dataset <- function(csv_file) {
  # Load data from CSV file
  data <- data.table::fread(
    file = csv_file,
    header = FALSE,
    sep = ",",
    na.strings = ""
  )
  # Times
  times <- lubridate::ymd_hms(data$V1)
  # Values
  values <- data$V2
  # 2 columns
  if (ncol(data) == 2) {
    return(data.frame(times, values))
  } else {
    # Imputed
    imputed <- data$V3
    # 3 columns
    if (ncol(data) == 3) {
      return(data.frame(times, values, imputed))
    } else {
      # Original times
      original_times <- lubridate::ydm_hms(data$V4)
      # 4 columns
      if (ncol(data) == 4) {
        return(data.frame(times, values, imputed, original_times))
      }
    }
  }
}

################################################################################
# cook_raw_dataframe()
################################################################################

cook_raw_dataframe <- function(raw_df, from_date, to_date, dset_key, filename=NULL, metadata=NULL) {
  # List of samples per day (REMARK: ADD AS NEEDED!)
  spd <- get_samples_per_day()
  # Selection
  spd <- spd[[dset_key]]
  
  # Time series ends
  first_ts_date <- raw_df$times[1]
  last_ts_date <- raw_df$times[nrow(raw_df)] #utils::tail(raw_df, 1)[[1, 1]]
  
  # Check interval left end
  if (any(class(from_date) == "character")) {
    if (from_date == "first") {
      from_date <- first_ts_date
    }
  }
  # Check interval right end
  if (any(class(to_date) == "character")) {
    if (to_date == "last") {
      to_date <- last_ts_date
    }
  }
  
  # Adjust date limits to existing data
  if (from_date < last_ts_date & to_date > first_ts_date) {
    from_date <- max(from_date, first_ts_date)
    to_date   <- min(to_date, last_ts_date)
  } else {
    return(NULL)
  }
  
  # Chop raw_df
  raw_df <- raw_df[raw_df[,1] >= from_date & raw_df[,1] <= to_date,]
  
  # Create time sequence
  # time_seq <- seq(from_date, to_date, by=paste(86400 / spd, "sec"))
  # sum_factor <- cut(time_seq, breaks = "1 hour")

  # Round all dates towards zero according to the spd
  date_floors <- floor_date(raw_df$times, unit = paste(86400/(spd*60), "min"))
  # Aggregate according to the number of samples per day (spd)
  aggr_data <- aggregate(
    x   = raw_df$values,
    by  = list(times = date_floors),
    FUN = sum
  )
  # Create time sequence
  time_seq <- seq(
    from = min(date_floors),
    to = max(date_floors),
    by = paste(86400/(spd*60), "min")
  )
  # Complete data frame
  cooked_df <- as.data.frame(tidyr::complete(aggr_data, times=time_seq))
  colnames(cooked_df)<- c("times", "values")
  # cooked_df <- as.data.frame(raw_df %>% tidyr::complete(times=time_seq))
  
  # Get seasonal periods
  seasonal_periods <- NULL
  cooked_df_length <- dim(cooked_df)[1]
  # Days
  if (cooked_df_length > 2 * spd) {
    seasonal_periods <- c(seasonal_periods, spd)
  }
  # Weeks
  if (cooked_df_length > 2 * 7 * spd) {
    seasonal_periods <- c(seasonal_periods, 7 * spd)
  }
  # Years
  if (cooked_df_length > 2 * 365 * spd) {
    seasonal_periods <- c(seasonal_periods, 365 * spd)
  }
  
  # Number of NA
  number_of_na <- sum(is.na(cooked_df[,2]))
  # Check if all values are 0
  cooked_df_is_0 <- all(cooked_df[!is.na(cooked_df[,2]),2] == 0.0)
  
  # Common list
  common_list <- list(
    df               = cooked_df,
    dset_key         = dset_key,
    filename         = filename,
    seasonal_periods = seasonal_periods,
    number_of_na     = number_of_na,
    is_0             = cooked_df_is_0
  )
  return(common_list)
}

################################################################################
# impute_cooked_dataframe()
################################################################################

#' Imputed dataframe from cooked dataframe
#'
#' @description
#' Impute missing samples in a cooked dataframe. It can use an algorithm for short gaps (e.g. "interpolation") and another one for longer gaps (e.g. "locf").
#' 
#' @param cdf Cooked dataframe.
#' @param season Seasonal period (e.g. 1 week) in number of samples.
#' @param short_gap Number of samples considered as a short gap.
#' @param short_algorithm Algorithm used to impute short gaps.
#' @param long_algorithm Algorithm used to impute long gaps.
#'
#' @return Imputed dataframe, i.e. a cooked dataframe with a 3rd column indicating if each sample has been imputed or not.
#'
#' @export

impute_cooked_dataframe <- function(cdf, season, short_gap, short_algorithm="interpolation", long_algorithm="locf") {
  # Initialize
  imp_ts <- NULL
  
  # Time series pending imputation
  not_imp_ts <- ts(data=cdf$df[,2], frequency=season) # 1 week
  
  # Imputed time series
  try(
    imp_ts <- imputeTS::na_seasplit(
      not_imp_ts, algorithm = short_algorithm, maxgap = short_gap
    ),
    silent=TRUE
  )
  try(
    imp_ts <- imputeTS::na_seasplit(
      imp_ts, algorithm = long_algorithm
    ),
    silent=TRUE
  )
  
  if(!is.null(imp_ts)) {
    # Imputed dataframe
    cdf$df <- data.frame(
      times   = cdf$df[,1],
      values  = as.double(imp_ts),
      imputed = as.integer(is.na(not_imp_ts))
    )
  } else {
    cdf <- NULL
  }
  return(cdf)
}

################################################################################
# extend_dataset_v2()
################################################################################

extend_dataset_v2 <- function(
  input_folder, output_folder, dset_key, metadata_files=NULL,
  from_date="first", to_date="last", working_with_generation=FALSE, min_years = 1
  ) {
  
  # Get list of filenames in dataset folder
  dset_filenames <- list.files(input_folder, pattern = "*.csv")
  # Extract relevant data from metadata files (if any!)
  if (!is.null(metadata_files)) {
    # Load metadata dataframes into a big list
    metadata_dataframes <- lapply(
      metadata_files,
      data.table::fread,
      header     = TRUE,
      sep        = ",",
      na.strings = "",
      encoding   = "UTF-8"
    )
  }
  
  # Setup parallel backend to use many processors
  cores <- parallel::detectCores() - 1
  cl <- parallel::makeCluster(cores, outfile = "")
  doParallel::registerDoParallel(cl)
  
  # Progress bar
  pb <- txtProgressBar(style=3)
  # fnames length
  length_fnames <- length(dset_filenames)
  #print(length_fnames)
  
  # Analysis loop
  packages <- c("tidyr", "lubridate")
  export <- c(
    "extract_metadata",
    "get_raw_dataframe_from_dataset",
    "cook_raw_dataframe",
    "get_samples_per_day",
    "impute_cooked_dataframe"
  )
  
  out <- foreach::foreach (
    x = 1:length_fnames, .packages = packages, .export = export) %dopar% {
  #for(x in 1:length_fnames) {
    # Set progress bar
    setTxtProgressBar(pb, x/length_fnames)
    
    # File name selection
    dset_filename <- dset_filenames[x]
    # Load raw dataframe from dataset and impute
    file_path <- paste0(input_folder, dset_filename)
    rdf <- get_raw_dataframe_from_dataset(file_path)
    # GOIENER DATASETS ONLY
    if (!working_with_generation) {
      rdf <- rdf[,1:2]
    } else {
      rdf <- rdf[,c(1,3)]
      names(rdf) <- c("times", "values")
    }
	### HERE IS THE tidyr::complete() FUNCTION (Complete gaps with NAs)
    cdf <- cook_raw_dataframe(
      raw_df    = rdf,
      from_date = from_date, 
      to_date   = to_date, 
      dset_key  = dset_key, 
      filename  = dset_filename
    )
    # If cdf is NULL OR samples are NOT equidistributed in time, SKIP
    if (!is.null(cdf) & length(table(diff(cdf$df$times))) == 1) {
      # Get length
      initial_date   <- cdf$df[1,1]
      final_date     <- cdf$df[nrow(cdf$df),1]
      # length_in_days <- as.numeric(final_date - initial_date)
      length_in_years <- 
        lubridate::interval(initial_date,final_date)/lubridate::years(1)
      # If TS is longer than min_years, impute; ELSE discard
      if (length_in_years >= min_years) {
        edf <- impute_cooked_dataframe(
          cdf       = cdf, 
          season    = cdf$seasonal_periods[1] * 7, 
          short_gap = cdf$seasonal_periods[1] / 3
        )
        if (!is.null(edf)) {
          # Save dataframe in output folder
          path <- paste0(
            output_folder, strsplit(dset_filename, ".csv")[[1]], ".RData"
          )
          # Extract metadata
          if (!is.null(metadata_files)) {
            metadata_list <- extract_metadata(
              edf      = edf,
              dfs      = metadata_dataframes,
              dset_key = dset_key,
              filename = dset_filename
            )
          } else {
            metadata_list <- NULL
          }
          # Append metadata
          edf <- append(edf, metadata_list)
          
          save(edf, file=path)
        }
      }
    }
  }
  # Stop parallelization
  parallel::stopCluster(cl)
  
  cat("\n")
}

### USER DEFINED VARIABLES

## Metadata files
metadata_goi <- "C:/Users/carlos.quesada/Documents/WHY/2022.02.01 - Corrigiendo goiener-ext-3.R/old/Contratos_goi_meg_cor_20211222.csv"
metadata_lcl <- "G:/Mi unidad/WHY/Datos (raw)/Low Carbon London/informations_households.csv"
metadata_iss <- "G:/Mi unidad/WHY/Datos (raw)/ISSDA/CER Electricity Revised March 2012/CER_Electricity_Documentation/allocations.csv"
metadata_nee <- "G:/Mi unidad/WHY/Datos (raw)/NEEA/sites.csv"

#Function call for "goi"
extend_dataset_v2(
  input_folder = "C:/Users/carlos.quesada/Documents/WHY/2022.02.01 - Corrigiendo goiener-ext-3.R/", #"G:/Mi unidad/WHY/Datasets/nee/raw/",
  output_folder = "C:/Users/carlos.quesada/Documents/WHY/2022.02.01 - Corrigiendo goiener-ext-3.R/",
  dset_key = "goi",
  metadata_files = metadata_goi,
  working_with_generation = FALSE,
  min_years = 1,
  from_date = "first",
  to_date = "last"
)