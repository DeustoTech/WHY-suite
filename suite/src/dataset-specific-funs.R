################################################################################
##  CARLOS QUESADA GRANJA
##  MARCH 23, 2022
##  UNIVERSIDAD DE DEUSTO
##  ---------------------
##  DATASET-SPECIFIC FUNCTIONS:
##  FUNCTIONS TO ADAPT WHEN A NEW DATASET COMES TO TOWN
##  Extracted from "goiener-ext-3v3.R"
##  ---------------------
##  1) get_samples_per_day() -> Add the number of samples per day
##  2) manage_times() --------> Correct time offsets
##  3) extract_metadata() ----> Do whatever possible to incorporate metafeatures
################################################################################

################################################################################
# get_samples_per_day()
################################################################################

get_samples_per_day <- function() {
  list(
    edrp = 48,
    goi  = 24, 
    iss  = 48, 
    lcl  = 48, 
    nee  = 96,
    por  = 96,
    ref  = 24
  )
}

################################################################################
# manage_times()
# CALLABLE FUNCTIONS:
#   correct_dst()
#   correct_tz()
################################################################################

manage_times <- function(edf) {
  
  # EDRP & LCL 
  if (edf$dset_key %in% c("lcl", "edrp")) {
    edf <- correct_dst(edf, "Europe/London")
  }
  
  # NEE 
  if (edf$dset_key == "nee") {
    # Correct DST (daylight saving time)
    if (edf$tz_abbr == "PST") tzone <- "US/Pacific"
    if (edf$tz_abbr == "MST") tzone <- "US/Mountain"
    edf <- correct_dst(edf, tzone)
    
    # # Correct TZ (time zone)
    # edf <- correct_tz(edf, edf$tz_utc_offset)
  }
  
  return(edf)
}

################################################################################
# extract_metadata_iss()
################################################################################

extract_metadata_iss <- function(out) {
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

  return(out)
}

################################################################################
# extract_metadata_lcl()
################################################################################

extract_metadata_lcl <- function(out) {
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
  
  return(out)
}

################################################################################
# extract_metadata_nee()
################################################################################

extract_metadata_nee <- function(out) {
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
  
  return(out)
}

################################################################################
# extract_metadata_goi()
################################################################################

extract_metadata_goi <- function(out) {
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
  
  return(out)
}

################################################################################
# extract_metadata_por()
################################################################################

extract_metadata_por <- function(out) {
  # Processed metadata
  out[["mdata_file_idx"]] <- 0
  out[["country"]] <- "pt"
  out[["is_household"]] <- NA
  
  return(out)
}

################################################################################
# extract_metadata_edrp()
################################################################################

extract_metadata_edrp <- function(out) {
  # Processed metadata
  out[["mdata_file_idx"]] <- 0
  out[["country"]] <- "gb"
  out[["is_household"]] <- NA
  
  return(out)
}

################################################################################
# extract_metadata()
################################################################################

extract_metadata <- function(edf, dfs, dset_key, filename) {
  # Initialize the output list
  out <- list()
  
  # "filename" -> name of the file with the extension
  # "file"     -> name of the file without the extension
  # "fname"    -> name used to locate the proper metadata entry
  
  # Common metadata (depending on the data.frame) ##############################
  out[["file"]]           <- strsplit(edf$filename, ".csv")[[1]]
  out[["data_set"]]       <- edf$dset_key
  out[["num_of_samples"]] <- length(edf$df$values)
  out[["ts_start_date"]]  <- edf$df$times[1]
  out[["ts_end_date"]]    <- edf$df$times[nrow(edf$df)]
  out[["ts_days"]]        <- as.numeric(edf$df$times[nrow(edf$df)] - edf$df$times[1])
  out[["abs_imputed_na"]] <- edf$number_of_na
  out[["rel_imputed_na"]] <- edf$number_of_na / length(edf$df$values)
  
  # ADD NEW DATASETS HERE!
  if (dset_key == "lcl")  out <- extract_metadata_lcl(out)
  if (dset_key == "iss")  out <- extract_metadata_iss(out)
  if (dset_key == "nee")  out <- extract_metadata_nee(out)
  if (dset_key == "goi")  out <- extract_metadata_goi(out)
  if (dset_key == "por")  out <- extract_metadata_por(out)
  if (dset_key == "edrp") out <- extract_metadata_edrp(out)
  
  return(out)
}
