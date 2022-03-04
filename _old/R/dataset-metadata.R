################################################################################
# get_samples_per_day
################################################################################

#' List of samples per day per dataset
#'
#' @description
#' Get a list of samples per day per dataset.
#'
#' @return A list of samples per day per dataset.
#'
#' @export

get_samples_per_day <- function() {
  list(
    goi = 24, 
    go2 = 24,
    go3 = 24,
    go4 = 24,
    iss = 48, 
    lcl = 48, 
    nee = 96,
    por = 96,
    ref = 24,
    go3_pre = 24,
    go3_pst = 24,
    go3_p20 = 24,
    go3_P21 = 24,
    go4_pre = 24,
    go4_pst = 24,
    go4_p20 = 24,
    go4_P21 = 24
  )
}

################################################################################
# get_dataset_dependent_metadata
################################################################################

#' Assign metadata to a list
#'
#' @description
#' Extract metadata from files.
#'
#' @param dset_key Dataset key: \code{lcl}, \code{goi}, etc.
#' @param metadata A dataframe of metadata.
#'
#' @return A list of metadata.
#'
#' @export

get_dataset_dependent_metadata <- function(dset_key, metadata) {
  # Particular list  
  if (dset_key == "lcl") {
    dset_list <- list(
      acorn         = metadata[[2]],
      acorn_grouped = metadata[[3]]
    )
  }
  if (dset_key %in%
      c("goi", "go2", "go3", "go3_pre", "go3_pst", "go3_p20", "go3_p21", "go4", "go4_pre", "go4_pst", "go4_p20", "go4_p21")) {
    dset_list <- metadata
    # dset_list <- list(
    #   cups         = metadata[[1]],      
    #   start_date   = metadata[[2]],
    #   end_date     = metadata[[3]],      
    #   tariff       = metadata[[4]],
    #   p1_kw        = metadata[[5]],      
    #   p2_kw        = metadata[[6]],
    #   p3_kw        = metadata[[7]],      
    #   self_consump = metadata[[8]],
    #   province     = metadata[[9]],      
    #   municipality = metadata[[10]],
    #   zip_code     = metadata[[11]],     
    #   cnae         = metadata[[12]]
    # )
  }
  if (dset_key == "iss") {
    dset_list <- list(
      id         = metadata[[2]],
      code       = metadata[[3]],
      res_stimul = metadata[[4]],
      res_tariff = metadata[[5]]
    )
  }
  if (dset_key == "nee") {
    dset_list <- list(
      id            = metadata[[1]],
      site_id       = metadata[[2]],
      state         = metadata[[3]],
      hz            = metadata[[4]],
      cz            = metadata[[5]],
      tz_abbr       = metadata[[6]],
      tz_utc_offset = metadata[[7]],
      station_id    = metadata[[8]]
    )
  }
  if (dset_key %in% c("ref", "por")) {
    dset_list <- list()
  }
  return(dset_list)
}

################################################################################
# extract_metadata
################################################################################

#' Ancillary function to extract metadata
#'
#' @description
#' Extract metadata from files.
#'
#' @param dfs A list of dataframes with metadata info. 
#' @param dset_key Dataset key: \code{lcl}, \code{goi}, etc.
#' @param filename A string with the file name.
#'
#' @return A list of metadata.
#'
#' @export

extract_metadata <- function(dfs, dset_key, filename) {
  # Low Carbon London
  if (dset_key == "lcl") {
    acorn_tag     <- strsplit(filename, ".csv")[[1]]
    acorn         <- dfs[[1]][dfs[[1]][,1] == acorn_tag, 3]
    acorn_grouped <- dfs[[1]][dfs[[1]][,1] == acorn_tag, 4]
    
    return(list(acorn_tag, acorn, acorn_grouped))
  }
  
  # Goiener
  if (dset_key %in%
      c("goi", "go2", "go3", "go3_pre", "go3_pst", "go3_p20", "go3_p21", "go4", "go4_pre", "go4_pst", "go4_p20", "go4_p21")) {
    out <- list()
    
    # Identify current user
    cups <- strsplit(filename, ".csv")[[1]]
    out[["cups"]] <- cups
    
    if("cups_ref" %in% colnames(dfs[[1]])) {
      all_idx <- which(dfs[[1]]$cups_ref == cups)
      idx <- max(all_idx)
      out[["mdata_file_idx"]] <- idx
    }
    if("fecha_alta" %in% colnames(dfs[[1]])) {
      start_date <- min(lubridate::ymd(dfs[[1]]$fecha_alta[all_idx]))
      out[["start_date"]] <- start_date
    }
    if("fecha_baja" %in% colnames(dfs[[1]])) {
      end_date <- max(lubridate::ymd(dfs[[1]]$fecha_baja[all_idx]))
      out[["end_date"]] <- end_date
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
      out[["province"]] <- province
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
  
    # # There may be several entries for a cups: get index (highest date)
    # idx <- which.max(as.POSIXct(dfs[[1]][dfs[[1]][,1] == cups, 2]))
    # # Get all data
    # start_date   <- as.POSIXct(dfs[[1]][dfs[[1]][,1] == cups,  2][idx], tz="GMT")
    # end_date     <- as.POSIXct(dfs[[1]][dfs[[1]][,1] == cups,  3][idx], tz="GMT")
    # tariff       <-            dfs[[1]][dfs[[1]][,1] == cups,  4][idx]
    # p1_kw        <- as.numeric(dfs[[1]][dfs[[1]][,1] == cups,  5][idx])
    # p2_kw        <- as.numeric(dfs[[1]][dfs[[1]][,1] == cups,  6][idx])
    # p3_kw        <- as.numeric(dfs[[1]][dfs[[1]][,1] == cups,  7][idx])
    # self_consump <-            dfs[[1]][dfs[[1]][,1] == cups,  8][idx]
    # province     <-            dfs[[1]][dfs[[1]][,1] == cups,  9][idx]
    # municipality <-            dfs[[1]][dfs[[1]][,1] == cups, 10][idx]
    # zip_code     <- as.numeric(dfs[[1]][dfs[[1]][,1] == cups, 11][idx])
    # cnae         <- as.numeric(dfs[[1]][dfs[[1]][,1] == cups, 12][idx])
    
    # return(list(cups, start_date, end_date, tariff, p1_kw, p2_kw, p3_kw,
                # self_consump, province, municipality, zip_code, cnae))
    
    return(out)
  }
  
  # ISSDA
  if (dset_key == "iss") {
    id         <- strsplit(filename, ".csv")[[1]]
    code       <- as.numeric(dfs[[1]][dfs[[1]][,1] == id, 2][1])
    res_stimul <-            dfs[[1]][dfs[[1]][,1] == id, 3][1]
    res_tariff <-            dfs[[1]][dfs[[1]][,1] == id, 4][1]
    sme_alloc  <-            dfs[[1]][dfs[[1]][,1] == id, 5][1]
    
    return(list(id, code, res_stimul, res_tariff, sme_alloc))
  }
  
  # NEEA
  if (dset_key == "nee") {
    # Get index from filename
    id <- as.numeric(strsplit(filename, "-Mains.csv")[[1]])
    idx <- which(dfs[[1]]$ee_site_id == id)
    # Get metadata
    site_id       <- dfs[[1]]$site_id[idx]
    state         <- dfs[[1]]$state[idx]
    hz            <- dfs[[1]]$hz[idx]
    cz            <- dfs[[1]]$cz[idx]
    tz_abbr       <- dfs[[1]]$tz_abbr[idx]
    tz_utc_offset <- dfs[[1]]$tz_utc_offset[idx]
    station_id    <- dfs[[1]]$station_id[idx]
    
    return(list(id, site_id, state, hz, cz, tz_abbr, tz_utc_offset, station_id))
  }
  
  # REFIT
  if (dset_key == "ref" | dset_key == "por") {
    return(NULL)
  }
}
