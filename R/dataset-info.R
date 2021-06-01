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
    iss = 48, 
    lcl = 48, 
    nee = 96,
    por = 96,
    ref = 24 
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
  if (dset_key %in% c("goi", "go2")) {
    dset_list <- list(
      cups         = metadata[[1]],      
      start_date   = metadata[[2]],
      end_date     = metadata[[3]],      
      tariff       = metadata[[4]],
      p1_kw        = metadata[[5]],      
      p2_kw        = metadata[[6]],
      p3_kw        = metadata[[7]],      
      self_consump = metadata[[8]],
      province     = metadata[[9]],      
      municipality = metadata[[10]],
      zip_code     = metadata[[11]],     
      cnae         = metadata[[12]]
    )
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
  if (dset_key %in% c("goi", "go2")) {
    # Identify current user
    cups <- strsplit(filename, ".csv")[[1]]
    # There may be several entries for a cups: get index (highest date)
    idx <- which.max(as.POSIXct(dfs[[1]][dfs[[1]][,1] == cups, 2]))
    # Get all data
    start_date   <- as.POSIXct(dfs[[1]][dfs[[1]][,1] == cups,  2][idx], tz="GMT")
    end_date     <- as.POSIXct(dfs[[1]][dfs[[1]][,1] == cups,  3][idx], tz="GMT")
    tariff       <-            dfs[[1]][dfs[[1]][,1] == cups,  4][idx]
    p1_kw        <- as.numeric(dfs[[1]][dfs[[1]][,1] == cups,  5][idx])
    p2_kw        <- as.numeric(dfs[[1]][dfs[[1]][,1] == cups,  6][idx])
    p3_kw        <- as.numeric(dfs[[1]][dfs[[1]][,1] == cups,  7][idx])
    self_consump <-            dfs[[1]][dfs[[1]][,1] == cups,  8][idx]
    province     <-            dfs[[1]][dfs[[1]][,1] == cups,  9][idx]
    municipality <-            dfs[[1]][dfs[[1]][,1] == cups, 10][idx]
    zip_code     <- as.numeric(dfs[[1]][dfs[[1]][,1] == cups, 11][idx])
    cnae         <- as.numeric(dfs[[1]][dfs[[1]][,1] == cups, 12][idx])
    
    return(list(cups, start_date, end_date, tariff, p1_kw, p2_kw, p3_kw,
                self_consump, province, municipality, zip_code, cnae))
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