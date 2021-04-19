    # File path to big CSV
    feats_path <- "/home/ubuntu/carlos.quesada/disk/features/feats_v1.0.csv"
    # Output path
    outpt_path <- "/home/ubuntu/carlos.quesada/disk/features/metadata.csv"
    # File path to Goiener 
    goien_path <- "/home/ubuntu/carlos.quesada/disk/goiener/ext-bug-corr-rep/"
    # File path to ISSDA 
    issda_path <- "/home/ubuntu/carlos.quesada/disk/issda/ext/"
    # File path to Low Carbon London 
    loclo_path <- "/home/ubuntu/carlos.quesada/disk/lcl/ext/"
    # File path to REFIT 
    refit_path <- "/home/ubuntu/carlos.quesada/disk/refit/ext/"
    
    # Load feats
    feats <- data.table::fread(
      file   = feats_path,
      header = TRUE,
      sep    = ","
    )
    
    # FUNCTION TO LOAD METADATA FROM RDATA FILES
    load_metadata <- function(input_df) {
      ## INPUTS
      file_name <- input_df[[1]]
      data_set  <- input_df[[2]]
      ## OUTPUT
      o <- list()
      
      ## LOAD FILE
      # Goiener data set
      if (data_set == "goi") {
        load(paste(goien_path, file_name, ".RData", sep=""))
      }
      # ISSDA data set
      if (data_set == "iss") {
        load(paste(issda_path, file_name, ".RData", sep=""))
      }
      # Low Carbon London data set
      if (data_set == "lcl") {
        load(paste(loclo_path, file_name, ".RData", sep=""))
      }
      # REFIT data set
      if (data_set == "ref") {
        load(paste(refit_path, file_name, ".RData", sep=""))
      }
      
      ## COMMON METADATA
      # file
      o$file <- file_name
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
      if (data_set == "goi") {
        # country
        o$country <- "es"
        # administrative_division
        o$administrative_division <-
          stringr::str_replace(edf$province, ",", ";")
        # municipality
        o$municipality <-
          stringr::str_replace(edf$municipality, ",", ";")
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
      
      ## REFIT-SPECIFIC METADATA
      if (data_set == "ref") {
        # country
        o$country <- "gb"
        # administrative_division
        o$administrative_division <- NA
        # municipality
        o$municipality <- "Loughborough"
        # zip_code
        o$zip_code <- NA
        ## Spatial resolution
        o$is_household <- 1
        # cnae
        o$cnae <- NA
        # acorn
        o$acorn <- NA
        # acorn_grouped
        o$acorn_grouped <- NA
      }
      
      return(o)
    }
    
    # Generate table  
    x <- do.call(dplyr::bind_rows, apply(feats[,1:2], 1, load_metadata))
    # Save
    data.table::fwrite(
      x         = x,
      file      = outpt_path,
      append    = F,
      quote     = F,
      sep       = ",",
      row.names = F,
      col.names = T,
      dateTimeAs = "write.csv"
    )
