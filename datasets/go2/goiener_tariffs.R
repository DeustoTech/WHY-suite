# ADDITION OF ISSDA SURVEY AS FEATURES

library(purrr)
library(dplyr)

goiener_survey_addition <- function() {
  # Path to survey
  goiener_survey_file <- c(
    "G:/Mi unidad/WHY/Datos (raw)/GOIENER/Contratos_Goiener_20210301_anonymized.csv",
    "G:/Mi unidad/WHY/Datos (raw)/MEGARA/Contratos_Megara_20210301_anonymized.csv"
  )
  # Path to features
  feats_path <- "G:/Mi unidad/WHY/Features/feats_v1.14.csv"
  # Output path
  output_path <- "G:/Mi unidad/WHY/Features/feats_v1.15.csv"
  
  tariff_df <- data.frame()
  
  datasets <- c("go2", "meg")
  for (ii in 1:2) {
    # Load feats
    metadata <- data.table::fread(
      file   = goiener_survey_file[ii],
      header = TRUE,
      sep    = ","
    )
    
    metadata <- metadata[with(metadata, order(cups_ref, fecha_alta))]
    unique_cups <- unique(metadata$cups_ref)
    
    for (uu in unique_cups) {
      idx <- max(which(metadata$cups_ref == uu))
      tariff_df <- rbind(tariff_df, data.frame(
        file   = metadata$cups_ref[idx],
        data_set = datasets[ii],
        tariff = metadata$tarifa.tarifa_atr_ref[idx],
        p1_kw  = metadata$p1_kw[idx],
        p2_kw  = metadata$p2_kw[idx],
        p3_kw  = metadata$p3_kw[idx],
        self_consumption_type = metadata$tipo_autoconsumo_ref[idx]
      ))
    }
  }
  
  # Load feats
  feats <- data.table::fread(
    file   = feats_path,
    header = TRUE,
    sep    = ","
  )
  
  browser()
  
  # Merge
  new_feats <- feats %>% dplyr::left_join(tariff_df, by=c("file", "data_set"))
  
  data.table::fwrite(
    x         = new_feats,
    file      = output_path,
    append    = F,
    quote     = F,
    sep       = ",",
    row.names = F,
    col.names = T,
    dateTimeAs = "write.csv"
  )
}

goiener_survey_addition()