# ADDITION OF ISSDA SURVEY AS FEATURES

library(purrr)
library(dplyr)

issda_survey_addition <- function() {
  # Path to survey
  issda_survey_file <- "G:/Mi unidad/WHY/Datos (raw)/ISSDA/38_CER Electricity_Gas/CER Electricity Revised March 2012/CER_Electricity_Data/Survey data - CSV format/Smart meters Residential pre-trial survey data.csv"
  # Path to features
  feats_path <- "G:/Mi unidad/WHY/Features/feats_v1.17.csv"
  # Output path
  output_path <- "G:/Mi unidad/WHY/Features/feats_v1.18.csv"
  
  # Load feats
  questions <- data.table::fread(
    file   = issda_survey_file,
    header = TRUE,
    sep    = ","
  )
  
  # Managing names
  q_names <- strsplit(names(questions), ":")
  q_names <- purrr::map(q_names, 1)
  q_names <- sub('Question ', '', q_names)
  q_names <- c("file", paste0("q_", q_names[2:length(q_names)]))
  q_names <- make.unique(q_names)
  
  names(questions) <- q_names
  questions$file <- as.character(questions$file)
  
  # Load feats
  feats <- data.table::fread(
    file   = feats_path,
    header = TRUE,
    sep    = ","
  )
  
  # Merge
  new_feats <- feats %>% dplyr::left_join(questions, by="file")
  
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

issda_survey_addition()