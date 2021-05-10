# ADDITION OF ISSDA SURVEY AS FEATURES

library(purrr)

issda_survey_addition <- function() {
  # Path to survey
  issda_survey_file <- "G:/Mi unidad/WHY/Datos (raw)/ISSDA/38_CER Electricity_Gas/CER Electricity Revised March 2012/CER_Electricity_Data/Survey data - CSV format/Smart meters Residential pre-trial survey data.csv"
  
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
  
  browser()
}

issda_survey_addition()