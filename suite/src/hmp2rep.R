################################################################################
##  CARLOS QUESADA GRANJA
##  MARCH 2, 2022
##  UNIVERSIDAD DE DEUSTO
##  ---------------------
##  REPORTING FILE
################################################################################

# REPORTING FILES IN SUBFOLDER 'src'
hmp2crd_src <- "cards-report.Rmd"
hmp2sll_src <- "cards-report-scroll.Rmd"
hmp2m40_src <- "map40-report.R"
no_file_src <- "no-file.png"

####################
## REPORT MANAGER ##
####################
reporting <- function(
  rep_type,
  rep_title,
  clu_dir,
  rep_fname,
  ff,
  dd,
  mm,
  cc,
  new_hmp_fname,
  tag
) {
  ############
  ## SCROLL ##
  ############
  if ("scroll" %in% rep_type) {
    print("## RMARKDOWN SCROLL REPORT ##")
    params_list <- list(
      rmd_title     = rep_title,
      no_card_types = 33,
      hmp_dir       = paste0(clu_dir, "hmp/"),
      distr_dir     = paste0(clu_dir, "distr/"),
      acf_dir       = paste0(clu_dir, "acf/"),
      dplot_dir     = paste0(clu_dir, "dplot/"),
      nofile_path   = paste(getwd(), "src", no_file_src, sep="/"),
      ff            = ff,
      dd            = dd,
      mm            = mm,
      cc            = cc
    )
    rmarkdown::render(
      input       = paste(getwd(), "src", hmp2sll_src, sep="/"),
      output_file = paste0(clu_dir, "report/", rep_fname),
      params      = params_list
    )
  }
  
  ###########
  ## BASIC ##
  ###########
  if ("basic" %in% rep_type) {
    print("## RMARKDOWN BASIC REPORT ##")
    params_list <- list(
      rmd_title     = rep_title,
      no_card_types = 1,
      hmp_dir       = paste0(clu_dir, "hmp/"),
      hmm_dir       = paste0(clu_dir, "hmm/"),
      nofile_path   = paste(getwd(), "src", no_file_src, sep="/"),
      ff            = ff,
      dd            = dd,
      mm            = mm,
      cc            = cc
    )
    rmarkdown::render(
      input       = paste(getwd(), "src", hmp2crd_src, sep="/"),
      output_file = paste0(clu_dir, "report/", rep_fname),
      params      = params_list
    )
  }
  
  ################
  ## BASIC + SD ##
  ################
  if ("sd" %in% rep_type) {
    print("## RMARKDOWN BASIC + SD REPORT ##")
    params_list <- list(
      rmd_title     = rep_title,
      no_card_types = 2,
      hmp_dir       = paste0(clu_dir, "hmp/"),
      hmm_dir       = paste0(clu_dir, "hmm/"),
      hmpsd_dir     = paste0(clu_dir, "hmpsd/"),
      hmmsd_dir     = paste0(clu_dir, "hmmsd/"),
      nofile_path   = paste(getwd(), "src", no_file_src, sep="/"),
      ff            = ff,
      dd            = dd,
      mm            = mm,
      cc            = cc
    )
    rmarkdown::render(
      input       = paste(getwd(), "src", hmp2crd_src, sep="/"),
      output_file = paste0(clu_dir, "report/", rep_fname),
      params      = params_list
    )
  }
  
  ######################
  ## BASIC + SD + RSD ##
  ######################
  if ("rsd" %in% rep_type) {
    print("## RMARKDOWN BASIC + SD + RSD REPORT ##")
    params_list <- list(
      rmd_title     = rep_title,
      no_card_types = 3,
      hmp_dir       = paste0(clu_dir, "hmp/"),
      hmm_dir       = paste0(clu_dir, "hmm/"),
      hmpsd_dir     = paste0(clu_dir, "hmpsd/"),
      hmmsd_dir     = paste0(clu_dir, "hmmsd/"),
      hmprsd_dir    = paste0(clu_dir, "hmprsd/"),
      hmmrsd_dir    = paste0(clu_dir, "hmmrsd/"),
      nofile_path   = paste(getwd(), "src", no_file_src, sep="/"),
      ff            = ff,
      dd            = dd,
      mm            = mm,
      cc            = cc
    )
    rmarkdown::render(
      input       = paste(getwd(), "src", hmp2crd_src, sep="/"),
      output_file = paste0(clu_dir, "report/", rep_fname),
      params      = params_list
    )
  }
  
  ############
  ## MAP 40 ##
  ############
  if ("map40" %in% rep_type) {
    source(paste(getwd(), "src", hmp2m40_src, sep="/"))
    print("## MAP-TO-40 REPORT ##")
    map40(
      new_hmp_dir   = paste0(clu_dir, "hmm/"),
      all_hmp_dir   = "/home/ubuntu/carlos.quesada/analyses/clValid2/2021.06.08_km-som-var-cl/hmm/",
      new_hmp_fname = new_hmp_fname, #c("hmm_sAggrDRM_goi_som_30cl_i-", ".RData"),
      all_hmp_fname = c("hmm_175_", "-40.RData"),
      out_dir       = clu_dir,
      tag           = tag
    )
  }
}