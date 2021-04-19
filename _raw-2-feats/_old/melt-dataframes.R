# f_in <- c(
#   "C:/Users/carlos.quesada/Documents/features/goiener_20.12.15/feats-29943.csv",
#   "C:/Users/carlos.quesada/Documents/features/goiener_20.12.15/feats-29944.csv",
#   "C:/Users/carlos.quesada/Documents/features/goiener_20.12.15/feats-29945.csv",
#   "C:/Users/carlos.quesada/Documents/features/goiener_20.12.16/feats-16644.csv",
#   "C:/Users/carlos.quesada/Documents/features/goiener_20.12.16/feats-16645.csv",
#   "C:/Users/carlos.quesada/Documents/features/goiener_20.12.16/feats-16646.csv",
#   "C:/Users/carlos.quesada/Documents/features/goiener_21.01.04/feats-13475.csv",
#   "C:/Users/carlos.quesada/Documents/features/goiener_21.01.04/feats-13476.csv",
#   "C:/Users/carlos.quesada/Documents/features/goiener_21.01.04/feats-13477.csv"
# )
# f_out <- "C:/Users/carlos.quesada/Documents/features/goiener.csv"

# f_in <- c(
#   "C:/Users/carlos.quesada/Documents/features/lcl_20.12.15/feats-14410.csv",
#   "C:/Users/carlos.quesada/Documents/features/lcl_20.12.15/feats-14411.csv",
#   "C:/Users/carlos.quesada/Documents/features/lcl_20.12.15/feats-14412.csv"
# )
# f_out <- "C:/Users/carlos.quesada/Documents/features/lcl.csv"

f_in <- c(
  "C:/Users/carlos.quesada/Documents/features/issda_21.01.12/feats-11680.csv",
  "C:/Users/carlos.quesada/Documents/features/issda_21.01.12/feats-11681.csv",
  "C:/Users/carlos.quesada/Documents/features/issda_21.01.12/feats-11682.csv"
)
f_out <- "C:/Users/carlos.quesada/Documents/features/issda.csv"

d_f_all <- data.frame()

for (ii in 1:length(f_in)) {
  # Read file
  d_f <- data.table::fread(
    file       = f_in[ii],
    header     = TRUE,
    sep        = ",",
    na.strings = ""
  )
  # Merge dataframes
  d_f_all <- rbind(d_f_all, d_f)
}

# Save dataframe
data.table::fwrite(
  d_f_all,
  file       = f_out,
  row.names  = FALSE,
  col.names  = TRUE,
  sep        = ",",
  na         = "")

browser()