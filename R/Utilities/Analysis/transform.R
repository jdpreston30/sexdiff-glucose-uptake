log_trans <- function(tb) {
  tb %>%
    mutate(across(.cols = 4:ncol(tb), .fns = ~ log(.)))
}