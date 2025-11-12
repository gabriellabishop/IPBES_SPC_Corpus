format_count <- function(x) {
  ifelse(
    is.na(x),
    "NA",
    format(x, big.mark = ",", trim = TRUE)
  )
}
