summary <- function(x) {
  medcorr <- round(median(x$df$r), 3)
  mincorr <- round(min(x$df$r), 3)
  maxcorr <- round(max(x$df$r), 3)
  count_insig <- sum(x$df$sigif == " ")
  n_x <- length(x$df$variable)

  cat("Correlating", x$y, "with", paste0(x$x),
      "\nThe median absolute correlation was", medcorr, "with a range from",
      mincorr, "to", maxcorr, "\n", n_x - count_insig, "out of", n_x,
      "variables were significant at the p < 0.05 level.")
}
