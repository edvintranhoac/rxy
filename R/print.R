#'@export

print.ryx <- function(x) {
  if(!inherits(x, "ryx")) stop("Must be class ryx")

  df <- x$df

  df$r <- round(df$r, 3)
  df$p <- signif(df$p, 3)

  cat(paste0("Correlations of ", x$y, " with\n"))
  base::print(df, row.names = FALSE)
}
