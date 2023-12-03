#'@export

plot.ryx <- function(x) {
  if(!inherits(x, "ryx")) stop("Must be class ryx")
  suppressMessages(require(ggplot2))

  df <- x$df
  df$sign <- ifelse(df$r >= 0, "positive", "negative")
  df$absr <- abs(df$r)
  df$variable <- factor(df$variable, levels = df$variable[order(df$absr)])

  ggplot(df, aes(x = absr, y = variable, absr)) +
    geom_point(aes(color = sign), size = 3) +
    geom_segment(aes(x = 0, xend = absr, yend = variable), color = "lightgray") +
    labs(title = paste0("Correlations with ", x$y), x = "Correlation (absolute value)", y = "Variables",
         color = "Direction") +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
    scale_color_manual(values = c("red", "blue")) +
    theme_bw() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(linetype = 2),
          panel.grid.minor.x = element_blank())
}
