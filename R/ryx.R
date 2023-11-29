

ryx <- function(data, y, x){
  if(missing(x)){
    x <- names(data)[sapply(data, class)=="numeric"]
    x <- setdiff(x, y)
  }
  df <- data.frame()
  for (var in x){
    res <- cor.test(data[[y]], data[[var]])
    df_temp <- data.frame(variable = var,
                          r = res$estimate,
                          p = res$p.value)
    df <- rbind(df, df_temp)
    df <- df[order(-abs(df$r)),]
  }

  df$sigif <- ifelse(df$p < .001, "***",
                     ifelse(df$p < .01, "**",
                            ifelse(df$p < .05, "*", " ")))
  results <- list(y=y, x=x, df=df)
  class(results) <- "ryx"
  return(results)
}




library(MASS)
x <- ryx(Boston, y="medv")

print <- function(x) {
  df <- x$df

  df$r <- round(df$r, 3)
  df$p <- signif(df$p, 3)

  cat(paste0("Correlations of ", x$y, " with\n"))
  base::print(df, row.names = FALSE)
}

print(x)


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

summary(x)

require(ggplot2)
df <- x$df
df$sign <- ifelse(df$r >= 0, "positive", "negative")
df$absr <- abs(df$r)
df$variable <- factor(df$variable, levels = df$variable[order(df$absr)])

ggplot(df, aes(x = absr, y = variable, absr)) +
  geom_segment(aes(x = 0, xend = absr, yend = variable)) +
  geom_point(aes(color = sign)) +
  labs(x = "Correlation (absolute value)", y = "Variables") +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  theme_bw()



