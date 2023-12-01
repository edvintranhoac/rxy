#' Compute, summarize, and visualize correlations between variables
#'
#' Use this package to calculate correlations between specified variables. It computes correlation coefficients,
#' summarizes the results, and generates plots for visualization.
#'
#'@param data A data frame
#'@param y A numeric variable
#'@param x One ore more numeric variables
#'@return Results in a list
#'@import ggplot2
#'@importFrom stats cor.test median
#'@export

#'@examples

#' # Correlate mpg with the rest of the variables in mtcars
#'
#' library(rxy)
#' library(MASS)
#'
#' x <- ryx(Boston, y = "medv")
#'
#' # Print results
#' print(x)
#'
#' # Summarize results in words
#' summary(x)
#'
#' # Plot results
#' plot(x)

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



