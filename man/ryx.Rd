% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ryx.R
\name{ryx}
\alias{ryx}
\title{Compute, summarize, and visualize correlations between variables}
\usage{
ryx(data, y, x)
}
\arguments{
\item{data}{A data frame}

\item{y}{A numeric variable}

\item{x}{One or more numeric variables}
}
\value{
Results in a list
}
\description{
Use this package to calculate correlations between specified variables. It computes correlation coefficients,
summarizes the results, and generates plots for visualization.
}
\examples{
# Correlate mpg with the rest of the variables in the Boston dataset

library(rxy)
library(MASS)

x <- ryx(Boston, y = "medv")

# Print results
print(x)

# Summarize results in words
summary(x)

# Plot results
plot(x)
}
