
# rxy

<img src="scatter.png" width="200"/>

<!-- badges: start -->
<!-- badges: end -->

The goal of rxy is to facilitate the exploration of correlations within a dataset.

## Installation

You can install the development version of rxy from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("edvintranhoac/rxy")
```

## Example

Here is an example of using the ryx function and then explorting the results using helper functions:

``` r
library(rxy)
# Correlate mpg with the rest of the variables in mtcars
x <- ryx(mtcars, y = "mpg")

# Print results
print(x)

# Summarize results in words
summary(x)

# Plot results
plot(x)
```

