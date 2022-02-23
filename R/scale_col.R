#' @title Scale a column in a tibble
#'
#' @description Function to z-score a column, ignoring NA values
#'
#' @param x numeric: data to z_score
#'
#' @details
#' scale_col is a convenience function for calculating the z-scored
#' values in a column
#'
#' It's faster than: (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
#'
#' @return numeric
#' @docType package
#' @name scale_col
#' @examples
#' d <- tibble(x = c(rnorm(10), NA))
#' d %>% mutate(dz = scale_col(x))
#' A tibble: 11 x 2
#'        x      dz
#'    <dbl>   <dbl>
#' 1 -1.90   -1.87
#' 2  0.135   0.336
#' 3 -1.10   -1.00
#' 4 -0.0678  0.116
#' 5 -0.113   0.0666
#' 6  0.594   0.833
#' 7 -0.771  -0.646
#' 8  1.05    1.33
#' 9  0.908   1.17
#' 10 -0.485  -0.337
#' 11 NA      NA
#' @export

scale_col <- function(x) {
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm = TRUE)
}

