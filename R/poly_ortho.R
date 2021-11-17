#' @title Calculate and return the i-th ordered orthogonal polynomial
#'
#' @description For the given vector, return the orthogonal
#' parameterization of the i-th ordered polynomial
#'
#'
#' @param x numeric: data from which to calculate polynomials
#' @param ord int: order of polynomial to extract (0 < ord < 5)
#'
#' @details
#' poly_ortho uses stats::poly to calculate orthogonal polynomials
#' of the given data. These are necessarily to examine these
#' non-linear effects in models while avoiding co-linearity
#' between lower order polynomials.
#'
#' The returned orthogonalized vectors may be useful for creating
#' design matrices in contexts were polynomials are desired, but the
#' poly function cannot be easily used (e.g. with rstan functions)
#'
#' @return numeric
#' @docType package
#' @name poly_ortho
#' @seealso \code{\link[stats]{poly}}
#' @examples
#' x <- sort(rnorm(5))
#' poly_ortho(x, 1)
#' [1] -0.45049407 -0.25381561 -0.09458044 -0.05031853  0.84920864
#' poly_ortho(x, 2)
#' [1] -0.34934124  0.81629373 -0.03666156 -0.45774186  0.02745094
#' poly_ortho(x, 3)
#' [1]  0.68735315 -0.02314875 -0.42889989 -0.51474832  0.27944380
#' @export
poly_ortho <- function(x, ord=2) {
  if (ord < 1)
    stop('Order must be at least 1')
  if (ord > 4)
    stop('Current implementation only goes to 4th order')
  z <- poly(x, ord)
  a <- attributes(z)$coefs$alpha
  n <- attributes(z)$coefs$norm2


  f0 <- 1 / sqrt(n[2])
  f1 <- (x-a[1]) / sqrt(n[3])
  if (ord == 1) return(f1)
  f2 <- ( (x-a[2]) * sqrt(n[3]) * f1 - n[3]/sqrt(n[2]) * f0) /
    sqrt(n[4])
  if (ord == 2) return(f2)
  f3 <- ( (x-a[3]) * sqrt(n[4]) * f2 - n[4]/sqrt(n[3]) * f1) /
    sqrt(n[5])
  if (ord == 3) return(f3)
  f4 <- ( (x-a[4]) * sqrt(n[5]) * f3 - n[5]/sqrt(n[4]) * f2) /
    sqrt(n[6])
  if (ord == 4) return(f4)
}
