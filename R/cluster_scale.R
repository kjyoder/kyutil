#' @title Perform cluster mean centering and convert to cluster-specific z-scores
#'
#' @description For the given data frame,
#' calculate the mean of the given column ('x')
#' within each cluster defined by column 'g'.
#'
#' A new data.frame is returned with two additional columns,
#'   *_m the mean of x within each cluster
#'   *_z the z-scored values of x
#'
#' @param df data.frame containing the variables
#' @param x char; label of the column to cluster mean center
#' @param g char; label of the column with cluster ids
#' @param label char; to use for new columns (default=x)
#' @param clobber logical; to control behavior when a column <label>_m or <label>_z already exists. TRUE: overwrite existing column. FALSE: exit function (default=FALSE)
#'
#' @return data.frame,tibble
#' @docType package
#' @name cluster_scale
#' @examples
#' df <- tibble(x=rnorm(10), g=rep(c(0,1),5))
#' df <- cluster_scale(df, x, g)
#' df
#' # A tibble: 10 x 4
#'       x     g   x_m    x_z
#'   <dbl> <dbl> <dbl>  <dbl>
#' 1 -0.680     0 0.260 -1.10
#' 2  0.927     1 0.350  0.484
#' 3 -0.590     0 0.260 -0.993
#' 4  1.44      1 0.350  0.912
#' 5  1.10      0 0.260  0.984
#' 6 -0.625     1 0.350 -0.817
#' 7  0.447     0 0.260  0.218
#' 8 -1.22      1 0.350 -1.31
#' 9  1.02      0 0.260  0.889
#'10  1.23      1 0.350  0.735
#'
#' @export
cluster_scale <- function(df, x, g, label, clobber=FALSE) {
  require(dplyr)
  require(stringr)
  require(rlang)
  # set up new column names
  x <- enquo(x)
  g <- enquo(g)
  if (is_missing(label)) {
    label <- get_expr(x)
  }
  m_col = str_c(label, '_m')
  z_col = str_c(label, "_z")
  new_cols = c('mreservedkey', 'zreservedkey')
  names(new_cols) <- c(m_col, z_col)

  # check for pre-existing columns to prevent accidental clobbering
  for (col in names(new_cols)) {
    if (col %in% names(df)) {
      if (clobber) {
        warning(str_c('Replacing column: ', col))
        df <- df %>% dplyr::select(-col)
      } else {
        stop(str_c("Created column [",col,"] already exists. Set clobber=TRUE to overwrite"))
      }
    }
  }
  # calculate cluster means
  dm <- df %>%
    dplyr::select(!!g, !!x) %>%
    group_by(!!g) %>%
    summarize(mreservedkey = mean(!!x, na.rm=TRUE),
              reservedsd = sd(!!x, na.rm=TRUE))
  # add means to df, calculate cluster-centered values, then rename new columns
  d <- df %>%
    left_join(dm, by=as_name(g)) %>%
    mutate(zreservedkey = (!!x - mreservedkey)/reservedsd) %>%
    select(-reservedsd) %>%
    rename(!!new_cols)

  return(d)
}
