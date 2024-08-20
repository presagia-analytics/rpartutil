#' @title Get the Error Diagnostics from an `rfsrc` Object
#'
#' @param x an `rfsrc` object.
#' @return A tibble with a single row. Each column corresponds to an `rfsrc` diagnostic.
#' @importFrom tibble as_tibble
#' @importFrom randomForestSRC get.confusion get.brier.error get.auc get.rfq.threshold
#' get.imbalanced.performance
#' @importFrom stats na.omit var
#' @importFrom tibble tibble
#' @export
rf_num_error_diagnostics = function(x) {
  err.rate <- cbind(x$err.rate)
  allcol.na <- apply(
    err.rate, 
    2, 
    function(x) {
      all(is.na(x))
    }
  )
  attributes(err.rate)$na.action <- NULL
  err.rate <- na.omit(err.rate[, !allcol.na, drop = FALSE])
  r_squared <- 1 - err.rate[nrow(err.rate), ] / var(x$yvar, na.rm = TRUE)
  mse_oob = as.numeric(err.rate)
  tibble(mse_oob = mse_oob, r_squared = r_squared)
}
