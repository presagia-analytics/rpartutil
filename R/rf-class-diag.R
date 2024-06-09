library(tibble)
library(randomForestSRC)


#' @title Get the Error Diagnostics from an `rfsrc` Object
#'
#' @param x an `rfsrc` object.
#' @return A tibble with a single row. Each column corresponds to an `rfsrc` diagnostic.
#' @importFrom tibble as_tibble
#' @importFrom randomForestSRC get.confusion get.brier.error get.auc get.rfq.threshold
#' get.imbalanced.performance
#' @importFrom stats na.omit
#' @export
rf_class_error_diagnostics = function(x) {
  conf.matx <- get.confusion(
    x$yvar, 
    if (!is.null(x$class.oob) && !all(is.na(x$class.oob))) {
      x$class.oob
    } else {
      x$class
    }
  )
  miss.err.rate <- 1 - sum(diag(conf.matx[, -ncol(conf.matx),
      drop = FALSE]))/sum(conf.matx[, -ncol(conf.matx),
      drop = FALSE])
  names(dimnames(conf.matx)) <- c("  observed", "predicted")
  if (length(levels(x$yvar)) > 2) {
      brier.err <- get.brier.error(
        x$yvar, 
        if (!is.null(x$predicted.oob) && !all(is.na(x$predicted.oob))) {
          x$predicted.oob
        } else {
          x$predicted
        }, 
        normalized = FALSE
      )
      brier.norm.err <- get.brier.error(
        x$yvar, 
        if (!is.null(x$predicted.oob) && !all(is.na(x$predicted.oob))) {
          x$predicted.oob
        } else {
          x$predicted
        }
      )
      auc.err <- get.auc(
        x$yvar, 
        if (!is.null(x$predicted.oob) && !all(is.na(x$predicted.oob))) {
          x$predicted.oob
        } else {
          x$predicted
        }
      )
      iratio <- pr.auc.err <- gmean.err <- NULL
  } else {
      if (x$forest$rfq) {
        threshold <- get.rfq.threshold(x$forest$yvar)
      } else {
        threshold <- 0.5
      }
      perO <- get.imbalanced.performance(
        x$yvar, 
        if (!is.null(x$predicted.oob) && !all(is.na(x$predicted.oob))) {
          x$predicted.oob
        } else {
          x$predicted
        },
        threshold = threshold, 
        confusion = TRUE
      )
      iratio <- perO$iratio
      brier.err <- perO$brier
      brier.norm.err <- perO$brier.norm
      auc.err <- perO$auc
      pr.auc.err <- perO$pr.auc
      gmean.err <- perO$gmean
  }
  ret = list(
    imbalance_ratio = iratio,
    brier_score = brier.err,
    norm_brier_score = brier.norm.err,
    auc = auc.err,
    pr_auc = pr.auc.err,
    g_mean = gmean.err
  )
  err.rate <- cbind(x$err.rate)
  allcol.na <- apply(err.rate, 2, function(x) {
      all(is.na(x))
  })
  if (sum(inherits(x, c("rfsrc", "grow"), TRUE) == c(1, 2)) ==
      2) {
      grow.mode <- TRUE
  } else {
      grow.mode <- FALSE
  }
  err.rate <- na.omit(err.rate[, !allcol.na, drop = FALSE]) |>
    as.vector()
  append(ret, list(error_rate = err.rate)) |> as_tibble()
}
