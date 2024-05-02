
#' Get a Node's Descendants
#' @param nodes the node to get the descendants.
#' @param include should the descriptions be included? Default it TRUE.
#' @export
descendants = function (nodes, include = TRUE) {
  n <- length(nodes)
  if (n == 1L)
    return(matrix(TRUE, 1L, 1L))
  ind <- 1:n
  desc <- matrix(FALSE, n, n)
  if (include)
    diag(desc) <- TRUE
  parents <- match((nodes%/%2L), nodes)
  lev <- floor(log(nodes, base = 2))
  desc[1L, 2L:n] <- TRUE
  for (i in max(lev):2L) {
    desc[cbind(ind[parents[lev == i]], ind[lev == i])] <- TRUE
    parents[lev == i] <- parents[parents[lev == i]]
    lev[lev == i] <- i - 1L
  }
  desc
}
