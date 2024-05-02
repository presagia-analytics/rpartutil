
#' Match Nodes
#' @param nodes the nodes to match.
#' @param nodelist the list of nodes.
#' @param leaves the leaves to match to.
#' @param print.it should the result be printed? Default is `TRUE`.
#' @export
node.match = function(nodes, nodelist, leaves, print.it = TRUE) {
  node.index <- match(nodes, nodelist, 0L)
  bad <- nodes[node.index == 0L]
  if (length(bad) > 0 && print.it)
    warning(gettextf("supplied nodes %s are not in this tree",
      paste(bad, collapse = ",")), domain = NA)
  good <- nodes[node.index > 0L]
  if (!missing(leaves) && any(leaves <- leaves[node.index])) {
    warning(gettextf("supplied nodes %s are leaves", paste(good[leaves],
      collapse = ",")), domain = NA)
    node.index[node.index > 0L][!leaves]
  }
  else node.index[node.index > 0L]
}
