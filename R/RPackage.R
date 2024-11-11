#' CreatePackageInR: A Package for Demonstrating Custom Functions in R
#'
#' The CreatePackageInR package provides a collection of functions that are used for
#' graph creation and mathematical calculations, such as finding the greatest common divisor (GCD)
#' and calculating shortest paths using Dijkstra's algorithm.
#'
#' @section CreatePackageInR functions:
#' The package includes the following functions:
#' \itemize{
#'   \item \code{euclidean()}: Implements the Euclidean algorithm to find the GCD of two numbers.
#'   \item \code{dijkstra()}: Implements Dijkstra's algorithm to find the shortest path from a given initial node.
#' }
#'
#' @keywords internal
"_PACKAGE"

#' Euclidean Algorithm to find GCD
#'
#' @param a First number
#' @param b Second number
#' @return GCD of a and b
#' @examples
#' euclidean(100, 1000)
#' euclidean(123612, 13892347912)
#' @export
euclidean <- function(a, b) {
  if (!is.numeric(a) || !is.numeric(b)) {
    stop("Both arguments must be numeric.")
  }
  while (b != 0) {
    temp <- b
    b <- a %% b
    a <- temp
  }
  return(abs(a))
}

#' Dijkstra's Algorithm for shortest paths
#'
#' @param graph Data frame of edges (v1, v2, w)
#' @param init_node Initial node to calculate paths from
#' @return Vector of minimum distances from init_node
#' @examples
#' wiki_graph <- data.frame(
#'   v1 = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 5, 5, 6, 6, 6),
#'   v2 = c(2, 3, 6, 1, 3, 4, 1, 2, 4, 6, 2, 3, 5, 4, 6, 1, 3, 5),
#'   w = c(7, 9, 14, 7, 10, 15, 9, 10, 11, 2, 15, 11, 6, 6, 9, 14, 2, 9)
#' )
#' dijkstra(wiki_graph, 1)
#' @export
dijkstra <- function(graph, init_node) {
  if (!'data.frame' %in% class(graph) || !all(c('v1', 'v2', 'w') %in% names(graph))) {
    stop("Graph must be a data frame with columns v1, v2, and w.")
  }
  if (!is.numeric(init_node) || !init_node %in% c(graph$v1, graph$v2)) {
    stop("init_node must be a numeric value present in graph.")
  }

  nodes <- unique(c(graph$v1, graph$v2))
  dist <- setNames(rep(Inf, length(nodes)), nodes)
  dist[as.character(init_node)] <- 0
  priority_queue <- setNames(dist, names(dist))

  while(length(priority_queue) > 0) {
    current_node <- names(priority_queue)[which.min(priority_queue)]
    priority_queue <- priority_queue[-which.min(priority_queue)]

    edges <- subset(graph, v1 == current_node)
    for (i in 1:nrow(edges)) {
      new_dist <- dist[current_node] + edges[i, "w"]
      if (new_dist < dist[as.character(edges[i, "v2"])]) {
        dist[as.character(edges[i, "v2"])] <- new_dist
        priority_queue[as.character(edges[i, "v2"])] <- new_dist
      }
    }
  }
  return(unname(dist))
}
