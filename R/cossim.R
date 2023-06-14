#' Compute cosine similarity among rows in a matrix
#'
#' @param x A matrix of examples (rows) by features (columns)
#' @return A matrix with dimensions `nrow(x)` by `nrow(x)`.
#'
#' @export
cossim <- function(x) {
  x <- as.matrix(x)
  y <- x / sqrt(rowSums(x * x))
  return(tcrossprod(y))
}


#' Simple, but inefficient, implementation of cosine similarity for testing
#'
#' @param x A matrix of examples (rows) by features (columns)
#' @return A matrix with dimensions `nrow(x)` by `nrow(x)`.
simple_cossim <- function(x) {
  n <- nrow(x)
  y <- matrix(0, nrow = n, ncol = n)
  for (i in 1:n) {
    for (j in 1:n) {
      y[i,j] <- crossprod(x[i,], x[j,]) / sqrt(crossprod(x[i,]) * crossprod(x[j,]))
    }
  }
  return(y)
}
