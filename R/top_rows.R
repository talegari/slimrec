#' @title top_rows
#' @description Find the row numbers corresponding the largest values in a
#'   particular column of a matrix
#' @details To find top-n recommendations of a ratings matrix given an item (or
#'   a user).  Although k recommendations are expected to be returned, the
#'   function might sometimes return more or less than k recommendations.
#'
#'   \itemize{ \item Less: This happens when it is not possible to recommend k
#'   elements. For example, k is larger than the number of elements. \item More:
#'   This happens when a few elements have same rating. The function returns the
#'   index corresponding to all the elements which have the same rating. If
#'   ratings were \code{3,2,2,2,3}: \itemize{ \item k = 3: returns
#'   \code{1, 5, 2, 3, 4} \item k = 2: returns \code{1, 5} \item k = 1: returns
#'   \code{1, 5} }}
#'
#' @param mat (Sparse matrix of class 'dgCmatrix' or a integer/numeric matrix or
#'   'big.matrix') Rating matrix.
#' @param col (positive integer) Column number in which top rows are to be
#'   selected.
#' @param k (positive integer) Number of row numbers to be recommended. This
#'   might not be strictly adhered to, see Details.
#' @param ignore (integer vector) Row numbers to be ignored.
#'
#' @examples
#' \dontrun{
#' require("slimrec")
#' data(ft_small)
#' dir.create(file.path(tempdir(), "slim_test"))
#' temp <- slim(ft_small
#'              , returnMat = TRUE
#'              , directory = file.path(tempdir(), "slim_test")
#'              )
#' unlink(file.path(tempdir(), "slim_test"), recursive = TRUE)
#' dim(temp[["ratingMat"]])
#' top_rows(temp[["ratingMat"]], col = 53)
#' top_rows(temp[["ratingMat"]], col = 53, ignore = c(3,969))
#' }
#'
#' @export

top_rows <- function(mat
                     , col
                     , k = 5
                     , ignore){
  # assertions         ----
  if(!(inherits(mat, "dgCMatrix") ||
       inherits(mat, "matrix") ||
       inherits(mat, "big.matrix"))){
    stop("'mat' should be a sparse matrix of class 'dgCMatrix' OR a numeric/integer matrix OR an object of class 'big.matrix'")
  }

  if(!inherits(mat, "big.matrix")){
    if(anyNA(mat)){
      stop("'mat' should not contain NA")
    }
  }

  n <- ncol(mat)
  m <- nrow(mat)

  if(!(is.count(col) && col <= n)){
    stop("'col' should be a positive integer not greater than the number of columns")
  }

  if(!(is.count(k))){
    stop("'k' should be a positive integer")
  }
  if(!missing(ignore)){
    if(!(all(ignore == floor(ignore))) ||
       !(max(ignore) <= m) ||
       !(min(ignore) > 0)
       ){
      stop("'ignore' should be integer vector corresponding to the row numbers to be ignored")
    }
    ignore <- unique(ignore)
    if(length(ignore) == m){
      return(integer(0))
    }
  }

  # compute top_k rows ----
  theCol        <- as.list(mat[, col])
  names(theCol) <- 1:m
  if(!missing(ignore)){
    theCol      <- theCol[-ignore]
  }
  k         <- min(k, length(theCol))
  tops      <- sort(unlist(theCol), decreasing = TRUE)[1:k]
  top_k_min <- min(tops)
  top_k_max <- max(tops)
  top_index <- names(theCol)[theCol >= top_k_min & theCol <= top_k_max]
  top_k     <- top_index[order(unlist(theCol[top_index]), decreasing = TRUE)]
  return(as.integer(top_k))
}