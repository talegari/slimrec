# implement RMSE measure on random perturbations for implicit data
perturb_slim <- function(mat
                         , alphaRange  = seq(0, 1, 0.1)
                         , nonNegCoeff = TRUE
                         , fraction    = 0.1
                         , ntimes      = 10
                         , seed
                         , directory
                         , nproc       = 1L
                         , progress    = TRUE){
  # assertions ----
  if(!(inherits(mat, "dgCMatrix"))){
    stop("'mat' should be a sparse matrix of class 'dgCMatrix'")
  }
  if(anyNA(mat)){
    stop("'mat' should not contain NA")
  }
  if(!(all(alphaRange >= 0) && all(alphaRange <= 1))){
    stop("'alpha' should be a number between(inclusive) 0 and 1")
  }
  if(length(alphaRange) > 100){
    stop("'alphaRange' can have a maximum length of 100")
  }
  alphaRange <- sort(alphaRange)
  if(!(is.flag(nonNegCoeff))){
    stop("'nonNegCoeff' has to either TRUE or FALSE")
  }
  if(!(fraction > 0 && fraction < 1)){
    stop("'fraction' should be between(not inclusive) 0 and 1")
  }
  if(!is.count(ntimes)){
    stop("'ncount' should be a positive integer")
  }
  if(!missing(seed)){
    if(length(seed) != length(alphaRange)){
      stop("'seed' vector should have the same length as 'alphaRange'")
    }
    if(!all(vapply(seed, is.count, logical(1)))){
    }
  } else{
    seed <- sample(1:1e6, length(alphaRange))
  }
  if(missing(directory)){
    directory <- tempdir()
  } else {
    if(!(is.string(directory))){
      stop("'directory' should be a string")
    }
    if(!(dir.exists(directory))){
      stop("'directory' should exist")
    }
    if(!(is.writeable(directory))){
      stop("'directory' is not writable. Check user permissions")
    }
  }
  if(!(is.count(nproc))){
    stop("'nproc' should be a positive integer")
  }
  if(!(is.flag(progress))){
    stop("'progress' should be either TRUE or FALSE")
  }

  # function to perturb mat and compute the error ----
  matLen <- length(mat)
  perturb_error <- function(iterVal){
    mat2     <- mat
    nOnes    <- sum(mat)
    sparsity <- nOnes/matLen
    nPerturb <- ceiling(fraction * nOnes)
    nRandom  <- ceiling(nPerturb/sparsity)

    set.seed(seed[iterVal])
    rands      <- unique(round(runif(nRandom, 1, matLen)))
    randsIndex <- rep(FALSE, length(rands))
    for(arand in 1:length(randsIndex)){
      if(mat2[arand] == 1){
        mat2[arand]        <- 0L
        randsIndex[arand]  <- TRUE
      }
    }

    # run slim
    temp     <- slim(mat           = mat2
                     , alpha       = alphaRange[iterVal]
                     , nonNegCoeff = nonNegCoeff
                     , directory   = directory
                     , returnMat   = TRUE
                     , nproc       = nproc
                     , progress    = FALSE
                     , check       = FALSE
                     , cleanup     = TRUE
                     )

    errorVal <- ModelMetrics::rmse(temp$ratingMat[randsIndex]
                                   , rep(1, sum(randsIndex))
                                   )
    return(errorVal)
  }

  # compute and return seed and errors
  errors <- vapply(1:length(alphaRange), perturb_error, numeric(1))
  return(data.frame(seed = seed, error = errors))
}
