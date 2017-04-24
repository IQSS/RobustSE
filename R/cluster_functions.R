# function to calculate meat of cluster robust matrix
meat.clust <- function(fm, cluster){
    C <- cluster
    N <- length(cluster)
    M <- length(unique(C))
    K <- fm$rank

    ## degress of freedom adjustment
    dfa <- (M / (M-1))*((N-1)/(N-K))

    # within cluster and between cluster sums
    u.clust  <- apply(sandwich::estfun(fm),2, function(x) tapply(x, cluster, sum))
    # meat matrix calculation with degrees of freedom adjustment
    meat <- dfa*(crossprod(u.clust))

    return(meat)
}

clust.robust <- function(fm, cluster){
    C <- cluster
    N <- length(cluster)
    M <- length(unique(C))
    K <- fm$rank

    ## degress of freedom adjustment
    dfa <- (M / (M-1))*((N-1)/(N-K))

    # within cluster and between cluster sums
    u.clust  <- apply(sandwich::estfun(fm),2, function(x) tapply(x, cluster, sum))
    # meat matrix calculation with degrees of freedom adjustment
    meat <- dfa*(crossprod(u.clust))

    # bread calculation
    bread <- vcov(fm)

    # cluster robust vcov matrox
    V.cl <- bread %*% meat %*% bread

    return(V.cl)
}
