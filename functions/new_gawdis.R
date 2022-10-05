
require(gtools)

cor.matrix <- function(dataframe){
  
  combinations <- gtools::combinations(n = ncol(dataframe), r = 2, v = 1:ncol(dataframe))
  
  empty_matrix <- matrix(data = NA, nrow = ncol(dataframe), ncol = ncol(dataframe))
  
  for(i in 1:nrow(combinations)){
    r <- combinations[i,1]
    c <- combinations[i,2]
    
    num <- cor(dataframe[,c(r,c)])[1,2]
    
    empty_matrix[r,c] <-  num
    empty_matrix[c,r] <-  num
    
  }
  
  diag(empty_matrix) <- 1
  
  
  return(empty_matrix)
  
}


new_gawdis <- function (x, W = NULL, asym.bin = NULL, ord = c("podani", "metric", 
                                               "classic"), w.type = c("analytic", "optimized", "equal", 
                                                                      "user"), groups = NULL, groups.weight = FALSE, fuzzy = NULL, 
         opti.getSpecDists = NULL, opti.f = NULL, opti.min.weight = 0.01, 
         opti.max.weight = 1, opti.maxiter = 300, silent = FALSE) 
{
  if (length(dx <- dim(x)) != 2 || !(is.data.frame(x) || is.numeric(x))) 
    stop("x is not a dataframe or a numeric matrix\n")
  if ((!is.null(fuzzy)) & (groups.weight)) {
    warning("(fuzzy!=NULL) and groups.weight=TRUE is not possible, groups.weight is set to FALSE")
    groups.weight <- FALSE
  }
  if ((!is.null(fuzzy)) & (length(groups) < 1)) {
    warning("fuzzy!=NULL requires having groups argument set, distances will not be transformed")
  }
  w.type <- match.arg(w.type)
  if ((length(groups) >= 1) & (length(unique(groups)) == 1) & 
      (w.type != "equal")) {
    warning("just one group (~one trait) is automatically setting w.type to equal")
    w.type <- "equal"
  }
  if (!silent) {
    if (length(groups) >= 1) {
      print(paste("Running w.type=", w.type, " on groups=c(", 
                  paste(groups, collapse = ","), ")", sep = ""))
    }
    else {
      print(paste("Running w.type=", w.type, sep = ""))
    }
  }
  
  dx <- dim(x)
  n <- dx[1]
  p <- dx[2]
  ord <- match.arg(ord)
  varnames <- dimnames(x)[[2]]
  xorig = x
  if (is.data.frame(x)) {
    type <- sapply(x, data.class)
  } else {
    type <- rep("numeric", p)
    names(type) <- colnames(x)
  }
  if (any(type == "character")) 
    for (i in 1:p) if (type[i] == "character") 
      x[, i] <- as.factor(x[, i])
  is.bin <- function(k) all(k[!is.na(k)] %in% c(0, 1))
  bin.var <- rep(NA, p)
  names(bin.var) <- varnames
  for (i in 1:p) bin.var[i] <- is.bin(x[, i])
  if (any(type[bin.var] != "numeric")) 
    stop("Binary variables should be of class 'numeric'\n")
  type[type %in% c("numeric", "integer")] <- 1
  type[type == "ordered"] <- 2
  type[type %in% c("factor", "character")] <- 3
  type[bin.var] <- 4
  if (!is.null(asym.bin)) {
    if (!all(bin.var[asym.bin])) 
      stop("Asymetric binary variables must only contain 0 or 1\n")
    else type[asym.bin] <- 5
  }
  type <- as.numeric(type)
  x <- data.matrix(x)
  if (any(type == 2)) {
    if (ord != "classic") 
      for (i in 1:p) if (type[i] == 2) 
        x[, i] <- rank(x[, i], na.last = "keep")
    else for (i in 1:p) if (type[i] == 2) 
      x[, i] <- as.numeric(x[, i])
  }
  range.Data <- function(v) {
    r.Data <- range(v, na.rm = T)
    res <- r.Data[2] - r.Data[1]
    return(res)
  }
  range2 <- apply(x, 2, range.Data)
  comp.Timax <- function(v) {
    Ti.max <- max(v, na.rm = T)
    no.na <- v[!is.na(v)]
    res <- length(no.na[no.na == Ti.max])
    return(res)
  }
  Timax <- apply(x, 2, comp.Timax)
  comp.Timin <- function(v) {
    Ti.min <- min(v, na.rm = T)
    no.na <- v[!is.na(v)]
    res <- length(no.na[no.na == Ti.min])
    return(res)
  }
  Timin <- apply(x, 2, comp.Timin)
  if (ord == "podani") {
    pod <- 1
  } else {
    pod <- 2
  }
  if (w.type == "analytic") 
    if (any(is.na(x))) {
      warning("Analytic solution cannot be calculated due to missing trait values. Equal weighting will be applied.")
      w.type <- "equal"
    }
  d.raw <- matrix(NA, nrow = n * (n - 1)/2, ncol = p)
  for (i in 1:p) {
    if (type[i] == 1) 
      d.raw[, i] <- as.numeric(dist(x[, i]/range2[i]))
    if (type[i] == 2) {
      if (ord != "podani") 
        d.raw[, i] <- as.numeric(dist(x[, i]/range2[i]))
      else {
        tie <- rep(NA, n)
        for (j in 1:n) tie[j] <- sum(x[, i] == x[j, i], 
                                     na.rm = T)
        w <- matrix(NA, nrow = n, ncol = n)
        for (j in 1:n) w[j, ] <- abs(x[, i] - x[j, i]) - 
          ((tie[j] - 1)/2) - ((tie - 1)/2)
        w <- (w/(range2[i] - ((Timax[i] - 1)/2) - ((Timin[i] - 
                                                      1)/2)))
        for (j in 1:n) for (k in 1:n) if (is.na(x[j, 
                                                  i]) | is.na(x[k, i])) {
          w[j, k] <- NA
        }
        else {
          if ((x[j, i] == x[k, i])) 
            w[j, k] <- 0
        }
        d.raw[, i] <- 1 - as.numeric(as.dist(1 - w))
      }
    }
    if (type[i] == 3) {
      w <- matrix(NA, nrow = n, ncol = n)
      for (j in 1:n) w[j, ] <- as.numeric(x[, i] == x[j, 
                                                      i])
      d.raw[, i] <- as.numeric(as.dist(1 - w))
    }
    if (type[i] == 4) 
      d.raw[, i] <- as.numeric(dist(x[, i]))
    if (type[i] == 5) {
      w1 <- x[, i] %*% t(x[, i])
      w1 <- 1 - w1
      w2 <- (1 - x[, i]) %*% t(1 - x[, i])
      is.na(w1) <- (w2 == 1)
      d.raw[, i] <- as.numeric(as.dist(w1))
    }
  }
  tnames = dimnames(x)[[2]]
print(nrow(d.raw))
  
  cor.mat <- cor.matrix(d.raw)
  if (w.type == "analytic") {
    if (!is.null(groups)) {
      w <- c()
      d.raw2 <- NULL
      k <- 0
      tnames <- paste("gr", unique(groups), sep = "")
      for (i in unique(groups)) {
        k = k + 1
        ii = (1:length(groups))[i == groups]
        if (length(ii) > 1) {
          if (groups.weight) {
            print("Traits inside the group were weighted - analytic.")
            group.gaw = gawdis(as.data.frame(xorig[, 
                                                   ii]), w.type = "analytic", groups = NULL, 
                               groups.weight = F, fuzzy = fuzzy, silent = T)
          }
          else {
            print("Traits inside the group were not weighted - analytic.")
            group.gaw = gawdis(as.data.frame(xorig[, 
                                                   ii]), w.type = "equal", groups = rep(i, 
                                                                                        length(ii)), groups.weight = F, fuzzy = fuzzy, 
                               silent = T)
          }
          if (is.null(d.raw2)) {
            d.raw2 <- matrix(NA, nrow = nrow(d.raw), 
                             ncol = length(unique(groups)))
            d.raw2[, k] <- c(group.gaw)
          }
          else {
            d.raw2[, k] <- c(group.gaw)
          }
          w <- c(w, attr(group.gaw, "weights"))
        }
      }
      w <- w/sum(w)
      cor.mat2 <- cor.matrix(d.raw2)
      sigma2 <- apply(d.raw2, 2, sd, na.rm = T) * (nrow(d.raw2) - 
                                                     1)
      A <- matrix(NA, nrow = length(sigma2), ncol = length(sigma2))
      A[1, ] <- 1
      for (i in 2:length(sigma2)) for (j in 1:length(sigma2)) A[i, 
                                                                j] <- sigma2[j] * (cor.mat2[i, j] - cor.mat2[1, 
                                                                                                             j])
      w2 <- solve(A, c(1, rep(0, length(sigma2) - 1)))
      w.mat <- t(matrix(rep(w, nrow(d.raw)), nrow = p, 
                        ncol = nrow(d.raw)))
      d.rawna <- p - rowSums(is.na(d.raw))
      for (dn in which(d.rawna != p)) w.mat[dn, !is.na(d.raw[dn, 
      ])] <- w[!is.na(d.raw[dn, ])]/sum(w[!is.na(d.raw[dn, 
      ])])
      res2 <- rowSums(d.raw * w.mat, na.rm = T)
      correls2 <- cor.matrix(cbind(res2, d.raw))[-1, 
                                                                         1]
      d.raw <- d.raw2
      p <- length(sigma2)
      w3 <- w
      w <- w2
      cor.mat <- cor.mat2
    }
    else {
      sigma <- apply(d.raw, 2, sd, na.rm = T) * (nrow(d.raw) - 
                                                   1)
      A <- matrix(NA, nrow = p, ncol = p)
      A[1, ] <- 1
      for (i in 2:p) for (j in 1:p) A[i, j] <- sigma[j] * 
        (cor.mat[i, j] - cor.mat[1, j])
      w <- solve(A, c(1, rep(0, p - 1)))
    }
  }
  
  
  
  if (w.type == "optimized") {
    opti.res <- GAgawdis(tr = xorig, asym.bin = asym.bin, 
                         ord = ord, gr = groups, gr.weight = groups.weight, 
                         fuzzy = fuzzy, getSpecDists = opti.getSpecDists, 
                         f = opti.f, min.weight = opti.min.weight, max.weight = opti.max.weight, 
                         maxiter = opti.maxiter)
    w <- c(opti.res$weights)/sum(c(opti.res$weights))
  }
  if (w.type == "equal") {
    if (!is.null(groups)) {
      ugr <- unique(groups)
      w <- c()
      for (oneg in groups) w <- c(w, sum(groups == oneg)/((length(ugr)) * 
                                                            sum(groups == oneg) * sum(groups == oneg)))
    }
    else {
      w <- rep(1, p)/sum(rep(1, p))
    }
  }
  if (w.type == "user") {
    if (!missing(W)) {
      if (length(W) != p | !is.numeric(W)) 
        stop("w needs to be a numeric vector of length = number of variables in x\n")
      if (all(W == 0)) 
        stop("Cannot have only 0's in 'w'\n")
      w <- W/sum(W)
    }
    else w <- rep(1, p)/sum(rep(1, p))
  }
  if ((length(groups) < 1) | (length(unique(groups)) == 1) | 
      (w.type %in% c("analytic", "optimized"))) {
    w.mat <- t(matrix(rep(w, nrow(d.raw)), nrow = p, ncol = nrow(d.raw)))
    d.rawna <- p - rowSums(is.na(d.raw))
    for (dn in which(d.rawna != p)) w.mat[dn, !is.na(d.raw[dn, 
    ])] <- w[!is.na(d.raw[dn, ])]/sum(w[!is.na(d.raw[dn, 
    ])])
    res <- rowSums(d.raw * w.mat, na.rm = T)
    if ((!is.null(fuzzy)) && (length(unique(groups)) == 1) && 
        (unique(groups) %in% fuzzy)) 
      res <- res/max(res, na.rm = T)
    res <- ifelse(d.rawna == 0, NA, res)
  }
  else {
    ugr <- unique(groups)
    pgaw <- NULL
    pgaw.na <- NULL
    for (i in ugr) {
      ii = (1:length(groups))[i == groups]
      if (is.null(pgaw)) {
        pgaw <- gawdis(as.data.frame(x[, ii]), W = w[ii], 
                       w.type = w.type, groups = rep(i, length(ii)), 
                       fuzzy = fuzzy)
        pgaw.na <- !is.na(gowdis(as.data.frame(x[, ii])))
        if ((!is.null(fuzzy)) && (i %in% fuzzy)) 
          pgaw <- pgaw/max(pgaw, na.rm = T)
      }
      else {
        onegaw <- gawdis(as.data.frame(x[, ii]), W = w[ii], 
                         w.type = w.type, groups = rep(i, length(ii)), 
                         fuzzy = fuzzy)
        pgaw.na1 <- !is.na(gowdis(as.data.frame(x[, ii])))
        pgaw.na <- pgaw.na + pgaw.na1
        if ((!is.null(fuzzy)) && (i %in% fuzzy)) {
          pgaw <- pgaw + (onegaw/max(onegaw, na.rm = T))
        }
        else {
          pgaw <- pgaw + onegaw
        }
      }
    }
    res <- pgaw/pgaw.na
  }
  correls <- cor.matrix(cbind(res, d.raw))[-1, 
                                                                   1]
  if (any(w < 0) & (!silent)) 
    warning("Some weights are negative! Try to remove the corresponding trait from the analysis or re-run the function with w.type='optimized'")
  type[type == 1] <- "N"
  type[type == 2] <- "O"
  type[type == 3] <- "C"
  type[type == 4] <- "B"
  type[type == 5] <- "A"
  if (any(is.na(res))) 
    attr(res, "NA.message") <- "NA's in the dissimilarity matrix!"
  attr(res, "Labels") <- dimnames(x)[[1]]
  attr(res, "Size") <- n
  attr(res, "Metric") <- "Gower"
  attr(res, "Types") <- type
  names(correls) <- tnames
  attr(res, "correls") <- correls
  names(w) <- tnames
  attr(res, "weights") <- w
  if (!is.null(groups) & (w.type == "analytic")) {
    names(w3) <- dimnames(x)[[2]]
    attr(res, "indiv.weights") <- w3
    attr(res, "indiv.correls") <- correls2
  }
  if (!is.null(groups) & (w.type == "optimized")) {
    d.rawg <- data.frame(row.names = 1:length(c(opti.res$spedis[[1]])))
    for (sdi in 1:length(opti.res$spedis)) d.rawg[[names(opti.res$spedis)[sdi]]] <- c(opti.res$spedis[[names(opti.res$spedis)[sdi]]])
    correlsg <- cor.matrix(cbind(res, d.rawg))[-1, 
                                                                       1]
    attr(res, "group.correls") <- correlsg
  }
  colnames(d.raw) <- tnames
  attr(res, "components") <- d.raw
  colnames(cor.mat) <- tnames
  rownames(cor.mat) <- tnames
  attr(res, "cor.mat") <- cor.mat
  d.raw.f <- apply(d.raw, 2, function(x) {
    table(x)/sum(table(x))
  })
  d.raw.a <- unlist(lapply(d.raw.f, function(x) {
    any(x > 0.66)
  }))
  d.raw.n <- names(d.raw.a)[d.raw.a]
  if ((length(d.raw.n) > 1) & (!silent)) 
    warning(paste("Consider removing traits: ", paste(d.raw.n, 
                                                      collapse = " "), ", because of unbalanced distribution.", 
                  sep = ""))
  if ((sd(correls) > 0.05) & (is.null(groups)) & (!silent)) {
    warning("The effects of invididual traits are higly variable, consider excluding some less variable or unbalanced traits.")
    if (w.type == "optimized") 
      warning("Or consider increase of opti.maxiter.")
  }
  class(res) <- "dist"
  return(res)
}