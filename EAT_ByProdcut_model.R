#' @title Directional Distance Function for Good Output Technology
#'
#' @description This function calculates the Directional Distance Function (DDF)
#' for Good Output Technology under the Murphy et al. (2012) parading using
#' DEA/FDH and CEAT/EAT
#'
#' @name DDF_T1
#'
#' @param data \code{data.frame} containing the DMUs' data
#' @param q Column good input indexes in \code{data}.
#' @param p Column bad input indexes in \code{data}.
#' @param y Column good output indexes in \code{data}.
#' @param FDH should convexified approach be used? If \code{TRUE}, DEA or CEAT
#' model are calculated. If \code{FALSE}, FDH or EAT are.
#' @param EAT_T1 Efficiency Analysis Tree fitted to good output technology. 
#' If \code{NULL} the scores are calculated under DEA or FDH. If not, they are
#' calculated under CEAT or EAT.
#'
#' @return The efficiency score for each DMU

DDF_T1 <- function(data, q, p, y, FDH, EAT_T1) {
  
  # Compute for EAT/CEAT
  if (!is.null(EAT_T1)) {
    atreeTk <- EAT_T1[["model"]][["a"]]
    atreeTk_q <- as.matrix(atreeTk[, q])
    atreeTk_p <- as.matrix(atreeTk[, p])
    ytreeTk <- EAT_T1[["model"]][["y"]]
    N_leaves <- EAT_T1[["model"]][["leaf_nodes"]]
  } else { # Compute for FDH/DEA
    atreeTk_q <- as.matrix(data[, q])
    atreeTk_p <- as.matrix(data[, p])
    ytreeTk <- as.matrix(data[, y])
    N_leaves <-  nrow(data)
  }
  
  numDMU <- nrow(data)
  q_k <- as.matrix(data[, q])
  p_k <- as.matrix(data[, p])
  y_k <- as.matrix(data[, y])
  m1 <- length(q) # num of good inputs
  m2 <- length(p) # num of bad inputs
  r <- length(y) # num of good outputs
  scores <- matrix(nrow = numDMU, ncol = 1)
  
  # get scores
  for(d in 1:numDMU){
    
    g <- data[d,y] # g <- (rep(0,m1), rep(0,m2), data[d,y], data[d,z])
    
    objVal <- matrix(ncol = 1 + N_leaves, nrow = 1) # beta + lambdas
    objVal[1] <- 1 # beta
    
    # structure for lpSolve
    #lps <- make.lp(nrow = m1 + m2 + r, ncol = N_leaves+ 1)
    lps <- make.lp(nrow = 0, ncol = N_leaves+ 1)
    lp.control(lps, sense = 'max')
    set.objfn(lps, objVal)
    
    # constrain 2.1 - good inputs
    for(xi in 1:m1)
    { 
      add.constraint(lps, xt = c(0, atreeTk_q[, xi]), "<=",  rhs = q_k[d, xi])
    }
    # constrain 2.2 - bad inputs
    for(xi in 1:m2)
    { 
      add.constraint(lps, xt = c(0, atreeTk_p[, xi]), "<=",  rhs = p_k[d, xi])
    }
    # constrain 2.3 - good output
    for(yi in 1:r)
    { 
      add.constraint(lps, xt = c(- g[yi], ytreeTk[, yi]), ">=", rhs = y_k[d, yi])
    }
    
    # Constrain 2.4 - lambda = 1
    add.constraint(lprec = lps, xt = c(0, rep(1, N_leaves)), type = "=", rhs = 1)
    
    # Constrain 2.5 - Binary
    if (FDH) {
      set.type(lps, 2:(N_leaves+ 1), "binary")
    }
    
    status <- solve(lps)
    scores[d, ] <- get.objective(lps)
    
  }
  return(scores)
}


#' @title Directional Distance Function for Bad Output Technology
#'
#' @description This function calculates the Directional Distance Function (DDF)
#' for Bad Output Technology under the Murphy et al. (2012) parading using
#' DEA/FDH and CEAT/EAT
#'
#' @name DDF_T2
#'
#' @param data \code{data.frame} containing the DMUs' data
#' @param q Column good input indexes in \code{data}.
#' @param p Column bad input indexes in \code{data}.
#' @param z Column bad output indexes in \code{data}.
#' @param FDH should convexified approach be used? If \code{TRUE}, DEA or CEAT
#' model are calculated. If \code{FALSE}, FDH or EAT are.
#' @param EAT_T1 Efficiency Analysis Tree fitted to bad output technology. 
#' If \code{NULL} the scores are calculated under DEA or FDH. If not, they are
#' calculated under CEAT or EAT.
#'
#' @return The efficiency score for each DMU

DDF_T2 <- function(data, p, z, FDH, EAT_T2) {
  # Compute for EAT
  if (!is.null(EAT_T2)) {
    atreeTk <- EAT_T2[["model"]][["a"]]
    ytreeTk <- EAT_T2[["model"]][["y"]]
    N_leaves <- EAT_T2[["model"]][["leaf_nodes"]]
  } else { # Compute for FDH/DEA
    atreeTk <- as.matrix(data[, z])
    ytreeTk <- as.matrix(data[, p])
    N_leaves <-  nrow(data)
  }
  
  numDMU <- nrow(data)
  p_k <- as.matrix(data[, p])
  z_k <- as.matrix(data[, z])
  m2 <- length(p) # num of bad inputs
  s <- length(z) # num of bad outputs
  scores <- matrix(nrow = numDMU, ncol = 1)
  
  # get scores
  for(d in 1:numDMU){
    
    g <- data[d,z] # g <- (rep(0,m1), rep(0,m2), data[d,y], data[d,z])
    
    objVal <- matrix(ncol = 1 + N_leaves, nrow = 1) # beta + lambdas
    objVal[1] <- 1 # beta
    
    # structure for lpSolve
    lps <- make.lp(nrow = m2 + s, ncol = N_leaves+ 1)
    lp.control(lps, sense = 'max')
    set.objfn(lps, objVal)
    
    # constrain 2.1 - bad inputs
    for(xi in 1:m2)
    { 
      add.constraint(lps, xt = c(0, ytreeTk[, xi]), ">=",  rhs = p_k[d, xi])
    }
    # constrain 2.2 - bad output
    for(yi in 1:s)
    { 
      add.constraint(lps, xt = c(g[yi], atreeTk[, yi]), "<=", rhs = z_k[d, yi])
    }
    
    # Constrain 2.3 - lambda = 1
    add.constraint(lprec = lps, xt = c(0, rep(1, N_leaves)), type = "=", rhs = 1)
    
    # Constrain 2.4 - Binary
    if (FDH) {
      set.type(lps, 2:(N_leaves+ 1), "binary")
    }
    
    status <- solve(lps)
    scores[d, ] <- get.objective(lps)
    
  }
  return(scores)
}

#' @title Directional Distance Function for By Production EAT/CEAT
#'
#' @description This function calculates the Directional Distance Function (DDF)
#' for the Technology under the Murphy et al. (2012) parading using
#' DEA/FDH and CEAT/EAT
#'
#' @name DDF_T2
#'
#' @param data \code{data.frame} containing the DMUs' data
#' @param q Column good input indexes in \code{data}.
#' @param p Column bad input indexes in \code{data}.
#' @param y Column good output indexes in \code{data}.
#' @param y Column bad output indexes in \code{data}.
#' @param delta1 Importance of good output technology. Note that delta1+delta2 = 1
#' @param delta2 Importance of bad output technology. Note that delta1+delta2 = 1
#' @param numStop Stopping rule for EAT
#'
#' @return The efficiency score for each DMU
#' 
EATByProduct <- function(data, q, p, y, z, delta1, delta2, numStop) {
  
  # Fit a EAT tree to T1
  EAT_T1 <- EAT(data, x = c(q,p), y = y, numStop = numStop, max.depth = Inf)
  # Solve DDF for T1 with g =(0,0,y,z)
  score_EAT_T1 <- DDF_T1(data, q, p, y, TRUE, EAT_T1)
  score_CEAT_T1 <- DDF_T1(data, q, p, y, FALSE, EAT_T1)
  score_FDH_T1 <- DDF_T1(data, q, p, y, TRUE, NULL)
  score_DEA_T1 <- DDF_T1(data, q, p, y, FALSE, NULL)
  
  # Fit a EAT tree to T2
  EAT_T2 <- EAT(data, x = z, y = p, max.depth = Inf)
  #  Solve DDF for T2 with g =(0,0,y,z)
  score_EAT_T2 <- DDF_T2(data, p, z, TRUE, EAT_T2)
  score_CEAT_T2 <- DDF_T2(data, p, z, FALSE, EAT_T2)
  score_FDH_T2 <- DDF_T2(data, p, z, TRUE, NULL)
  score_DEA_T2 <- DDF_T2(data, p, z, FALSE, NULL)
  
  # Combine scores from T1 and T2
  score_EAT <- delta1 * (1- (1/(1+score_EAT_T1))) + delta2 * score_EAT_T2
  score_CEAT <- delta1 * (1- (1/(1+score_CEAT_T1))) + delta2 * score_CEAT_T2
  score_FDH<- delta1 * (1- (1/(1+score_FDH_T1))) + delta2 * score_FDH_T2
  score_DEA<- delta1 * (1- (1/(1+score_DEA_T1))) + delta2 * score_DEA_T2
  
  return(data.frame(score_EAT_T1 = score_EAT_T1, 
                    score_EAT_T2 = score_EAT_T2, 
                    score_EAT = score_EAT,
                    score_CEAT_T1 = score_CEAT_T1, 
                    score_CEAT_T2 = score_CEAT_T2, 
                    score_CEAT = score_CEAT,
                    score_FDH_T1 = score_FDH_T1, 
                    score_FDH_T2 = score_FDH_T2, 
                    score_FDH = score_FDH,
                    score_DEA_T1 = score_DEA_T1, 
                    score_DEA_T2 = score_DEA_T2, 
                    score_DEA = score_DEA))
}
