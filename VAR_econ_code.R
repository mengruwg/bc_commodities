source("R/VAR_country_setup.R")

data <- data_aus[c(7, 3, 1, 5, 6, 4, 2)]
data$mp_rate <- data$mp_rate / 100
data$i10y <- data$i10y / 100
data[c(2, 5, 6, 7)] <- data[c(2, 5, 6, 7)] * 100

model <- VAR(y = data, lag.max = 8, ic = "AIC")

plot(irf(model))



estimate_var <- function(Y, 
                         lag = 2) {
  
  # Setup -------------------------------------------------------------------

  Y <- as.matrix(Y)
  X <- lag_x(Y, lag)
  
  Y <- Y[(lag + 1):nrow(Y), ]
  X <- X[(lag + 1):nrow(X), ]
  
  X_col <- ncol(X)
  Y_col <- ncol(Y)
  Y_row <- nrow(Y)
  
  ols_est <- solve(crossprod(X)) %*% crossprod(X, Y)
  sse <- crossprod(Y - X %*% ols_est)
  sigma <- ols_sigma <- sse / (Y_row - X_col)
  
  # identification
  id <- t(chol(sigma))
  #id_inv <- solve(id)
  #id_inv[upper.tri(matrix(1, ncol(id), nrow(id)))] <- 0
  
  M_comp <- matrix(0, X_col, X_col)
  M_comp[1:Y_col, ] <- t(ols_est)
  M_comp[(Y_col + 1):X_col, 1:(X_col - Y_col)] <- diag(Y_col * (lag - 1))
  
  J <- diag(1, nrow = X_col, ncol = Y_col)
  eta <- J %*% id
  
  z <- vector("numeric", X_col)
  z <- t(Y[1, ])
  z <- append(z, Y[2, ], Y_col)
  
  eta %*% M_comp
  
  imp_responses <- array(0, c(Y_col * lag, Y_col * lag, X_col))
  apply(shocks, 2, function(x) {
    x %*% t(companion)
  })
  
  shocks %*% t(companion)
  
  for(i in 1:20) {
    
  }
  
}

irf <- function(variables) {
  impulses <- responses <- colnames(Y)
  
  ci <- 1 - ci
  
  
  
  
  irf.mat <- array(0, c(M * p, M * p, k))
  irf.mat[, , 1] <- shock
  for (ihorz in 2:k) {
    irf.mat[, , ihorz] <- irf.mat[, , ihorz - 1] %*% t(B.comp)
  }
  irf.mat <- irf.mat[1:M, 1:M, ]
  
  IRF.store[irep - nburn, , , ] <- irf.mat
  
}

# Bayes -------------------------------------------------------------------

ar_sigma <- apply(Y, 2, function(x) {
  sqrt(arima(x, order = c(lag, 0, 0))$sigma2)
})

dummies <- get.dum(sigma = ar_sigma, Y_col = Y_col, lag = lag)

Y_prior <- dummies$Ydum
X_prior <- dummies$Xdum

mean_prior <- solve(crossprod(X_prior)) %*% crossprod(X_prior, Y_prior)

Y_post <- rbind(Y, Y_prior)
X_post <- rbind(X, X_prior)

var_post <- solve(crossprod(X_post))
mean_post <- var_post %*% crossprod(X_post, Y_post)

v_post <- kronecker(sigma, var_post)
v_post_chol <- t(chol(v_post))

t(chol(var_post)) %*% chol(var_post) == var_post

#Create dummy matrices
#Specify hyperparameter on the minnesota prior and on the constant
#these will the arguments of the function
get.dum <- function(theta = 0.1, 
                    gamma.prior = 10, # 0.15?
                    delta = 1,
                    sigma, 
                    Y_col,
                    lag) {
  
  ydummy <- matrix(0, 2 * Y_col + Y_col * (lag - 1) + 1, Y_col)
  xdummy <- matrix(0, 2 * Y_col + Y_col * (lag - 1) + 1, Y_col * lag + 1)
  
  ydummy[1:Y_col, ] <- diag((as.numeric(sigma) * delta) / theta)
  ydummy[(Y_col * (lag - 1) + Y_col + 1):(Y_col * (lag - 1) + 2 * Y_col), ] <-
    diag(as.numeric(sigma))
  
  jp <- diag(1:lag)
  xdummy[1:(Y_col * lag), 1:(Y_col * lag)] <-
    kronecker(jp, diag(as.numeric(sigma))) / theta
  xdummy[nrow(xdummy), ncol(xdummy)] <- gamma.prior
  return(list(Xdum = xdummy, Ydum = ydummy))
}


##### break

nsave <- 500
nburn <- 500
ntot <- nsave + nburn

A.store <- array(0, c(nsave, K, M))
y.store <- matrix(0, nsave, M)
IRF.store <- array(NA, c(nsave, M, M, k))



# eco ---------------------------------------------------------------------

for (irep in 1:ntot) {
  #Step I: Simulate from the posterior of A
  bigV.post <- kronecker(SIGMA, V.post)
  cholV.post <- t(chol(bigV.post))
  A.draw <- as.vector(A.post) + cholV.post %*% rnorm(K * M)
  A.draw <- matrix(A.draw, K, M)
  #Step II: Do SIGMA|Y from IW
  S_post <- crossprod(YY - XX %*% A.post)
  v_post <- nrow(YY)
  SIGMAinv <- matrix(rWishart(1, v_post, solve(S_post)), M, M)
  SIGMA <- solve(SIGMAinv)
  if (irep > nburn) {
    A.store[irep - nburn, , ] <- A.draw
    #Calculate one-step-ahead predictive density
    if (cons) {
      X.new <- c(Y[N, ], X[N, 1:(M * (p - 1))], 1)
    } else{
      X.new <- c(Y[N, ], X[N, 1:(M * (p - 1))])
    }
    y.store[irep - nburn, ] <-
      X.new %*% A.draw + t(t(chol(SIGMA)) %*% rnorm(M))
    
    #Calculate impulse responses
    #Step I: compute companion matrix
    if (cons) {
      B.comp <- matrix(0, K - 1, K - 1) #This is J
      B.comp[1:M, ] <- t(A.draw[1:(K - 1), ])
      B.comp[(M + 1):(K - 1), 1:(K - 1 - M)] <- diag(M * (p - 1))
    } else{
      #Construct the companion matrix
      B.comp <- matrix(0, K, K)#This is J
      B.comp[1:M, ] <- t(A.draw)
      B.comp[(M + 1):K, 1:(K - M)] <- diag(M * (p - 1))
    }
    #Step II: Compute IRFs
    #Do IRFs
    cond.OA <- 0
    counter <- 0
    while (cond.OA == 0) {
      counter <- counter + 1
      Rtilda <- matrix(rnorm(M ^ 2, 0, 1), M, M)
      qr.object <- qr(Rtilda)
      R <- qr.Q(qr.object)
      R <- R %*% diag((diag(R) > 0) - (diag(R) < 0))
      
      
      shock <- t(chol(SIGMA)) %*% R #CHG CHG CHG
      
      cond.MP <- (shock[1, 3] < 0) * (shock[2, 3] > 0) * (shock[3, 3] >
                                                            0)
      cond.AS <- (shock[1, 1] > 0) * (shock[2, 1] > 0) * (shock[3, 1] <
                                                            0)
      cond.AD <- (shock[1, 2] < 0) * (shock[2, 2] > 0) * (shock[3, 2] <
                                                            0)
      
      cond.OA <- cond.MP * cond.AS * cond.AD
    }
    
    irf.mat <- array(0, c(M * p, M * p, k))
    irf.mat[, , 1] <- shock
    for (ihorz in 2:k) {
      irf.mat[, , ihorz] <- irf.mat[, , ihorz - 1] %*% t(B.comp)
    }
    irf.mat <- irf.mat[1:M, 1:M, ]
    
    IRF.store[irep - nburn, , , ] <- irf.mat
    
  }
}

IRF.low <- apply(IRF.store, c(2, 3, 4), quantile, 0.16, na.rm = TRUE)
IRF.high <- apply(IRF.store, c(2, 3, 4), quantile, 0.84, na.rm = TRUE)
IRF.median <- apply(IRF.store, c(2, 3, 4), median, na.rm = TRUE)

#Start plotting the IRFs w.r.t the different shocks
par(mfrow = c(5, 5))

for (ii in 1:5) {
  for (jj in 1:5) {
    ts.plot(
      cbind(IRF.low[ii, jj, ], IRF.high[ii, jj, ], IRF.median[ii, jj, ]),
      ylab = colnames(Y)[[ii]],
      main = colnames(Y)[[jj]]
    )
    abline(h = 0, col = "red")
  }
}
