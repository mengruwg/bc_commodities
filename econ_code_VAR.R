data <- readRDS("data/country_data.RDS")
data_aus <- data$AUS[c(-1, -8)]
data_aus <- data_aus[complete.cases(data_aus), ]

lag_x <- function(x, lag) {
  x <- as.matrix(x)
  x_rows <- nrow(x)
  x_cols <- ncol(x)
  x_lagged <- matrix(0, x_rows, lag * x_cols)
  for (i in 1:lag) {
    x_lagged[(lag + 1):x_rows, (x_cols * (i - 1) + 1):(x_cols * i)] <- 
      x[(lag + 1 - i):(x_rows - i), (1:x_cols)]
  }
  return(x_lagged)
}

p <- 4
const <- TRUE


Y <- as.matrix(data_aus)
X <- lag_x(Y, 4)
Y <- Y[(p + 1):nrow(Y), ]
X <- X[(p + 1):nrow(X), ]

if (const) {
  X <- cbind(1, X)
}

K <- ncol(X)
M <- ncol(Y)

B.OLS <- solve(crossprod(X)) %*% crossprod(X, Y)

SSE <- crossprod(Y - X %*% B.OLS)
N <- nrow(Y)
SIGMA <- SIGMA.OLS <- SSE / (N - K)

#Start constructing a VAR prior
#Step A: Run a set of AR(p) models
ar_sigma <- vector("numeric", M)
for (ii in 1:M) {
  ar_sigma[ii] <- sqrt(arima(Y[, ii], order = c(p, 0, 0))$sigma2)
}
#Create dummy matrices
#Specify hyperparameter on the minnesota prior and on the constant
#these will the arguments of the function
get.dum <- function(theta, gamma.prior, delta, mysigma, M, p) {
  #-----------
  ydummy <- matrix(0, 2 * M + M * (p - 1) + 1, M)
  xdummy <- matrix(0, 2 * M + M * (p - 1) + 1, M * p + 1)
  
  ydummy[1:M, ] <- diag((as.numeric(mysigma) * delta) / theta)
  ydummy[(M * (p - 1) + M + 1):(M * (p - 1) + 2 * M), ] <-
    diag(as.numeric(mysigma))
  
  jp <- diag(1:p)
  xdummy[1:(M * p), 1:(M * p)] <-
    kronecker(jp, diag(as.numeric(mysigma))) / theta
  xdummy[nrow(xdummy), ncol(xdummy)] <- gamma.prior
  return(list(Xdum = xdummy, Ydum = ydummy))
}


delta <- 1 # Mean on the first own lag
theta <- 0.1 # tightness
gamma.prior <- 10 #intercept
dummies <- get.dum(theta, gamma.prior, 1, ar_sigma, M, p)
Y_ <- dummies$Ydum
X_ <- dummies$Xdum
A.prior <-
  solve(crossprod(dummies$Xdum)) %*% crossprod(dummies$Xdum, dummies$Ydum)


#Compute posterior variance
XX <- rbind(X, X_)
YY <- rbind(Y, Y_)
V.post <- solve(crossprod(XX))
A.post <- V.post %*% crossprod(XX, YY)

nsave <- 500
nburn <- 500
ntot <- nsave + nburn

A.store <- array(0, c(nsave, K, M))
y.store <- matrix(0, nsave, M)
IRF.store <- array(NA, c(nsave, M, M, k))

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

