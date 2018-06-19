require(ggplot2)
require(ggthemes)
require(reshape2)

mlag <- function(X, lag)
{
  p <- lag
  X <- as.matrix(X)
  Traw <- nrow(X)
  N <- ncol(X)
  Xlag <- matrix(0, Traw, p * N)
  for (ii in 1:p) {
    Xlag[(p + 1):Traw, (N * (ii - 1) + 1):(N * ii)] = X[(p + 1 - ii):(Traw -
                                                                        ii), (1:N)]
  }
  return(Xlag)
}

sign_restr <- FALSE
p <- 4
cons <- TRUE
nhor <- 20


Y <- as.matrix(data)
X <- mlag(Y, p)
Y <- Y[(p + 1):nrow(Y), ]
X <- X[(p + 1):nrow(X), ]
if (cons) {
  X <- cbind(X, 1)
}

K <- ncol(X)
M <- ncol(Y)

B.OLS <- solve(crossprod(X)) %*% crossprod(X, Y)
yfit <- X %*% B.OLS

SSE <- crossprod(Y - X %*% B.OLS)
T <- nrow(Y)
SIGMA <- SIGMA.OLS <- SSE / (T - K)

#Start constructing a VAR prior
#Step A: Run a set of AR(p) models
mysigma <- matrix(NA, M, 1)
for (ii in 1:M) {
  tmpar <- arima(Y[, ii], order = c(p, 0, 0))
  mysigma[ii, 1] <- sqrt(tmpar$sigma2)
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
dummies <- get.dum(theta, gamma.prior, 1, mysigma, M, p)
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
IRF.store <- array(NA, c(nsave, M, M, nhor))

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
      X.new <- c(Y[T, ], X[T, 1:(M * (p - 1))], 1)
    } else{
      X.new <- c(Y[T, ], X[T, 1:(M * (p - 1))])
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
    if (sign_restr) {
      cond.OA <- 0
      counter <- 0
      while (cond.OA == 0) {
        counter <- counter + 1
        Rtilda <- matrix(rnorm(M ^ 2, 0, 1), M, M)
        qr.object <- qr(Rtilda)
        R <- qr.Q(qr.object)
        R <- R %*% diag((diag(R) > 0) - (diag(R) < 0))
        
        
        shock <- t(chol(SIGMA)) %*% R #CHG CHG CHG
        
        cond.MP <- (shock[1, 3] < 0) * (shock[2, 3] > 0) * (shock[3, 3] > 0)
        cond.AS <- (shock[1, 1] > 0) * (shock[2, 1] > 0) * (shock[3, 1] < 0)
        cond.AD <- (shock[1, 2] < 0) * (shock[2, 2] > 0) * (shock[3, 2] < 0)
        
        cond.OA <- cond.MP * cond.AS * cond.AD
        }
      } else { # or not
      shock <- t(chol(SIGMA))
    }
    
    irf.mat <- array(0, c(M * p, M * p, nhor))
    irf.mat[, , 1] <- shock
    for (ihorz in 2:nhor) {
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


### custom plot

plot_irf <- function(irf, impulse = 1, var_names) {
  irf_lo <- apply(irf, c(2, 3, 4), quantile, 0.15, na.rm = TRUE)
  irf_hi <- apply(irf, c(2, 3, 4), quantile, 0.85, na.rm = TRUE)
  irf_md <- apply(irf, c(2, 3, 4), median, na.rm = TRUE)
  
  df <- data.frame("id" = 1:dim(irf_md)[3], 
                   "md" = t(irf_md[, impulse, ]), 
                   "hi" = t(irf_hi[, impulse, ]), 
                   "lo" = t(irf_lo[, impulse, ]))

  x <- melt(df, id.vars = "id")
  x$facet <- var_names[as.integer(substr(x$variable, 
                                         nchar(as.character(x$variable)), 
                                         nchar(as.character(x$variable))))]
  plot_col <- c(rep("darkblue", nrow(irf_md)), rep("darkred", nrow(irf_md)), rep("darkred", nrow(irf_md)))
  
  ggplot(x, aes(x = id, y = value, colour = variable)) +
    geom_line() +
    scale_color_manual(values = plot_col) +
    theme_wsj() +
    theme(legend.position = "none") +
    facet_grid(facet ~ ., scales = "free_y") +
    ggtitle(paste(var_names[impulse], "shock")) +
    scale_x_continuous(expand = c(0, 0), breaks = seq(0, dim(irf_md)[3], 2))
}

# plot_irf(IRF.store, 1, names(data))
# plot_irf(IRF.store, 2, names(data))
# plot_irf(IRF.store, 3, names(data))
# plot_irf(IRF.store, 4, names(data))
# plot_irf(IRF.store, 5, names(data))
# plot_irf(IRF.store, 6, names(data))
# plot_irf(IRF.store, 7, names(data))

