var <- function(x, p, const = TRUE, horizon = 20, nburn = 1000, nsave = 1000,
                minnesota = FALSE, delta = 1, theta = 0.1, gamma = 10) {
  
  Y <- as.matrix(x)
  X <- lag_data(Y, p)
  
  Y <- Y[(p + 1):nrow(Y), ]
  X <- X[(p + 1):nrow(X), ]
  
  if(const) X <- cbind(X, 1)
  
  K <- ncol(X)
  M <- ncol(Y)
  N <- nrow(Y)
  
  # OLS
  B_ols <- solve(crossprod(X)) %*% crossprod(X, Y)
  SSE <- crossprod(Y- X %*% B_ols)
  sig_ols <- sig <- SSE / (N - K)
  
  # Add prior
  if(minnesota) minnesota_prior()
  
  # Get posteriors
  V_post <- solve(crossprod(X))
  A_post <- V_post %*% crossprod(X, Y)
  S_post <- crossprod(Y - X %*% A_post)
  S_post_inv <- solve(S_post)
  v_post <- nrow(Y)
  
  # Pre-calculate 1-step predictive density
  if(const) {
    if(p == 1) {
      X_pred <- c(Y[N, ], 1)
    } else {
      X_pred <- c(Y[N, ], X[N, 1:(M * (p - 1))], 1)
    }
  } else {
    if(p == 1) {
      X_pred <- Y[N, ]
    } else {
      X_pred <- c(Y[N, ], X[N, 1:(M * (p - 1))])
    }
  }
  
  # Loop
  A_store <- array(0, c(nsave, K, M))
  Y_store <- matrix(0, nsave, M)
  IRF_store <- array(NA, c(nsave, M, M, horizon))
  
  for(i in 1:(nburn + nsave)) {
    # Posterior of A
    V_sim <- kronecker(sig, V_post)
    V_chol <- t(chol(V_sim))
    A_draw <- as.vector(A_post) + V_chol %*% rnorm(K * M)
    A_draw <- matrix(A_draw, K, M)
    # Posterior of Sigma
    sig_inv <- matrix(rWishart(1, v_post, S_post_inv), M, M)
    sig <- solve(sig_inv)
    
    if(i > nburn) {
      A_store[i - nburn, , ] <- A_draw
      
      # 1-step predicitve density
      Y_store[i - nburn, ] <- X_pred %*% A_draw + t(t(chol(sig)) %*% rnorm(M))
      
      ### Impulse responses
      
      # 1. Companion Matrix
      comp_size <- ifelse(const, K - 1, K)
      B_comp <- matrix(0, comp_size, comp_size)
      B_comp[1:M, ] <- t(A_draw[1:comp_size, ])
      if(p > 1) B_comp[(M + 1):comp_size, 1:(comp_size - M)] <- diag(M * (p - 1))
      
      # 2. Compute IR
      shock <- t(chol(sig))
      
      IRF_draw <- array(0, c(M * p, M * p, horizon))
      IRF_draw[1:M, 1:M, 1] <- shock
      for(j in 2:horizon) {
        IRF_draw[, , j] <- IRF_draw[, , j - 1] %*% t(B_comp)
      }
      IRF_draw <- IRF_draw[1:M, 1:M, ]
      IRF_store[i - nburn, , ,] <- IRF_draw
    }
  }
  
  out <- list("A" = A_store, "Y" = Y_store, "IRF" = IRF_store)
}

lag_data <- function(x, p) {
  x <- as.matrix(x)
  x_rows <- nrow(x)
  x_cols <- ncol(x)
  
  x_lag <- matrix(0, x_rows, p * x_cols)
  
  for (i in 1:p) {
    x_lag[(p + 1):x_rows, (x_cols * (i - 1) + 1):(x_cols * i)] <- 
      x[(p + 1 - i):(x_rows - i), (1:x_cols)]
  }
  return(x_lag)
}

minnesota_prior <- function(x) {
  return(0)
}