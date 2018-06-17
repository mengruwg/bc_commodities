olsvar <- function(y, lag) {
  y_dims <- dim(y)
  y <- t(y)
  
  Y <- y[, lag:y_dims[1]]
  
  for (i in 1:(lag-1)) {
    Y <- rbind(Y, y[, (lag - i):(y_dims[1] - i)])
  }
  
  X <- rbind(1, Y[, 1:(y_dims[1] - lag)])
  Y <- Y[, 2:(y_dims[1] - lag + 1)]
  
  A <- tcrossprod(Y, X) %*% solve(tcrossprod(X))
  SIGMA <- tcrossprod(Y - A %*% X) / (y_dims[1] - lag - lag * y_dims[2] - 1)
  V <- A[, 1]
  A <- A[, 2:(lag * y_dims[2] + 1)]
  
  output <- list("A" = A, "SIGMA" = SIGMA, "V" = V)
  
  return(output)
}

# columns: are the time - immediate to + horizon
# rows: the impact of a shock,
#   e.g. row 1 is the impact of var1 on var1, row 2 is var1 on var2, etc.
irfvar <- function(A, B_inv, lag, horizon = 12) {
  size <- nrow(B_inv)
  
  J <- cbind(diag(size), matrix(0, size, size * (lag - 1)))
  
  IRF <- matrix(NA, nrow = size^2, ncol = horizon + 1)
  rownames(IRF) <- paste0(sapply(colnames(A), rep, size), 
                         "_on_",
                         rep(colnames(A), size))[1:size^2]
  colnames(IRF) <- 0:horizon
  
  IRF[, 1] <- matrix(B_inv, ncol = 1)
  for (i in 0:horizon) {
    IRF[, i + 1] <- matrix(J %*% (A %^% i) %*% t(J) %*% B_inv, ncol = 1)
  }
  
  return(IRF)
}

plot_irf <- function(IRF) {
  var_count <- sqrt(nrow(IRF))
  df <- apply(IRF, 1, cumsum)
  df <- data.frame(id = 0:(nrow(df) - 1), df)
  
  row_id <- seq(2, ncol(df), var_count)
  plots <- vector("list", var_count)
  
  j <- 1
  for (i in row_id) {
    df_temp <- df[, c(1, i:(i + var_count - 1))]
    df_temp <- melt(df_temp, id = "id")
    
    plots[[j]] <- ggplot(df_temp, aes(x = id, y = value, colour = variable)) +
      geom_line() +
      geom_hline(yintercept = 0, colour = "black") +
      theme_fivethirtyeight() +
      scale_color_gdocs(name = NULL) +
      scale_x_continuous(expand = c(0, 0))
    
    j <- j + 1
  }
  return(plots)
}

lag <- 4
data <- as.matrix(data)
ols <- olsvar(data, lag)
B_inv <- t(chol(ols$SIGMA[1:ncol(data), 1:ncol(data)]))
irf <- irfvar(ols$A, B_inv, lag, horizon = 36)

plots <- plot_irf(irf)
plots[[1]]
plots[[2]]
plots[[3]]
plots[[4]]
plots[[5]]
plots[[6]]
plots[[7]]
