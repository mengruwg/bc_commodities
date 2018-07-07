library(ggplot2)
library(reshape2)
library(BMS)

plot_irf <- function(x, impulse, var_names) {
  quantiles <- apply(x, c(2, 3, 4), function(z) quantile(z, c(0.16, 0.25, 0.5, 0.75, 0.84)))
  quantiles <- quantiles[, , impulse, ]
  
  dfs <- vector("list", dim(quantiles)[2])
  for(i in 1:dim(quantiles)[2]) {
    dfs[[i]] <- data.frame("id" = 1:dim(quantiles)[3], 
                           "resp" = rep(i, dim(quantiles)[3]), 
                           "016" = c(t(quantiles[1, i, ])), 
                           "025" = c(t(quantiles[2, i, ])), 
                           "050" = c(t(quantiles[3, i, ])), 
                           "075" = c(t(quantiles[4, i, ])), 
                           "084" = c(t(quantiles[5, i, ])))
  }
  
  df <- do.call("rbind", dfs)
  df$resp <- var_names[df$resp]
  df$resp <- factor(df$resp, levels = var_names)
  
  ggplot(df, aes(x = id, y = X050)) +
    geom_ribbon(aes(ymin = X025, ymax = X050), fill = "grey30") +
    geom_ribbon(aes(ymin = X016, ymax = X025), fill = "grey60") +
    geom_ribbon(aes(ymin = X050, ymax = X075), fill = "grey30") +
    geom_ribbon(aes(ymin = X075, ymax = X084), fill = "grey60") +
    facet_grid(resp ~ ., scales = "free") +
    geom_line(col = "black", size = 1) +
    scale_x_continuous(expand = c(0, 0), breaks = seq(2, dim(quantiles)[3], dim(quantiles)[3] / 10)) +
    geom_hline(yintercept = 0) +
    ggtitle(paste(var_names[impulse], "impulse")) +
    theme(
      axis.title = element_blank(), 
      panel.background = element_rect(fill = NA),
      panel.grid.minor = element_line(colour = "lightgray", size = 0.5))
}

pip_heatmap = function(x,
                       colours,
                       palette = TRUE,
                       font_face = "plain",
                       font_colour = "black",
                       lim = NULL){
  
  if(missing(colours)) {
    colours = c(rgb(  0, 102, 156, maxColorValue = 255),
                rgb(255, 255, 255, maxColorValue = 255),
                rgb(165,   0,  33, maxColorValue = 255))
  }
  
  palette = colorRampPalette(colours)
  col_vector = palette(256)
  
  lim <- c(0, 1)
  
  x_lagged <- lag_data(x, 1)
  pips <- matrix(NA, ncol = ncol(x), nrow = ncol(x))
  for(col in 1:ncol(x)) {
    pip <- bms(cbind(x[-1, col], x_lagged[-1, ]), user.int = FALSE)
    pips[, col] <- pip$info$inccount / pip$info$cumsumweights
  }
  
  df = melt(pips)
  colnames(df) = c("X1", "X2", "value")
  
  label = ifelse(is.na(df$value), NA, sprintf("%.1f%%", df$value * 100))
  
  df$X1 <- factor(df$X1, levels = unique(df$X1))
  df$X2 <- factor(df$X2, levels = unique(df$X2))
  
  ggplot(df, aes(x = X1, y = X2) ) +
    geom_tile(aes(fill = value, alpha = 0.5),
              size = 1, width = 0.975, height = 0.975) +
    geom_text(aes(label = label), colour = rgb(64, 64, 64, maxColorValue = 255)) +
    scale_fill_gradientn(colours = col_vector, na.value = "white") +
    scale_y_discrete(expand = c(0, 0), 
                       breaks = 1:ncol(x), labels = names(x)) +
    scale_x_discrete(position = "top", expand = c(0, 0), 
                       breaks = 1:ncol(x), labels = names(x)) +
    xlab("Independents") +
    ylab("Dependents") +
    ggtitle("Posterior Inclusion Probabilities of the first lag") +
    coord_equal() +
    theme(
      text = element_text(colour = "gray"),
      panel.border = element_rect(colour = "black", fill = NA),
      plot.margin = unit(c(0.5, 1, 0, 0), "lines"),
      panel.background = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none",
      axis.ticks = element_blank(),
      axis.text.x = element_text(hjust = 0))
}

