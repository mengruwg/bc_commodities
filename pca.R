pca = prcomp(index[253:nrow(index), 2:9], scale. = TRUE)
pca_data = ts(pca$x[, c("PC1")])
index_ts = ts(index[, 2:9])