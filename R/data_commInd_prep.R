library(readr)

extract_quarter <- function(x) {
  x <- x[which(format(x$TIME, format = "%m") %in% c("03", "06", "09", "12")), ]
  x$TIME <- as.yearqtr(x$TIME, format = "%Y-Q%q")
  
  return(x)
}

stationarise_comm <- function(x) {
  x$Value <- log(x$Value)
  x[-1, 2:ncol(x)] <- apply(x[2:ncol(x)], 2, diff)
  # drop first row
  x <- x[-1, ]
  
  return(x)
}

data_indices <- read_delim("data/commodities/data_indices.csv", 
                           ";", escape_double = FALSE, 
                           col_types = cols(Date = col_date(format = "%d.%m.%Y"),
                                            Date_1 = col_date(format = "%d.%m.%Y"),
                                            Date_2 = col_date(format = "%d.%m.%Y"),
                                            Date_3 = col_date(format = "%d.%m.%Y"),
                                            Date_4 = col_date(format = "%d.%m.%Y"),
                                            Date_5 = col_date(format = "%d.%m.%Y"),
                                            Date_6 = col_date(format = "%d.%m.%Y"),
                                            Date_7 = col_date(format = "%d.%m.%Y"),
                                            Date_8 = col_date(format = "%d.%m.%Y"),
                                            Date_9 = col_date(format = "%d.%m.%Y"),
                                            Date_10 = col_date(format = "%d.%m.%Y"),
                                            Date_11 = col_date(format = "%d.%m.%Y"),
                                            Date_12 = col_date(format = "%d.%m.%Y"),
                                            Date_13 = col_date(format = "%d.%m.%Y"),
                                            Date_14 = col_date(format = "%d.%m.%Y"),
                                            Date_15 = col_date(format = "%d.%m.%Y")), 
                           trim_ws = TRUE, 
                           skip = 1)

# extract only some important indices for now
spgsci <- data_indices[(which(names(data_indices) == "SPGSCI Index") - 1):which(names(data_indices) == "SPGSCI Index")]
names(spgsci) <- c("TIME", "Value")
spgspm <- data_indices[(which(names(data_indices) == "SPGSPM Index") - 1):which(names(data_indices) == "SPGSPM Index")]
names(spgspm) <- c("TIME", "Value")
spgsin <- data_indices[(which(names(data_indices) == "SPGSIN Index") - 1):which(names(data_indices) == "SPGSIN Index")]
names(spgsin) <- c("TIME", "Value")
spgsen <- data_indices[(which(names(data_indices) == "SPGSEN Index") - 1):which(names(data_indices) == "SPGSEN Index")]
names(spgsen) <- c("TIME", "Value")

indices <- list(spgsci, spgspm, spgsin, spgsen)
names(indices) <- list("spgsci", "spgspm", "spgsin", "spgsen")
indices <- lapply(indices, extract_quarter)
indices <- lapply(indices, function(x) {
  names(x) <- c("TIME", "Value")
  x
  })
indices <- lapply(indices, stationarise_comm)

saveRDS(indices, "indices_stationary.rds")
