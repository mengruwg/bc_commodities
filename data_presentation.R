rm(list = ls())

library(readr)
library(reshape2)
library(ggplot2)
library(ggthemes)

data_ind = read_delim("data/data_indices.csv", 
                          ";", 
                          escape_double = FALSE, 
                          locale = locale(decimal_mark = ",", 
                                          grouping_mark = "."), 
                          trim_ws = TRUE, 
                          skip = 1)

# data_com = read_delim("data/data_commodities.csv", 
#                               ";", 
#                               escape_double = FALSE, 
#                               locale = locale(decimal_mark = ",", 
#                                               grouping_mark = "."), 
#                               trim_ws = TRUE, 
#                               skip = 1)
# 
# data_oil = read_delim("data/data_oilfutures.csv", 
#                              ";", 
#                              escape_double = FALSE, 
#                              locale = locale(decimal_mark = ",", 
#                                              grouping_mark = "."), 
#                              trim_ws = TRUE, 
#                              skip = 1)

bloomberg_import = function(x) {
  out = vector("list", ncol(x) / 2)

  for (i in seq(1, ncol(x), 2)) {
    out[[(i + 1) / 2]] = na.omit(x[, i:(i + 1)])

    index_name = substr(names(x[, i + 1]), 1, 
                        regexpr(" ", names(x[, i + 1]))[1] - 1)
    names(out[[(i + 1) / 2]]) = c("Date", index_name)
    
    out[[(i + 1) / 2]]$Date = as.character(out[[(i + 1) / 2]]$Date)
    out[[(i + 1) / 2]]$Date = as.Date(out[[(i + 1) / 2]]$Date, "%d%m%Y")
    
    names(out)[(i + 1) / 2] = index_name
  }
  
  return(out)
}

data_ind = bloomberg_import(data_ind)
data_ind = Reduce(function(...) merge(..., all = TRUE), data_ind)

data_ind_re = data.frame(mapply(`/`, data_ind[, 2:ncol(data_ind)], 
                                as.numeric(data_ind[253, 2:ncol(data_ind)]))) * 100
data_ind_re = data.frame(data_ind$Date, data_ind_re, data_ind[, 6:9])
names(data_ind_re)[1] = "Date"

# data_com = bloomberg_import(data_com)
# data_com = Reduce(function(...) merge(..., all = TRUE), data_com)
# 
# data_oil = bloomberg_import(data_oil)
# data_oil = Reduce(function(...) merge(..., all = TRUE), data_oil)

tail(data_ind_re)

df = melt(data_ind_re[, which(names(data_ind) %in% 
                             c("Date", "SPGSCI", "BCOM", "SPGSIN", "SPGSCL"))], 
          id = "Date")

ggplot(df, aes(x = Date, y = value, colour = variable)) +
  geom_line() +
  ggtitle("Commodity Indices") +
  scale_y_continuous(breaks = c(0, 100, 200, 400, 600)) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  scale_colour_manual(values = c(rgb(1, 0, 0), rgb(0, 0, 1), rgb(0.5, 0.8, 0.5), rgb(0.8, 0.5, 0.8))) +
  theme_fivethirtyeight()# +
  scale_colour_gdocs(name = NULL)

