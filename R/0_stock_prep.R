library(readr)
library(zoo)

stock_indices <- read_delim("data/datastream/stock_indices.csv", 
                            ";", escape_double = FALSE, 
                            col_types = cols(AUS_asx = col_double(), 
                                             CHL_igpa = col_double(), 
                                             DEU_dax = col_double(), 
                                             NOR_obx = col_double(), 
                                             USA_sp500 = col_double(), 
                                             ZAF_stock_index = col_double()), 
                            locale = locale(decimal_mark = ",", 
                                            grouping_mark = "."), 
                            trim_ws = TRUE)

names(stock_indices) <- c("TIME", "DEU", "USA", "NOR", "AUS", "CHL", "ZAF")

saveRDS(stock_indices, "data/raw_data/stock_indices.rds")
